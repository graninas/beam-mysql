{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Beam.MySQL.Extra where

import           Control.Exception.Safe (bracket, throw)
import           Control.Monad (when, (>=>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import           Data.Kind (Type)
import           Data.Text (Text, pack)
import           Data.Text.Lazy (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Vector (Vector, find, foldl1', head, length, mapMaybe,
                              null, unfoldrM, zip, (!?))
import           Database.Beam.Backend.SQL.Row (FromBackendRow)
import           Database.Beam.MySQL.Connection (MySQL, MySQLM (MySQLM),
                                                 MySQLMEnv (..),
                                                 MySQLStatementError (..),
                                                 acquireStream, drainStream)
import           Database.Beam.MySQL.Syntax.Insert (MySQLInsert,
                                                    MySQLInsertValuesSyntax (..))
import           Database.Beam.MySQL.Syntax.Misc (MySQLFieldNameSyntax (..))
import           Database.Beam.MySQL.Syntax.Render (RenderError (..),
                                                    RenderErrorType (..),
                                                    renderDelete, renderInsert,
                                                    renderSelect, renderUpdate)
import           Database.Beam.MySQL.Syntax.Select (BinOp (LAnd), CompOp (CEq),
                                                    MySQLExpressionSyntax (..),
                                                    MySQLFromSyntax (..),
                                                    MySQLProjectionSyntax (..),
                                                    MySQLSelect (SelectStmt),
                                                    MySQLSelectTableSyntax (..),
                                                    MySQLTableNameSyntax,
                                                    MySQLTableSourceSyntax (TableNamed),
                                                    Projection (..),
                                                    TableHeader (Anonymous),
                                                    TableRowExpression (..))
import           Database.Beam.Query (SqlDelete (..), SqlInsert (..),
                                      SqlSelect (..), SqlUpdate (..),
                                      runSelectReturningOne)
import           Database.MySQL.Base (FieldType, MySQLConn,
                                      MySQLValue (MySQLText), Query (Query),
                                      execute_, okAffectedRows)
import           Prelude hiding (head, length, null, read, zip)
import           System.IO.Streams (InputStream, read)

dumpInsertSQL :: forall (table :: (Type -> Type) -> Type) .
  SqlInsert MySQL table -> Maybe Text
dumpInsertSQL = \case
  SqlInsertNoRows -> Nothing
  SqlInsert _ ins -> case renderInsert ins of
    Left _                 -> Nothing
    Right (Query query, _) -> Just . toStrict . decodeUtf8 $ query

dumpSelectSQL :: forall (a :: Type) .
  SqlSelect MySQL a -> Maybe Text
dumpSelectSQL (SqlSelect sel) = case renderSelect sel of
  Left _                 -> Nothing
  Right (Query query, _) -> Just . toStrict . decodeUtf8 $ query

dumpUpdateSQL :: forall (table :: (Type -> Type) -> Type) .
  SqlUpdate MySQL table -> Maybe Text
dumpUpdateSQL = \case
  SqlIdentityUpdate -> Nothing
  SqlUpdate _ upd -> case renderUpdate upd of
    Left _                 -> Nothing
    Right (Query query, _) -> Just . toStrict . decodeUtf8 $ query

dumpDeleteSQL :: forall (table :: (Type -> Type) -> Type) .
  SqlDelete MySQL table -> Maybe Text
dumpDeleteSQL (SqlDelete _ del) = case renderDelete del of
  Left _                 -> Nothing
  Right (Query query, _) -> Just . toStrict . decodeUtf8 $ query

runInsertRowReturning :: forall (table :: (Type -> Type) -> Type) .
  (FromBackendRow MySQL (table Identity)) =>
  SqlInsert MySQL table -> MySQLM (Maybe (table Identity))
runInsertRowReturning stmt = case stmt of
  SqlInsertNoRows -> pure Nothing
  SqlInsert _ ins -> case renderInsert ins of
    Left (RenderError typ ast) -> case typ of
      UnsupportedOperation op -> unsupported op ast ins
    Right (query, _) -> case ins.insertValues of
      InsertFromSQL sel -> insertIntoSelect sel ins
      InsertSQLExpressions rows -> case length rows of
        0 -> pure Nothing -- should be impossible
        1 -> insertRowReturning ins (head rows) query
        _ -> multipleRowInserts rows ins

-- Helpers

-- Error reporting

unsupported :: Text -> Text -> MySQLInsert -> MySQLM a
unsupported op ast = throw . OperationNotSupported op ast . pack . show

insertIntoSelect :: MySQLSelect -> MySQLInsert -> MySQLM a
insertIntoSelect sel =
  throw .
    OperationNotSupported "Insert-into-select with returning" (pack . show $ sel) .
    pack .
    show

multipleRowInserts :: Vector TableRowExpression -> MySQLInsert -> MySQLM a
multipleRowInserts rows =
  throw .
    OperationNotSupported "Multiple row inserts with returning" (pack . show $ rows) .
    pack .
    show

impureExpr :: MySQLExpressionSyntax -> MySQLInsert -> MySQLM a
impureExpr e = throw . ImpureExpression (pack . show $ e) . pack . show

noPrimaryKey :: MySQLInsert -> MySQLM ()
noPrimaryKey ins =
  throw .
  OperationNotSupported "Insert row returning without primary key" ins.tableName.name .
  pack . show $ ins

-- Core logic

insertRowReturning :: forall (table :: (Type -> Type) -> Type) .
  (FromBackendRow MySQL (table Identity)) =>
  MySQLInsert ->
  TableRowExpression ->
  Query ->
  MySQLM (Maybe (table Identity))
insertRowReturning ins (TableRowExpression v) query = do
  -- Ensure that all of our inserted values come from pure expressions
  traverse_ (analyzeExpr ins) v
  -- Collect a vector of column names and corresponding values. This _has_ to
  -- retain the order exactly as specified by the insert, or we risk breaking
  -- subsequent queries.
  let fieldVals = zip ins.columns v
  conn <- getConnection
  -- Get the names of all primary key columns in the table.
  pkColNames <- getPkCols conn ins.tableName.name
  -- If we don't find anything, abort.
  when (null pkColNames) (noPrimaryKey ins)
  -- Determine if we have an auto-increment column, and if so, what it is
  mAIColumn <- getAutoIncColumn conn ins.tableName.name
  -- Run the insert
  res <- liftIO . execute_ conn $ query
  case okAffectedRows res of
    -- This means our insert missed for some reason, so we have nothing to give
    -- back.
    0 -> pure Nothing
    -- The insert hit, so we need to find out what changed.
    _ -> do
      -- Build a SELECT on the basis of all our gleaned information
      let sel =
            SqlSelect .
              buildPostSelect ins.tableName fieldVals pkColNames $ mAIColumn
      -- Execute to get our changes
      runSelectReturningOne sel

-- Marks the expression as pure (meaning, repeatable any number of times) and
-- impure (meaning side effecting).
data Purity = Pure | Impure
  deriving stock (Eq, Show)

instance Semigroup Purity where
  Impure <> _ = Impure
  Pure <> x   = x

instance Monoid Purity where
  mempty = Pure

analyzeExpr :: MySQLInsert -> MySQLExpressionSyntax -> MySQLM ()
analyzeExpr ins e = case go e of
  Pure   -> pure ()
  Impure -> impureExpr e ins
  where
    go :: MySQLExpressionSyntax -> Purity
    go e' = case e' of
      -- Literals are always pure
      Value _             -> Pure
      -- A row is pure if all its components are
      Row rowEs           -> foldMap go rowEs
      -- A coalesce operation is pure if all its components are
      Coalesce coalesceEs -> foldMap go coalesceEs
      -- A case expression is pure if all its components are. Realistically,
      -- that makes it a constant, since we either always meet one condition, or
      -- always meet none.
      Case{}              ->
        foldMap (\cb -> go cb.condition <> go cb.action) e'.cases <> go e'.defaultCase
      -- A field reference is always impure.
      Field{} -> Impure
      -- A binary operation is pure if its operands are.
      BinaryOperation{} -> go e'.lOperand <> go e'.rOperand
      -- A comparison operation is pure if its operands are.
      ComparisonOperation{} -> go e'.lOperand <> go e'.rOperand
      -- A prefix operation is pure if its operand is.
      PrefixOperation{} -> go e'.operand
      -- A postfix operation is pure if its operand is.
      PostfixOperation{} -> go e'.operand
      -- A NULLIF is pure if both its branches are.
      NullIf{} -> go e'.expr <> go e'.ifNull
      -- A POSITION is pure if the needle and haystack are both pure.
      Position{} -> go e'.needle <> go e'.haystack
      -- A CAST is pure if the expression being cast is.
      Cast{} -> go e'.expr
      -- Field extractions from pure expressions are pure.
      Extract{} -> go e'.expr
      -- CURRENT_TIMESTAMP is pure within a transaction, which we assume we're
      -- in, as runInsertRowReturning is unsafe outside one.
      CurrentTimestamp -> Pure
      -- DEFAULT cannot be assumed pure in general after MySQL 8.0.13. Given
      -- that we're not on this version, we're safe.
      Default -> Pure
      -- IN expressions are pure if all their components are.
      In{} -> foldMap go e'.exprs <> go e'.expr
      -- BETWEEN is pure if all its components are.
      Between{} -> go e'.expr <> go e'.lo <> go e'.hi
      -- Anything involving sub-SELECTs is assumed impure. This is a bit crude,
      -- but realistic analysis of these would be too hard for our goals.
      Exists{} -> Impure
      Unique{} -> Impure
      Subquery{} -> Impure
      -- Any aggregation is pure, provided the expression being aggregated on
      -- is.
      CountAll -> Pure
      Aggregation{} -> go e'.expr
      -- CONCAT is pure if all expressions being concatenated are.
      Concat concatEs -> foldMap go concatEs
      -- This should never come up, since beam never constructs this
      LastInsertId -> Impure

getConnection :: MySQLM MySQLConn
getConnection = MySQLM (asks go)
  where
    go :: MySQLMEnv -> MySQLConn
    go = \case
      DebugEnv _ c -> c
      ReleaseEnv c -> c

getPkCols :: MySQLConn -> Text -> MySQLM (Vector Text)
getPkCols conn nam = do
  let query = Query $
        "SELECT key_column_usage.column_name " <>
        "FROM information_schema.key_column_usage " <>
        "WHERE table_schema = schema() " <>
        "AND constraint_name = 'PRIMARY' " <>
        "AND table_name = '" <>
        (encodeUtf8 . fromStrict $ nam) <>
        "';"
  bracket (acquireStream conn query)
          drainStream
          (liftIO . unfoldrM go)
  where
    go ::
      InputStream (Vector (FieldType, MySQLValue)) ->
      IO (Maybe (Text, InputStream (Vector (FieldType, MySQLValue))))
    go stream = do
      res <- read stream
      pure $ (, stream) <$> (res >>= (!? 0) >>= extractText)

getAutoIncColumn :: MySQLConn -> Text -> MySQLM (Maybe Text)
getAutoIncColumn conn nam = do
  let query = Query $
        "SELECT column_name " <>
        "FROM information_schema.columns " <>
        "WHERE table_schema = schema() " <>
        "AND table_name = '" <>
        (encodeUtf8 . fromStrict $ nam) <>
        "' AND extra LIKE 'auto_increment' " <>
        "LIMIT 1;"
  bracket (acquireStream conn query)
          drainStream
          (liftIO . fmap ((>>= (!? 0)) >=> extractText) . read)

extractText :: (FieldType, MySQLValue) -> Maybe Text
extractText (_, val) = case val of
  MySQLText t -> pure t
  _           -> Nothing

buildPostSelect ::
  MySQLTableNameSyntax ->
  Vector (Text, MySQLExpressionSyntax) ->
  Vector Text ->
  Maybe Text ->
  MySQLSelect
buildPostSelect nam fieldVals pkCols mAICol =
  -- Order doesn't matter, LIMIT 1, no offset
  SelectStmt selTable mempty (Just 1) Nothing
  where
    selTable :: MySQLSelectTableSyntax
    selTable =
      SelectTableStatement
        Nothing -- no quantifier
        fieldsOf
        (Just . FromTable ourName $ Anonymous)
        (Just onPKVals)
        Nothing -- no GROUP BY
        Nothing -- no HAVING
    fieldsOf :: MySQLProjectionSyntax
    fieldsOf = ProjectExpressions . fmap (projectField . fst) $ fieldVals
    projectField :: Text -> Projection
    projectField t = Projection (Field . UnqualifiedField $ t) Nothing
    ourName :: MySQLTableSourceSyntax
    ourName = TableNamed nam
    onPKVals :: MySQLExpressionSyntax
    onPKVals =
      foldl1' (BinaryOperation LAnd) .
        mapMaybe (uncurry fieldEqExpr) $ fieldVals
    fieldEqExpr :: Text -> MySQLExpressionSyntax -> Maybe MySQLExpressionSyntax
    fieldEqExpr f e = do
      -- if it's not a primary key column, we ignore it
      _ <- find (f ==) pkCols
      rOp <- case (f ==) <$> mAICol of
              -- if the autoincrementing column is in our primary key, replace
              -- its DEFAULT with last_insert_id()
              Just True -> pure LastInsertId
              -- if we don't have an autoincrementing column in our primary key,
              -- or this isn't it, just paste the value as-was
              _         -> pure e
      let lOp = Field . UnqualifiedField $ f
      pure . ComparisonOperation CEq Nothing lOp $ rOp
