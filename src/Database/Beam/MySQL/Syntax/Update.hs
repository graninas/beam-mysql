{-# LANGUAGE TypeFamilies #-}

module Database.Beam.MySQL.Syntax.Update where

import           Data.Vector (Vector, fromList)
import           Database.Beam.Backend.SQL (IsSql92UpdateSyntax (..))
import           Database.Beam.MySQL.Syntax.Misc (MySQLFieldNameSyntax)
import           Database.Beam.MySQL.Syntax.Select (MySQLExpressionSyntax,
                                                    MySQLTableNameSyntax)

data FieldUpdate = FieldUpdate {
  fieldName       :: !MySQLFieldNameSyntax,
  fieldExpression :: !MySQLExpressionSyntax
  }
  deriving stock (Eq, Show)

data MySQLUpdate = UpdateStmt {
  tableName :: {-# UNPACK #-} !MySQLTableNameSyntax,
  updates   :: {-# UNPACK #-} !(Vector FieldUpdate),
  wher      :: !(Maybe MySQLExpressionSyntax)
  }
  deriving stock (Eq, Show)

instance IsSql92UpdateSyntax MySQLUpdate where
  type Sql92UpdateExpressionSyntax MySQLUpdate =
    MySQLExpressionSyntax
  type Sql92UpdateFieldNameSyntax MySQLUpdate =
    MySQLFieldNameSyntax
  type Sql92UpdateTableNameSyntax MySQLUpdate =
    MySQLTableNameSyntax
  {-# INLINABLE updateStmt #-}
  updateStmt ::
    MySQLTableNameSyntax ->
    [(MySQLFieldNameSyntax, MySQLExpressionSyntax)] ->
    Maybe MySQLExpressionSyntax ->
    MySQLUpdate
  updateStmt tableName' updates' =
    UpdateStmt tableName'
               (fromList . fmap (uncurry FieldUpdate) $ updates')
