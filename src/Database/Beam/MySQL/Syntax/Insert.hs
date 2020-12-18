{-# LANGUAGE TypeFamilies #-}

module Database.Beam.MySQL.Syntax.Insert where

import           Data.Text (Text)
import           Data.Vector (Vector, fromList)
import           Database.Beam.Backend.SQL (IsSql92InsertSyntax (..),
                                            IsSql92InsertValuesSyntax (..))
import           Database.Beam.MySQL.Syntax.Select (MySQLExpressionSyntax,
                                                    MySQLSelect,
                                                    MySQLTableNameSyntax,
                                                    TableRowExpression (TableRowExpression))

data MySQLInsertValuesSyntax =
  InsertSQLExpressions {-# UNPACK #-} !(Vector TableRowExpression) |
  InsertFromSQL MySQLSelect
  deriving stock (Eq, Show)

instance IsSql92InsertValuesSyntax MySQLInsertValuesSyntax where
  type Sql92InsertValuesExpressionSyntax MySQLInsertValuesSyntax =
    MySQLExpressionSyntax
  type Sql92InsertValuesSelectSyntax MySQLInsertValuesSyntax =
    MySQLSelect
  {-# INLINABLE insertSqlExpressions #-}
  insertSqlExpressions :: [[MySQLExpressionSyntax]] -> MySQLInsertValuesSyntax
  insertSqlExpressions =
    InsertSQLExpressions . fromList . fmap (TableRowExpression . fromList)
  {-# INLINABLE insertFromSql #-}
  insertFromSql :: MySQLSelect -> MySQLInsertValuesSyntax
  insertFromSql = InsertFromSQL

data MySQLInsert = InsertStmt {
  tableName    :: {-# UNPACK #-} !MySQLTableNameSyntax,
  columns      :: {-# UNPACK #-} !(Vector Text),
  insertValues :: MySQLInsertValuesSyntax
  }
  deriving stock (Eq, Show)

instance IsSql92InsertSyntax MySQLInsert where
  type Sql92InsertValuesSyntax MySQLInsert =
    MySQLInsertValuesSyntax
  type Sql92InsertTableNameSyntax MySQLInsert =
    MySQLTableNameSyntax
  {-# INLINABLE insertStmt #-}
  insertStmt ::
    MySQLTableNameSyntax ->
    [Text] ->
    MySQLInsertValuesSyntax ->
    MySQLInsert
  insertStmt tableName' columns' =
    InsertStmt tableName'
               (fromList columns')
