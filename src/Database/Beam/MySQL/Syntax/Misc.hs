-- Due to RDP plugin
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.MySQL.Syntax.Misc where

import           Data.Text (Text)
import           Database.Beam.Backend.SQL (IsSql92AggregationSetQuantifierSyntax (..),
                                            IsSql92ExtractFieldSyntax (..),
                                            IsSql92FieldNameSyntax (..),
                                            IsSql92QuantifierSyntax (..))

data MySQLFieldNameSyntax =
  QualifiedField {
    table :: {-# UNPACK #-} !Text,
    field :: {-# UNPACK #-} !Text
    } |
  UnqualifiedField {
    field :: {-# UNPACK #-} !Text
    }
  deriving stock (Eq, Show)

instance IsSql92FieldNameSyntax MySQLFieldNameSyntax where
  {-# INLINABLE qualifiedField #-}
  qualifiedField :: Text -> Text -> MySQLFieldNameSyntax
  qualifiedField = QualifiedField
  {-# INLINABLE unqualifiedField #-}
  unqualifiedField :: Text -> MySQLFieldNameSyntax
  unqualifiedField = UnqualifiedField

data MySQLQuantifierSyntax = All | Any
 deriving stock (Eq, Show)

instance IsSql92QuantifierSyntax MySQLQuantifierSyntax where
  {-# INLINABLE quantifyOverAll #-}
  quantifyOverAll :: MySQLQuantifierSyntax
  quantifyOverAll = All
  {-# INLINABLE quantifyOverAny #-}
  quantifyOverAny :: MySQLQuantifierSyntax
  quantifyOverAny = Any

data MySQLExtractFieldSyntax =
  SecondsField |
  MinutesField |
  HourField |
  DayField |
  MonthField |
  YearField
  deriving stock (Eq, Show)

instance IsSql92ExtractFieldSyntax MySQLExtractFieldSyntax where
  {-# INLINABLE secondsField #-}
  secondsField :: MySQLExtractFieldSyntax
  secondsField = SecondsField
  {-# INLINABLE minutesField #-}
  minutesField :: MySQLExtractFieldSyntax
  minutesField = MinutesField
  {-# INLINABLE hourField #-}
  hourField :: MySQLExtractFieldSyntax
  hourField = HourField
  {-# INLINABLE dayField #-}
  dayField :: MySQLExtractFieldSyntax
  dayField = DayField
  {-# INLINABLE monthField #-}
  monthField :: MySQLExtractFieldSyntax
  monthField = MonthField
  {-# INLINABLE yearField #-}
  yearField :: MySQLExtractFieldSyntax
  yearField = YearField

data MySQLAggregationSetQuantifierSyntax =
  SetDistinct |
  SetAll
  deriving stock (Eq, Show)

instance IsSql92AggregationSetQuantifierSyntax MySQLAggregationSetQuantifierSyntax where
  {-# INLINABLE setQuantifierDistinct #-}
  setQuantifierDistinct :: MySQLAggregationSetQuantifierSyntax
  setQuantifierDistinct = SetDistinct
  {-# INLINABLE setQuantifierAll #-}
  setQuantifierAll :: MySQLAggregationSetQuantifierSyntax
  setQuantifierAll = SetAll
