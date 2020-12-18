module Data.FakeUTC where

import           Data.Time (UTCTime)
import           Type.Reflection (Typeable)

newtype FakeUTC = FakeUTC UTCTime
  deriving newtype (Eq, Ord)
  deriving stock (Show, Typeable)
