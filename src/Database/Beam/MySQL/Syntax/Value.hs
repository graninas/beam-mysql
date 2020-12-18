module Database.Beam.MySQL.Syntax.Value where

import           Data.Aeson (ToJSON (toJSON), Value)
import           Data.ByteString (ByteString)
import           Data.FakeUTC (FakeUTC (FakeUTC))
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Scientific (Scientific)
import           Data.Text (Text, pack)
import           Data.Time (Day, LocalTime, TimeOfDay, utc, utcToLocalTime)
import           Data.ViaJson (ViaJson (ViaJson))
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend.SQL (HasSqlValueSyntax (sqlValueSyntax),
                                            SqlNull)

data MySQLValueSyntax =
  VBool !Bool |
  VInt8 {-# UNPACK #-} !Int8 |
  VInt16 {-# UNPACK #-} !Int16 |
  VInt32 {-# UNPACK #-} !Int32 |
  VInt64 {-# UNPACK #-} !Int64 |
  VWord8 {-# UNPACK #-} !Word8 |
  VWord16 {-# UNPACK #-} !Word16 |
  VWord32 {-# UNPACK #-} !Word32 |
  VWord64 {-# UNPACK #-} !Word64 |
  VScientific {-# UNPACK #-} !Scientific |
  VFloat {-# UNPACK #-} !Float |
  VDouble {-# UNPACK #-} !Double |
  VNothing | -- Missing value
  VNull | -- SQL NULL
  VByteString {-# UNPACK #-} !ByteString |
  VText {-# UNPACK #-} !Text |
  VDay !Day |
  VLocalTime {-# UNPACK #-} !LocalTime |
  VTimeOfDay {-# UNPACK #-} !TimeOfDay |
  -- Helper for munging via JSON
  VViaJSON !Value
  deriving stock (Eq, Show)

instance HasSqlValueSyntax MySQLValueSyntax Bool where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Bool -> MySQLValueSyntax
  sqlValueSyntax = VBool

instance HasSqlValueSyntax MySQLValueSyntax Int8 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Int8 -> MySQLValueSyntax
  sqlValueSyntax = VInt8

instance HasSqlValueSyntax MySQLValueSyntax Int16 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Int16 -> MySQLValueSyntax
  sqlValueSyntax = VInt16

instance HasSqlValueSyntax MySQLValueSyntax Int32 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Int32 -> MySQLValueSyntax
  sqlValueSyntax = VInt32

instance HasSqlValueSyntax MySQLValueSyntax Int64 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Int64 -> MySQLValueSyntax
  sqlValueSyntax = VInt64

instance HasSqlValueSyntax MySQLValueSyntax Int where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Int -> MySQLValueSyntax
  sqlValueSyntax = VInt64 . fromIntegral

instance HasSqlValueSyntax MySQLValueSyntax Word8 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Word8 -> MySQLValueSyntax
  sqlValueSyntax = VWord8

instance HasSqlValueSyntax MySQLValueSyntax Word16 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Word16 -> MySQLValueSyntax
  sqlValueSyntax = VWord16

instance HasSqlValueSyntax MySQLValueSyntax Word32 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Word32 -> MySQLValueSyntax
  sqlValueSyntax = VWord32

instance HasSqlValueSyntax MySQLValueSyntax Word64 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Word64 -> MySQLValueSyntax
  sqlValueSyntax = VWord64

instance HasSqlValueSyntax MySQLValueSyntax Word where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Word -> MySQLValueSyntax
  sqlValueSyntax = VWord64 . fromIntegral

instance HasSqlValueSyntax MySQLValueSyntax Scientific where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Scientific -> MySQLValueSyntax
  sqlValueSyntax = VScientific

instance HasSqlValueSyntax MySQLValueSyntax Float where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Float -> MySQLValueSyntax
  sqlValueSyntax = VFloat

instance HasSqlValueSyntax MySQLValueSyntax Double where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Double -> MySQLValueSyntax
  sqlValueSyntax = VDouble

instance (HasSqlValueSyntax MySQLValueSyntax a) =>
  HasSqlValueSyntax MySQLValueSyntax (Maybe a) where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Maybe a -> MySQLValueSyntax
  sqlValueSyntax = maybe VNothing sqlValueSyntax

instance HasSqlValueSyntax MySQLValueSyntax SqlNull where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: SqlNull -> MySQLValueSyntax
  sqlValueSyntax = const VNull

instance HasSqlValueSyntax MySQLValueSyntax ByteString where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: ByteString -> MySQLValueSyntax
  sqlValueSyntax = VByteString

instance HasSqlValueSyntax MySQLValueSyntax Text where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Text -> MySQLValueSyntax
  sqlValueSyntax = VText

instance HasSqlValueSyntax MySQLValueSyntax String where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: String -> MySQLValueSyntax
  sqlValueSyntax = VText . pack

instance HasSqlValueSyntax MySQLValueSyntax Day where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Day -> MySQLValueSyntax
  sqlValueSyntax = VDay

instance HasSqlValueSyntax MySQLValueSyntax TimeOfDay where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: TimeOfDay -> MySQLValueSyntax
  sqlValueSyntax = VTimeOfDay

instance HasSqlValueSyntax MySQLValueSyntax LocalTime where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: LocalTime -> MySQLValueSyntax
  sqlValueSyntax = VLocalTime

instance (ToJSON a) => HasSqlValueSyntax MySQLValueSyntax (ViaJson a) where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: ViaJson a -> MySQLValueSyntax
  sqlValueSyntax (ViaJson v) = VViaJSON . toJSON $ v

instance HasSqlValueSyntax MySQLValueSyntax FakeUTC where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: FakeUTC -> MySQLValueSyntax
  sqlValueSyntax (FakeUTC t) = VLocalTime . utcToLocalTime utc $ t
