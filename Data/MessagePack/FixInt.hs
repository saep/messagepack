{- |
Module      :  Data.MessagePack.FixInt
Description :  Fixed size integers
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

-}
module Data.MessagePack.FixInt
    where

import Data.Int
import Data.Word

data FixInt = NegFixInt   !Int8
            | FixInt8     !Int8
            | FixInt16    !Int16
            | FixInt32    !Int32
            | FixInt64    !Int64
            | PosFixInt   !Word8
            | PosFixInt8  !Word8
            | PosFixInt16 !Word16
            | PosFixInt32 !Word32
            | PosFixInt64 !Word64
    deriving (Eq, Ord, Show)

-- | Convert the 'FixInt' value to an integral. Make sure that the target type
-- is large enough for the numbers you are converting to.
integralValue :: (Integral i) => FixInt -> i
integralValue fi = case fi of
    NegFixInt   i -> fromIntegral i
    FixInt8     i -> fromIntegral i
    FixInt16    i -> fromIntegral i
    FixInt32    i -> fromIntegral i
    FixInt64    i -> fromIntegral i
    PosFixInt   i -> fromIntegral i
    PosFixInt8  i -> fromIntegral i
    PosFixInt16 i -> fromIntegral i
    PosFixInt32 i -> fromIntegral i
    PosFixInt64 i -> fromIntegral i

-- | Smart creator of fixint values. This function is smart in the case that it
-- uses the least amount of bits necessary to represent the value on the
-- encoded 'ByteString'.
fixIntValue :: (Integral i) => i -> FixInt
fixIntValue i
    | i >= 0   && i <= 127        = PosFixInt   $ fromIntegral i
    | i >= -32 && i <= -1         = NegFixInt   $ fromIntegral i
    | i >= 0   && i < 0x100       = PosFixInt8  $ fromIntegral i
    | i >= 0   && i < 0x10000     = PosFixInt16 $ fromIntegral i
    | i >= 0   && i < 0x100000000 = PosFixInt32 $ fromIntegral i
    | i >= 0                      = PosFixInt64 $ fromIntegral i
    | i >= -0x80                  = FixInt8     $ fromIntegral i
    | i >= -0x8000                = FixInt16    $ fromIntegral i
    | i >= -0x80000000            = FixInt32    $ fromIntegral i
    | otherwise                   = FixInt64    $ fromIntegral i

-- | Safely convert the given fixed integer value to one that has just the right
-- size.
anyValidFixInt :: FixInt -> FixInt
anyValidFixInt = fixIntValue . (integralValue :: FixInt -> Integer)

