{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Data.MessagePack
Description : Object data type with Serialize instances for it
Copyright   : (c) Rodrigo Setti, 2014
License     : MIT
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : portable

@Object@ is a message pack object, and it have constructors for all message
pack types.

The @Serialize@ instances define how Object values may be serialized and
deserialized to message pack binary format, following the specification.
-}
module Data.MessagePack
  ( Object(..)
  , MessagePack(..)
  , (+:)
  ) where

import           Control.Applicative
import           Control.DeepSeq       (NFData)
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString       as BS
import           Data.Int
import qualified Data.Map              as Map
import           Data.MessagePack.Spec
import           Data.Serialize
import           Data.Text             (Text)
import           Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import qualified Data.Vector           as Vector
import           Data.Word
import           GHC.Generics          (Generic)

infixr 5 +:

-- | Convenient operator to create a list of 'Object's from normal values.
-- @
-- values +: of :+ different :+ types :+ can +: be +: combined +: this +: way +: []
-- @
(+:) :: (MessagePack o) => o -> [Object] -> [Object]
o +: os = toObject o : os


data Object = ObjectNil
            -- | Unsigned integers from the MsgPack protocol: uint 8, uint 16, uint 32, uint 64
            | ObjectUInt   Word64
            -- | Signed integers and fixnums from the MsgPack protocol: positive fixnum, negative fixnum, int 8, int 16, int 32, int 64
            | ObjectInt    Int64
            | ObjectBool   Bool
            | ObjectFloat  Float
            | ObjectDouble Double
            | ObjectString BS.ByteString
            | ObjectBinary BS.ByteString
            | ObjectArray  [Object]
            | ObjectMap    (Map.Map Object Object )
            | ObjectExt    !Int8 BS.ByteString
    deriving (Eq, Ord, Show, Generic)

instance NFData Object


-- | This class can be used to conveniently convert between messagepack objects
-- and Haskell objects. The operator '(+:)' is a convenient operator that can be
-- used to create 'Object' lists as these are often used for RPC stuff or
-- implementations of instances of 'MessagePack'.
class MessagePack o where
    {-# MINIMAL toObject, fromObject #-}

    -- | Convert a Haskell value to a messagepack 'Object'. As you shouldn't
    -- have objects that can only sometimes be converted to an 'Object', this
    -- direction of conversion doesn't throw an error and you probably don't
    -- want to convert objects for which it isn't the case.
    toObject :: o -> Object

    -- | The conversion is sometimes lenient if it comes to convert a value.
    --   An example for this is the 'Bool' instance because there are languages
    --   that do not have a proper boolean type (e.g. C, vimL). This behavior is
    --   convenient and sometimes wnated in practice, if you don't want this,
    --   simply use the 'Object' types directly.
    fromObject :: Object -> Maybe o


instance Serialize Object where
    put (ObjectUInt i)
          | i >= 0   && i < 0x100       = putWord8 uint8  >> putWord8    (fromIntegral i)
          | i >= 0   && i < 0x10000     = putWord8 uint16 >> putWord16be (fromIntegral i)
          | i >= 0   && i < 0x100000000 = putWord8 uint32 >> putWord32be (fromIntegral i)
          | otherwise                   = putWord8 uint64 >> putWord64be (fromIntegral i)

    put (ObjectInt i)
          | i >= 0           && i <= 127        = putWord8 $ fromIntegral i
          | i >= -32         && i <= -1         = putWord8 $ fromIntegral i
          | i >= -0x80       && i < 0x80        = putWord8 int8   >> putWord8    (fromIntegral i)
          | i >= -0x8000     && i < 0x8000      = putWord8 int16  >> putWord16be (fromIntegral i)
          | i >= -0x80000000 && i < 0x80000000  = putWord8 int32  >> putWord32be (fromIntegral i)
          | otherwise                           = putWord8 int64  >> putWord64be (fromIntegral i)

    put ObjectNil          = putWord8 nil

    put (ObjectBool b)     = putWord8 $ if b then true else false

    put (ObjectFloat f)    = putWord8 float32 >> putFloat32be f

    put (ObjectDouble d)   = putWord8 float64 >> putFloat64be d

    put (ObjectString t) =
        header >> putByteString t
     where
        size  = BS.length t
        header
          | size <= 31     = putWord8 $ fixstr .|. fromIntegral size
          | size < 0x100   = putWord8 str8  >> putWord8 (fromIntegral size)
          | size < 0x10000 = putWord8 str16 >> putWord16be (fromIntegral size)
          | otherwise      = putWord8 str32 >> putWord32be (fromIntegral size)

    put (ObjectBinary bytes) =
        header >> putByteString bytes
      where
        size  = BS.length bytes
        header
          | size < 0x100   = putWord8 bin8  >> putWord8 (fromIntegral size)
          | size < 0x10000 = putWord8 bin16 >> putWord16be (fromIntegral size)
          | otherwise      = putWord8 bin32 >> putWord32be (fromIntegral size)

    put (ObjectArray a)    =
        buildArray >> mapM_ put a
      where
        size = length a
        buildArray
          | size <= 15     = putWord8 $ fixarray .|. fromIntegral size
          | size < 0x10000 = putWord8 array16 >> putWord16be (fromIntegral size)
          | otherwise      = putWord8 array32 >> putWord32be (fromIntegral size)

    put (ObjectMap m)      =
        buildMap >> mapM_ put (Map.toList m)
      where
        size = Map.size m
        buildMap
            | size <= 15     = putWord8 $ fixmap .|. fromIntegral size
            | size < 0x10000 = putWord8 map16 >> putWord16be (fromIntegral size)
            | otherwise      = putWord8 map32 >> putWord32be (fromIntegral size)

    put (ObjectExt t bytes) = header >> putWord8 (fromIntegral t) >> putByteString bytes
      where
        size = BS.length bytes
        header
          | size == 1      = putWord8 fixext1
          | size == 2      = putWord8 fixext2
          | size == 4      = putWord8 fixext4
          | size == 8      = putWord8 fixext8
          | size == 16     = putWord8 fixext16
          | size < 0x100   = putWord8 ext8  >> putWord8 (fromIntegral size)
          | size < 0x10000 = putWord8 ext16 >> putWord16be (fromIntegral size)
          | otherwise      = putWord8 ext32 >> putWord32be (fromIntegral size)

    get =
        getWord8 >>= getObject
      where
        getObject k
          | k == nil                          = return ObjectNil
          | k == false                        = return $ ObjectBool False
          | k == true                         = return $ ObjectBool True

          | k == bin8                         = do n <- fromIntegral <$> getWord8
                                                   ObjectBinary <$> getByteString n
          | k == bin16                        = do n <- fromIntegral <$> getWord16be
                                                   ObjectBinary <$> getByteString n
          | k == bin32                        = do n <- fromIntegral <$> getWord32be
                                                   ObjectBinary <$> getByteString n

          | k == float32                      = ObjectFloat  <$> getFloat32be
          | k == float64                      = ObjectDouble <$> getFloat64be

          | k .&. posFixintMask == posFixint  = return $ ObjectInt $ fromIntegral k
          | k .&. negFixintMask == negFixint  = return $ ObjectInt $ fromIntegral (fromIntegral k :: Int8)
          | k == uint8                        = ObjectUInt <$> fromIntegral <$> getWord8
          | k == uint16                       = ObjectUInt <$> fromIntegral <$> getWord16be
          | k == uint32                       = ObjectUInt <$> fromIntegral <$> getWord32be
          | k == uint64                       = ObjectUInt <$> getWord64be
          | k == int8                         = ObjectInt <$> fromIntegral <$> (get :: Get Int8)
          | k == int16                        = ObjectInt <$> fromIntegral <$> (get :: Get Int16)
          | k == int32                        = ObjectInt <$> fromIntegral <$> (get :: Get Int32)
          | k == int64                        = ObjectInt <$> fromIntegral <$> (get :: Get Int64)

          | k .&. fixstrMask    == fixstr     = let n = fromIntegral $ k .&. complement fixstrMask
                                                in  ObjectString <$> getByteString n
          | k == str8                         = do n <- fromIntegral <$> getWord8
                                                   ObjectString <$> getByteString n
          | k == str16                        = do n <- fromIntegral <$> getWord16be
                                                   ObjectString <$> getByteString n
          | k == str32                        = do n <- fromIntegral <$> getWord32be
                                                   ObjectString <$> getByteString n

          | k .&. fixarrayMask  == fixarray   = let n = fromIntegral $ k .&. complement fixarrayMask
                                                in  ObjectArray <$> replicateM n get
          | k == array16                      = do n <- fromIntegral <$> getWord16be
                                                   ObjectArray <$> replicateM n get
          | k == array32                      = do n <- fromIntegral <$> getWord32be
                                                   ObjectArray <$> replicateM n get

          | k .&. fixmapMask    == fixmap     = let n = fromIntegral $ k .&. complement fixmapMask
                                                in  ObjectMap <$> Map.fromList <$> replicateM n get
          | k == map16                        = do n <- fromIntegral <$> getWord16be
                                                   ObjectMap <$> Map.fromList <$> replicateM n get
          | k == map32                        = do n <- fromIntegral <$> getWord32be
                                                   ObjectMap <$> Map.fromList <$> replicateM n get
          | k == ext8                         = do n <- fromIntegral <$> getWord8
                                                   ObjectExt <$> (fromIntegral <$> getWord8)
                                                             <*> getByteString n
          | k == ext16                        = do n <- fromIntegral <$> getWord16be
                                                   ObjectExt <$> (fromIntegral <$> getWord8)
                                                             <*> getByteString n
          | k == ext32                        = do n <- fromIntegral <$> getWord32be
                                                   ObjectExt <$> (fromIntegral <$> getWord8)
                                                             <*> getByteString n
          | k == fixext1                      = ObjectExt <$> (fromIntegral <$> getWord8)
                                                          <*> getByteString 1
          | k == fixext2                      = ObjectExt <$> (fromIntegral <$> getWord8)
                                                          <*> getByteString 2
          | k == fixext4                      = ObjectExt <$> (fromIntegral <$> getWord8)
                                                          <*> getByteString 4
          | k == fixext8                      = ObjectExt <$> (fromIntegral <$> getWord8)
                                                          <*> getByteString 8
          | k == fixext16                     = ObjectExt <$> (fromIntegral <$> getWord8)
                                                          <*> getByteString 16

          | otherwise                         = fail $ "mark byte not supported: " ++ show k


instance MessagePack Object where
    toObject o = o

    fromObject o = Just o

instance MessagePack () where
    toObject _           = ObjectNil

    fromObject ObjectNil = pure ()
    fromObject o         = Nothing

instance MessagePack Bool where
    toObject = ObjectBool

    fromObject (ObjectBool o)     = pure o
    fromObject (ObjectInt  0)     = pure False
    fromObject (ObjectUInt 0)     = pure False
    fromObject ObjectNil          = pure False
    fromObject (ObjectBinary "0") = pure False
    fromObject (ObjectBinary "")  = pure False
    fromObject (ObjectString "0") = pure False
    fromObject (ObjectString "")  = pure False
    fromObject _                  = pure True

instance MessagePack Double where
    toObject                    = ObjectDouble

    fromObject (ObjectDouble o) = pure o
    fromObject (ObjectFloat o)  = pure $ realToFrac o
    fromObject (ObjectInt o)    = pure $ fromIntegral o
    fromObject (ObjectUInt o)   = pure $ fromIntegral o
    fromObject o                = Nothing

instance MessagePack Integer where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt o)    = pure $ toInteger o
    fromObject (ObjectUInt o)   = pure $ toInteger o
    fromObject (ObjectDouble o) = pure $ round o
    fromObject (ObjectFloat o)  = pure $ round o
    fromObject o                = Nothing

instance MessagePack Int64 where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt o)    = pure $ o
    fromObject (ObjectUInt o)   = if o <= fromIntegral (maxBound :: Int64)
                                     then pure $ fromIntegral o
                                     else Nothing
    fromObject (ObjectDouble o) = pure $ round o
    fromObject (ObjectFloat o)  = pure $ round o
    fromObject o                = Nothing

instance MessagePack Int where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt o)    = pure $ fromIntegral o
    fromObject (ObjectUInt o)   = if o <= fromIntegral (maxBound :: Int)
                                     then pure $ fromIntegral o
                                     else Nothing
    fromObject (ObjectDouble o) = pure $ round o
    fromObject (ObjectFloat o)  = pure $ round o
    fromObject o                = Nothing

instance MessagePack Int32 where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt o)    = pure $ fromIntegral o
    fromObject (ObjectUInt o)   = if o <= fromIntegral (maxBound :: Int32)
                                     then pure $ fromIntegral o
                                     else Nothing
    fromObject (ObjectDouble o) = pure $ round o
    fromObject (ObjectFloat o)  = pure $ round o
    fromObject o                = Nothing

instance MessagePack Int16 where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt o)    = pure $ fromIntegral o
    fromObject (ObjectUInt o)   = if o <= fromIntegral (maxBound :: Int16)
                                     then pure $ fromIntegral o
                                     else Nothing
    fromObject (ObjectDouble o) = pure $ round o
    fromObject (ObjectFloat o)  = pure $ round o
    fromObject o                = Nothing

instance MessagePack Int8 where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt o)    = pure $ fromIntegral o
    fromObject (ObjectUInt o)   = if o <= fromIntegral (maxBound :: Int8)
                                     then pure $ fromIntegral o
                                     else Nothing
    fromObject (ObjectDouble o) = pure $ round o
    fromObject (ObjectFloat o)  = pure $ round o
    fromObject o                = Nothing

instance MessagePack Word64 where
    toObject                    = ObjectUInt

    fromObject (ObjectInt o)    = if o < 0 then Nothing else pure $ fromIntegral o
    fromObject (ObjectUInt o)   = pure o
    fromObject (ObjectDouble o) = pure $ round o
    fromObject (ObjectFloat o)  = pure $ round o
    fromObject o                = Nothing

instance MessagePack Word where
    toObject                    = ObjectUInt . fromIntegral

    fromObject (ObjectInt o)    = if o < 0 then Nothing else pure $ fromIntegral o
    fromObject (ObjectUInt o)   = pure $ fromIntegral o
    fromObject (ObjectDouble o) = pure $ round o
    fromObject (ObjectFloat o)  = pure $ round o
    fromObject o                = Nothing


instance MessagePack Word32 where
    toObject                    = ObjectUInt . fromIntegral

    fromObject (ObjectInt o)    = if o >= 0 && o <= fromIntegral (maxBound :: Word32)
                                     then pure $ fromIntegral o
                                     else Nothing
    fromObject (ObjectUInt o)   = if o <= fromIntegral (maxBound :: Word32)
                                     then pure $ fromIntegral o
                                     else Nothing
    fromObject (ObjectDouble o) = pure $ round o
    fromObject (ObjectFloat o)  = pure $ round o
    fromObject o                = Nothing

instance MessagePack Word16 where
    toObject                    = ObjectUInt . fromIntegral

    fromObject (ObjectInt o)    = if o >= 0 && o <= fromIntegral (maxBound :: Word16)
                                     then pure $ fromIntegral o
                                     else Nothing
    fromObject (ObjectUInt o)   = if o <= fromIntegral (maxBound :: Word16)
                                     then pure $ fromIntegral o
                                     else Nothing
    fromObject (ObjectDouble o) = pure $ round o
    fromObject (ObjectFloat o)  = pure $ round o
    fromObject o                = Nothing

instance MessagePack Word8 where
    toObject                    = ObjectUInt . fromIntegral

    fromObject (ObjectInt o)    = if o >= 0 && o <= fromIntegral (maxBound :: Word8)
                                     then pure $ fromIntegral o
                                     else Nothing
    fromObject (ObjectUInt o)   = if o <= fromIntegral (maxBound :: Word8)
                                     then pure $ fromIntegral o
                                     else Nothing
    fromObject (ObjectDouble o) = pure $ round o
    fromObject (ObjectFloat o)  = pure $ round o
    fromObject o                = Nothing

instance (MessagePack o1, MessagePack o2) => MessagePack (o1, o2) where
    toObject (o1, o2) = ObjectArray $ [toObject o1, toObject o2]

    fromObject (ObjectArray [o1, o2]) = (,)
        <$> fromObject o1
        <*> fromObject o2
    fromObject o = Nothing

instance (MessagePack o1, MessagePack o2, MessagePack o3) => MessagePack (o1, o2, o3) where
    toObject (o1, o2, o3) = ObjectArray $ [toObject o1, toObject o2, toObject o3]

    fromObject (ObjectArray [o1, o2, o3]) = (,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
    fromObject o = Nothing

instance (Ord key, MessagePack key, MessagePack val)
        => MessagePack (Map.Map key val) where
    toObject = ObjectMap
        . Map.fromList . map (\(k, v) -> (toObject k, toObject v)) . Map.toList

    fromObject (ObjectMap om) = fmap Map.fromList
            . sequence
            . map (\(k, v) -> liftA2 (,) (fromObject k) (fromObject v))
            $ Map.toList om

    fromObject o = Nothing


instance MessagePack Text where
    toObject                    = ObjectString . encodeUtf8

    fromObject (ObjectBinary o) = pure $ decodeUtf8 o
    fromObject (ObjectString o) = pure $ decodeUtf8 o
    fromObject o                = Nothing


instance MessagePack BS.ByteString where
    toObject                    = ObjectBinary

    fromObject (ObjectBinary o) = pure o
    fromObject (ObjectString o) = pure o
    fromObject o                = Nothing

instance MessagePack o => MessagePack (Maybe o) where
    toObject = maybe ObjectNil toObject

    fromObject ObjectNil = return Nothing
    fromObject o         = fromObject o


instance MessagePack o => MessagePack (Vector.Vector o) where
    toObject = ObjectArray . Vector.toList . Vector.map toObject

    fromObject (ObjectArray os) = Vector.fromList <$> mapM fromObject os
    fromObject o                = Nothing

instance (MessagePack o1, MessagePack o2, MessagePack o3, MessagePack o4) => MessagePack (o1, o2, o3, o4) where
    toObject (o1, o2, o3, o4) = ObjectArray $ [toObject o1, toObject o2, toObject o3, toObject o4]

    fromObject (ObjectArray [o1, o2, o3, o4]) = (,,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
        <*> fromObject o4
    fromObject o = Nothing


instance (MessagePack o1, MessagePack o2, MessagePack o3, MessagePack o4, MessagePack o5) => MessagePack (o1, o2, o3, o4, o5) where
    toObject (o1, o2, o3, o4, o5) = ObjectArray $ [toObject o1, toObject o2, toObject o3, toObject o4, toObject o5]

    fromObject (ObjectArray [o1, o2, o3, o4, o5]) = (,,,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
        <*> fromObject o4
        <*> fromObject o5
    fromObject o = Nothing


instance (MessagePack o1, MessagePack o2, MessagePack o3, MessagePack o4, MessagePack o5, MessagePack o6) => MessagePack (o1, o2, o3, o4, o5, o6) where
    toObject (o1, o2, o3, o4, o5, o6) = ObjectArray $ [toObject o1, toObject o2, toObject o3, toObject o4, toObject o5, toObject o6]

    fromObject (ObjectArray [o1, o2, o3, o4, o5, o6]) = (,,,,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
        <*> fromObject o4
        <*> fromObject o5
        <*> fromObject o6
    fromObject o = Nothing


instance (MessagePack o1, MessagePack o2, MessagePack o3, MessagePack o4, MessagePack o5, MessagePack o6, MessagePack o7) => MessagePack (o1, o2, o3, o4, o5, o6, o7) where
    toObject (o1, o2, o3, o4, o5, o6, o7) = ObjectArray $ [toObject o1, toObject o2, toObject o3, toObject o4, toObject o5, toObject o6, toObject o7]

    fromObject (ObjectArray [o1, o2, o3, o4, o5, o6, o7]) = (,,,,,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
        <*> fromObject o4
        <*> fromObject o5
        <*> fromObject o6
        <*> fromObject o7
    fromObject o = Nothing


instance (MessagePack o1, MessagePack o2, MessagePack o3, MessagePack o4, MessagePack o5, MessagePack o6, MessagePack o7, MessagePack o8) => MessagePack (o1, o2, o3, o4, o5, o6, o7, o8) where
    toObject (o1, o2, o3, o4, o5, o6, o7, o8) = ObjectArray $ [toObject o1, toObject o2, toObject o3, toObject o4, toObject o5, toObject o6, toObject o7, toObject o8]

    fromObject (ObjectArray [o1, o2, o3, o4, o5, o6, o7, o8]) = (,,,,,,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
        <*> fromObject o4
        <*> fromObject o5
        <*> fromObject o6
        <*> fromObject o7
        <*> fromObject o8
    fromObject o = Nothing


instance (MessagePack o1, MessagePack o2, MessagePack o3, MessagePack o4, MessagePack o5, MessagePack o6, MessagePack o7, MessagePack o8, MessagePack o9) => MessagePack (o1, o2, o3, o4, o5, o6, o7, o8, o9) where
    toObject (o1, o2, o3, o4, o5, o6, o7, o8, o9) = ObjectArray $ [toObject o1, toObject o2, toObject o3, toObject o4, toObject o5, toObject o6, toObject o7, toObject o8, toObject o9]

    fromObject (ObjectArray [o1, o2, o3, o4, o5, o6, o7, o8, o9]) = (,,,,,,,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
        <*> fromObject o4
        <*> fromObject o5
        <*> fromObject o6
        <*> fromObject o7
        <*> fromObject o8
        <*> fromObject o9
    fromObject o = Nothing

