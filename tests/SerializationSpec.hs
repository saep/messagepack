{-# OPTIONS_GHC -fno-warn-orphans #-}
module SerializationSpec where

import Control.Applicative
import Data.MessagePack
import Data.Serialize
import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString as BS
import qualified Data.Map as M


instance Arbitrary Object where
    arbitrary = sized $ \n -> oneof [ return ObjectNil
                                    , ObjectUInt   <$> arbitrary
                                    , ObjectInt    <$> arbitrary
                                    , ObjectBool   <$> arbitrary
                                    , ObjectFloat  <$> arbitrary
                                    , ObjectDouble <$> arbitrary
                                    , ObjectString <$> arbitrary
                                    , ObjectBinary <$> arbitrary
                                    , ObjectArray  <$> resize (3 * n `quot` 4) arbitrary
                                    , ObjectMap    <$> resize (3 * n `quot` 4) arbitrary
                                    , ObjectExt    <$> arbitrary <*> arbitrary ]

    shrink (ObjectString s) = map ObjectString $ shrink s
    shrink (ObjectBinary b) = map ObjectBinary $ shrink b
    shrink (ObjectArray a)  = map ObjectArray (shrink a) ++ a
    shrink (ObjectMap m)    = map ObjectMap (shrink m) ++ M.keys m ++ M.elems m
    shrink (ObjectExt t s)  = map (ObjectExt t) $ shrink s
    shrink _                = []

instance Arbitrary BS.ByteString where

    arbitrary = BS.pack <$> arbitrary

    shrink = map BS.pack . shrink . BS.unpack


spec :: Spec
spec = do
  describe "Object serialization and deserialization" $ do
    it "encode followed by decode is the identity" $ do
      \o -> decode (encode o) `shouldBe` Right o

