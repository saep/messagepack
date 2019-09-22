{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedLists #-}
module SerializationSpec where

import           Control.Applicative
import qualified Data.ByteString     as BS
import qualified Data.Map            as M
import           Data.MessagePack
import           Data.Serialize
import qualified Data.Vector         as Vector
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary Object where
    arbitrary = sized $ \n -> oneof
      [ return ObjectNil
      , ObjectUInt   <$> arbitrary
      , ObjectInt    <$> arbitrary
      , ObjectBool   <$> arbitrary
      , ObjectFloat  <$> arbitrary
      , ObjectDouble <$> arbitrary
      , ObjectString <$> arbitrary
      , ObjectBinary <$> arbitrary
      , ObjectArray . Vector.fromList <$> resize (3 * n `quot` 4) arbitrary
      , ObjectMap .Vector.fromList <$> resize (3 * n `quot` 4) arbitrary
      , ObjectExt    <$> arbitrary <*> arbitrary
      ]

    shrink (ObjectString s) = map ObjectString $ shrink s
    shrink (ObjectBinary b) = map ObjectBinary $ shrink b
    shrink (ObjectArray a)  =
      -- use list instance for arbitraty
      let shrinkedElements = shrink (Vector.toList a)
       in map (ObjectArray . Vector.fromList) shrinkedElements ++ Vector.toList a
    shrink (ObjectMap m)    =
      map (ObjectMap . Vector.fromList) (shrink (Vector.toList m))
                              <> Vector.toList (Vector.map fst m)
                              <> Vector.toList (Vector.map snd m)
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

