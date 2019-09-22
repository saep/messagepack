{-# LANGUAGE OverloadedStrings #-}
module MessagePackClassSpec
    where

import           Data.MessagePack
import           SerializationSpec ()

import           Data.Int
import qualified Data.Map          as Map
import           Data.Text         (Text)
import qualified Data.Vector       as Vector
import           Data.Word

import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "unit" $ do
    it "is ObjectNil" $ do
      toObject () `shouldBe` ObjectNil
      fromObject ObjectNil `shouldBe` Just ()

  describe "boolean" $ do
    it "toObject is just the bool" $ property $
      \b -> toObject b `shouldBe` ObjectBool b

    let falseValues :: [Object]
        falseValues =
          [ ObjectBool False
          , ObjectInt  0
          , ObjectUInt 0
          , ObjectNil
          , ObjectBinary "0"
          , ObjectBinary ""
          , ObjectString "0"
          , ObjectString ""
          ]
    it "fromObject is truthy" $ property $
      \o -> o `notElem` falseValues ==> fromObject o `shouldBe` Just True

  describe "double" $ do
    it "toObject is ObjectDouble" $ property $ do
      \d -> toObject d `shouldBe` ObjectDouble d

    it "fromObject accepts any number" $ do
      fromObject (ObjectInt (-7)) `shouldBe` Just ((-7) :: Double)
      fromObject (ObjectUInt 7) `shouldBe` Just (7 :: Double)
      fromObject (ObjectDouble 7) `shouldBe` Just (7 :: Double)
      fromObject (ObjectFloat 7) `shouldBe` Just (7 :: Double)


  describe "integrals" $ do
    it "also accept floating values (just imagine a javascript client)" $ do
      fromObject (ObjectInt (-7)) `shouldBe` Just ((-7) :: Integer)
      fromObject (ObjectUInt 7) `shouldBe` Just (7 :: Integer)
      fromObject (ObjectDouble 7.8) `shouldBe` Just (8 :: Integer)
      fromObject (ObjectFloat (-7.5)) `shouldBe` Just ((-8) :: Integer)

    it "signed integrals are converted to ObjectInt" $ do
      toObject (8 :: Int8) `shouldBe` ObjectInt 8
      toObject (16 :: Int16) `shouldBe` ObjectInt 16
      toObject (32 :: Int32) `shouldBe` ObjectInt 32
      toObject (64 :: Int64) `shouldBe` ObjectInt 64
      toObject (64 :: Int) `shouldBe` ObjectInt 64

    it "unsigned integrals are converted to ObjectUInt" $ do
      toObject (8 :: Word8) `shouldBe` ObjectUInt 8
      toObject (16 :: Word16) `shouldBe` ObjectUInt 16
      toObject (32 :: Word32) `shouldBe` ObjectUInt 32
      toObject (64 :: Word64) `shouldBe` ObjectUInt 64
      toObject (64 :: Word) `shouldBe` ObjectUInt 64

    it "overflow in fromObject is Nothing" $ do
      let overflow = succ (fromIntegral (maxBound :: Int64))
      fromObject (ObjectUInt overflow) `shouldBe` (Nothing :: Maybe Int64)

    it "negative values values are cannot be converted to unsigned integrals" $ do
      fromObject (ObjectInt (-7)) `shouldBe` (Nothing :: Maybe Word)

  describe "vector" $ do
    it "maps each element to an object" $
      toObject (Vector.fromList [1,2,4 :: Int])
        `shouldBe` ObjectArray [ObjectInt 1, ObjectInt 2, ObjectInt 4]

  describe "map" $ do
    let objectMap = ObjectMap (Map.fromList
                                [ (ObjectUInt 1, ObjectString "1")
                                , (ObjectUInt 3, ObjectString "3")
                                , (ObjectUInt 7, ObjectString "7")
                                ])
        normalMap = Map.fromList [ (7::Word32, "7"::Text), (1,"1"), (3, "3")]
    it "maps keys and values properly to the map" $
      toObject normalMap `shouldBe` objectMap

    it "fromObject" $
      fromObject objectMap `shouldBe` Just normalMap

