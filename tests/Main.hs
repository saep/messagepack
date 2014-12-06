{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Data.MessagePack
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T

main :: IO ()
main = $(defaultMainGenerator)

instance Arbitrary FixInt where
    arbitrary = oneof
        [ PosFixInt   <$> choose (0, 127)
        , NegFixInt   <$> choose (-32,-1)
        , PosFixInt8  <$> arbitrary
        , PosFixInt16 <$> arbitrary
        , PosFixInt32 <$> arbitrary
        , PosFixInt64 <$> arbitrary
        , FixInt8     <$> arbitrary
        , FixInt16    <$> arbitrary
        , FixInt32    <$> arbitrary
        , FixInt64    <$> arbitrary
        ]

instance Arbitrary Object where
    arbitrary = sized $ \n -> oneof
        [ return ObjectNil
        -- cannot really test ObjectInt as it is not isomorph
        --, ObjectInt    <$> arbitrary
        , ObjectFixInt <$> arbitrary
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
    shrink (ObjectArray a)  = (map ObjectArray $ shrink a) ++ a
    shrink (ObjectMap m)    = (map ObjectMap $ shrink m) ++ M.keys m ++ M.elems m
    shrink (ObjectExt t s)  = map (ObjectExt t) $ shrink s
    shrink _                = []

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (M.Map k v) where

    arbitrary = M.fromList <$> arbitrary

    shrink  = map M.fromList . shrink . M.toList

instance Arbitrary BS.ByteString where

    arbitrary = BS.pack <$> arbitrary

    shrink = map BS.pack . shrink . BS.unpack

instance Arbitrary T.Text where

    arbitrary = T.pack <$> arbitrary

    shrink = map T.pack . shrink . T.unpack

shouldBeIsomorph :: Object -> Bool
shouldBeIsomorph o = case o of
    ObjectInt _ -> False
    _           -> True

prop_encodeDecodeIsIdentity :: Object -> Property
prop_encodeDecodeIsIdentity o =
    shouldBeIsomorph o ==> (either error (== o) . decode . encode) o

