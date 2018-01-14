{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
module Test.Hspec.SmallCheckSpec (main, spec) where

import           Test.Hspec

import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Runner as H
import           Test.SmallCheck (Property)
import           Test.QuickCheck (stdArgs)

import           Test.Hspec.SmallCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "evaluateExample" $ do
    context "Property IO" $ do
      it "returns Success if property holds" $ do
        evaluateExample (property True) `shouldReturn` H.Success

      it "returns Fail if property does not hold" $ do
        evaluateExample (property False) `shouldReturn` H.Failure Nothing (H.Reason "condition is false")

      it "shows what falsified it" $ do
        evaluateExample (property (/= (2 :: Int))) `shouldReturn` H.Failure Nothing (H.Reason "there exists 2 such that\n  condition is false")

      it "propagates exceptions" $ do
        evaluateExample (error "foobar" :: Property IO) `shouldThrow` errorCall "foobar"
  where
    evaluateExample :: (Example a, Arg a ~ ()) => a -> IO H.Result
    evaluateExample e = H.evaluateExample e defaultParams ($ ()) (const $ return ())

    defaultParams :: H.Params
    defaultParams = H.Params stdArgs (H.configSmallCheckDepth H.defaultConfig)
