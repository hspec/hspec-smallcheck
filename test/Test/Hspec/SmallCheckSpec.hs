module Test.Hspec.SmallCheckSpec (main, spec) where

import           Test.Hspec

import           Test.Hspec.SmallCheck ()
import qualified Test.Hspec.Core as H
import qualified Test.Hspec.Runner as H
import           Test.SmallCheck
import           Test.SmallCheck.Drivers
import           Test.QuickCheck (stdArgs)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "evaluateExample" $ do
    context "Property IO" $ do
      it "returns Success if property holds" $ do
        evaluateExample (test True :: Property IO) `shouldReturn` H.Success

      it "returns Fail if property does not hold" $ do
        evaluateExample (test False :: Property IO) `shouldReturn` H.Fail "condition is false"

      it "shows what falsified it" $ do
        evaluateExample (test (/= (2 :: Int)) :: Property IO) `shouldReturn` H.Fail "there exists 2 such that\n  condition is false"

      it "propagates exceptions" $ do
        evaluateExample (error "foobar" :: Property IO) `shouldThrow` errorCall "foobar"
  where

    evaluateExample :: Example a => a -> IO H.Result
    evaluateExample e = H.evaluateExample e defaultParams id (const $ return ())

    defaultParams :: H.Params
    defaultParams = H.Params stdArgs (H.configSmallCheckDepth H.defaultConfig)
