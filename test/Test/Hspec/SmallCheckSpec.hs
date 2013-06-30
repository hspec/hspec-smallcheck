module Test.Hspec.SmallCheckSpec (main, spec) where

import           Test.Hspec

import           Test.Hspec.SmallCheck ()
import qualified Test.Hspec.Core as H
import qualified Test.Hspec.Runner as H
import           Test.SmallCheck
import           Test.SmallCheck.Drivers

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
        evaluateExample (test (/= (23 :: Int)) :: Property IO) `shouldReturn` H.Fail "there exists 23 such that\n  condition is false"

      it "propagates exceptions" $ do
        evaluateExample (error "foobar" :: Property IO) `shouldThrow` errorCall "foobar"
  where
    evaluateExample = H.evaluateExample defaultParams
    defaultParams = H.Params (H.configQuickCheckArgs H.defaultConfig) (const $ return ())
