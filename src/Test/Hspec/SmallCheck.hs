{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hspec.SmallCheck (property) where

import           Control.Applicative
import           Test.Hspec.Core
import           Test.SmallCheck
import           Test.SmallCheck.Drivers

property :: Testable IO a => a -> Property IO
property = test

instance Example (Property IO) where
  evaluateExample c p = maybe Success (Fail . ppFailure) <$> smallCheckM (paramsSmallCheckDepth c) p
