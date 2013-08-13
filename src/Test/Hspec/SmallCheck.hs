{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hspec.SmallCheck where

import           Control.Applicative
import           Test.Hspec.Core
import           Test.SmallCheck
import           Test.SmallCheck.Drivers

instance Example (Property IO) where
  evaluateExample _ p = maybe Success (Fail . ppFailure) <$> smallCheckM 5 p
