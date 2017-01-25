{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hspec.SmallCheck (property) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Data.IORef
import           Test.Hspec.Core.Spec
import           Test.SmallCheck
import           Test.SmallCheck.Drivers

property :: Testable IO a => a -> Property IO
property = test

instance Example (Property IO) where
  type Arg (Property IO) = ()
  evaluateExample p c _ reportProgress = do
    counter <- newIORef 0
    let hook _ = do
          modifyIORef counter succ
          n <- readIORef counter
          reportProgress (n, 0)
#if MIN_VERSION_hspec_core(2,4,0)
    maybe Success (Failure Nothing . Reason . ppFailure) <$> smallCheckWithHook (paramsSmallCheckDepth c) hook p
#elif MIN_VERSION_hspec_core(2,2,0)
    maybe Success (Fail Nothing . ppFailure) <$> smallCheckWithHook (paramsSmallCheckDepth c) hook p
#else
    maybe Success (Fail . ppFailure) <$> smallCheckWithHook (paramsSmallCheckDepth c) hook p
#endif
