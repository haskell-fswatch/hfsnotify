{-# OPTIONS_GHC -F -pgmF sandwich-discover -optF --module-prefix=FSNotify. #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module FSNotify.Test (
  tests
  ) where

import Test.Sandwich
import TestLib.Util

#insert_test_imports


tests :: TestSpec
tests = $(getSpecFromFolder defaultGetSpecFromFolderOptions {
             getSpecCombiner = 'discoveredInParallel
             })
