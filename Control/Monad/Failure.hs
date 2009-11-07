{-# LANGUAGE CPP #-}
module Control.Monad.Failure where

import Control.Monad.Failure.Class

import Control.Monad.Failure.Transformers

#ifdef MTL
import Control.Monad.Failure.Mtl
#endif