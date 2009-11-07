{-# LANGUAGE CPP #-}
module Control.Monad.Failure (module Control.Monad.Failure.Class) where

import Control.Monad.Failure.Class

import Control.Monad.Failure.Transformers

#ifdef MTL
import Control.Monad.Failure.Mtl
#endif