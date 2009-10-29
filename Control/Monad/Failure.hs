{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

{-| Defines the class @MonadFail@ for monads which can fail,
    and the class @MonadLoc@ for monads which support recording
    the source code location and building a stack trace.
-}
module Control.Monad.Failure where

import Control.Exception (throw, Exception)
import Control.Monad
#if TRANSFORMERS
import Control.Monad.Trans
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.RWS
#else
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
#endif

import Data.Monoid

class Monad m => MonadFail e m where
    failure :: e -> m a

-- ----------
-- Instances
-- ----------
instance MonadFail e Maybe where failure _ = Nothing
instance MonadFail e []    where failure _ = []

instance Exception e => MonadFail e IO where
  failure = Control.Exception.throw

instance (Error e) => MonadFail e (Either e) where
  failure = Left

instance (Error e, Monad m) => MonadFail e (ErrorT e m) where
  failure = throwError

instance MonadFail e m => MonadFail e (ListT m) where
  failure = lift . failure

instance MonadFail e m => MonadFail e (ReaderT r m) where
  failure = lift . failure

instance (Monoid w, MonadFail e m) => MonadFail e (WriterT w  m) where
  failure = lift . failure

instance MonadFail e m => MonadFail e (StateT s m) where
  failure = lift . failure

instance (Monoid w, MonadFail e m) => MonadFail e (RWST r w s m) where
  failure = lift . failure