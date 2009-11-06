{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

{-| Defines the class @MonadFailure@ for monads which can fail,
    and the class @MonadLoc@ for monads which support recording
    the source code location and building a stack trace.
-}
module Control.Monad.Failure where

import Control.Exception (throw, Exception)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.RWS

import Data.Monoid

class Monad m => MonadFailure e m where
    failure :: e -> m a

-- ----------
-- Instances
-- ----------
instance MonadFailure e Maybe where failure _ = Nothing
instance MonadFailure e []    where failure _ = []

instance Exception e => MonadFailure e IO where
  failure = Control.Exception.throw

instance (Error e) => MonadFailure e (Either e) where
  failure = Left

instance (Error e, Monad m) => MonadFailure e (ErrorT e m) where
  failure = throwError

instance MonadFailure e m => MonadFailure e (ListT m) where
  failure = lift . failure

instance MonadFailure e m => MonadFailure e (ReaderT r m) where
  failure = lift . failure

instance (Monoid w, MonadFailure e m) => MonadFailure e (WriterT w  m) where
  failure = lift . failure

instance MonadFailure e m => MonadFailure e (StateT s m) where
  failure = lift . failure

instance (Monoid w, MonadFailure e m) => MonadFailure e (RWST r w s m) where
  failure = lift . failure
