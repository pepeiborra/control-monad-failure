{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Failure.MTL (module Control.Failure) where

import Control.Failure

import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State.Lazy     as Lazy
import Control.Monad.State.Strict   as Strict
import Control.Monad.Writer.Lazy    as Lazy
import Control.Monad.Writer.Strict  as Strict
import Control.Monad.RWS.Lazy       as Lazy
import Control.Monad.RWS.Strict     as Strict
import Data.Monoid

-- -----------------------
-- MonadFailure Instances
-- -----------------------

instance (Error e, Monad m) => Failure e (ErrorT e m) where
  failure = throwError

instance MonadFailure e m => Failure e (ListT m) where
  failure = lift . failure

instance MonadFailure e m => Failure e (ReaderT r m) where
  failure = lift . failure

instance (Monoid w, MonadFailure e m) => Failure e (Lazy.WriterT w  m) where
  failure = lift . failure

instance MonadFailure e m => Failure e (Lazy.StateT s m) where
  failure = lift . failure

instance (Monoid w, MonadFailure e m) => Failure e (Lazy.RWST r w s m) where
  failure = lift . failure

instance (Monoid w, MonadFailure e m) => Failure e (Strict.WriterT w  m) where
  failure = lift . failure

instance MonadFailure e m => Failure e (Strict.StateT s m) where
  failure = lift . failure

instance (Monoid w, MonadFailure e m) => Failure e (Strict.RWST r w s m) where
  failure = lift . failure

-- ---------------------
-- WrapFailure instances
-- ---------------------

instance (WrapFailure e m, Monad m) => WrapFailure e (ListT m) where
  wrapFailure f = ListT . wrapFailure f . runListT

instance (WrapFailure e m, Monad m) => WrapFailure e (ReaderT r m) where
  wrapFailure f m = ReaderT $ \r -> wrapFailure f (runReaderT m r)

instance (WrapFailure e m, Monoid w, Monad m) => WrapFailure e (Lazy.WriterT w m) where
  wrapFailure f = Lazy.WriterT . wrapFailure f . Lazy.runWriterT


-- all the following instances require undecidable instances
instance (WrapFailure e m, Monad m) => WrapFailure e (Lazy.StateT s m) where
  wrapFailure f m = Lazy.StateT $ \s -> wrapFailure f (Lazy.runStateT m s)

instance (WrapFailure e m, Monoid w, Monad m) => WrapFailure e (Lazy.RWST r w s m) where
  wrapFailure f m = Lazy.RWST $ \r s -> wrapFailure f (Lazy.runRWST m r s)

instance (WrapFailure e m, Monad m) => WrapFailure e (Strict.StateT s m) where
  wrapFailure f m = Strict.StateT $ \s -> wrapFailure f (Strict.runStateT m s)

instance (WrapFailure e m, Monoid w, Monad m) => WrapFailure e (Strict.RWST r w s m) where
  wrapFailure f m = Strict.RWST $ \r s -> wrapFailure f (Strict.runRWST m r s)
