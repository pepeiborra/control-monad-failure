{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Control.Monad.Failure.Transformers where

import Control.Monad.Failure.Class

import Control.Exception (throw, catch, Exception, SomeException(..))
import Control.Monad
import "transformers" Control.Monad.Trans
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy     as Lazy
import Control.Monad.Trans.State.Strict   as Strict
import Control.Monad.Trans.Writer.Lazy    as Lazy
import Control.Monad.Trans.Writer.Strict  as Strict
import Control.Monad.Trans.RWS.Lazy       as Lazy
import Control.Monad.Trans.RWS.Strict     as Strict
import Data.Monoid

-- -----------------------
-- MonadFailure Instances
-- -----------------------
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

instance (Monoid w, MonadFailure e m) => MonadFailure e (Lazy.WriterT w  m) where
  failure = lift . failure

instance MonadFailure e m => MonadFailure e (Lazy.StateT s m) where
  failure = lift . failure

instance (Monoid w, MonadFailure e m) => MonadFailure e (Lazy.RWST r w s m) where
  failure = lift . failure

instance (Monoid w, MonadFailure e m) => MonadFailure e (Strict.WriterT w  m) where
  failure = lift . failure

instance MonadFailure e m => MonadFailure e (Strict.StateT s m) where
  failure = lift . failure

instance (Monoid w, MonadFailure e m) => MonadFailure e (Strict.RWST r w s m) where
  failure = lift . failure

-- ---------------------
-- WrapFailure instances
-- ---------------------

instance Exception e => WrapFailure e IO where
  wrapFailure f m = m `Control.Exception.catch` \e@SomeException{} -> Control.Exception.throw (f e)

instance WrapFailure e m => WrapFailure e (ListT m) where
  wrapFailure f = ListT . wrapFailure f . runListT

instance WrapFailure e m => WrapFailure e (ReaderT r m) where
  wrapFailure f m = ReaderT $ \r -> wrapFailure f (runReaderT m r)

instance (WrapFailure e m, Monoid w) => WrapFailure e (Lazy.WriterT w m) where
  wrapFailure f = Lazy.WriterT . wrapFailure f . Lazy.runWriterT


-- all the following instances require undecidable instances
instance WrapFailure e m => WrapFailure e (Lazy.StateT s m) where
  wrapFailure f m = Lazy.StateT $ \s -> wrapFailure f (Lazy.runStateT m s)

instance (WrapFailure e m, Monoid w) => WrapFailure e (Lazy.RWST r w s m) where
  wrapFailure f m = Lazy.RWST $ \r s -> wrapFailure f (Lazy.runRWST m r s)

instance WrapFailure e m => WrapFailure e (Strict.StateT s m) where
  wrapFailure f m = Strict.StateT $ \s -> wrapFailure f (Strict.runStateT m s)

instance (WrapFailure e m, Monoid w) => WrapFailure e (Strict.RWST r w s m) where
  wrapFailure f m = Strict.RWST $ \r s -> wrapFailure f (Strict.runRWST m r s)
