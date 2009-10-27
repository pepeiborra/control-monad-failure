{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Failure (
                              module Control.Monad,
                              MonadFail(..)
                             ) where

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
import Text.PrettyPrint

class Monad m => MonadFail e m where
    failure :: e -> m a
    failWithSrcLoc :: String -> e -> m a
    failWithSrcLoc _ = failure

{-| Given a list of source locations and an error, @showFailWithStackTrace@ produces output of the form

>       <exception details>
>        in <module a>(<file a.hs>): (12,6)
>           <module b>(<file b.hs>): (11,7)
>           ...

-}
showFailWithStackTrace :: Show e => [String] -> e -> String
showFailWithStackTrace trace e = render$
             text (show e) $$
             text " in" <+> (vcat (map text $ reverse trace))

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