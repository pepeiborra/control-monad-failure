{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}

{-| Defines the class @MonadFail@ for monads which can fail,
    and the class @MonadLoc@ for monads which support recording
    the source code location and building a stack trace.

 * Stack traces are only provided for explicitly annotated program points.
   For now there is the TH macro 'withLocTH' to help with this.
   Eventually a preprocessor could be written to automatically insert calls
   to 'withLoc' at every do statement.
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
import Language.Haskell.TH.Syntax hiding (lift)
import Text.PrettyPrint

class Monad m => MonadFail e m where
    failure :: e -> m a
    failWithSrcLoc :: String -> e -> m a
    failWithSrcLoc _ = failure

-- --------------------------------
-- Error stack traces
-- --------------------------------

-- | Generating stack traces for failures
class MonadLoc a where
  -- | 'withLoc' records the given source location in the failure trace
  --   if the underlying monad supports recording stack traces
  --
  --   By default 'withLoc' is defined as the identity on its second argument
  withLoc :: String -> a -> a

instance MonadLoc a where withLoc _ = id

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

-- | 'withLocTH' is a convenient TH macro which expands to 'withLoc' @\<source location\>@
--   Usage:
--
--  > f x = $withLocTH $ do
withLocTH :: Q Exp
withLocTH = do
  loc <- qLocation
  let loc_msg = showLoc loc
  [| withLoc loc_msg |]
 where
   showLoc Loc{loc_module=mod, loc_filename=filename, loc_start=start} = render $
                     {- text package <> char '.' <> -}
                     text mod <> parens (text filename) <> colon <+> text (show start)

-- ----------
-- Instances
-- ----------

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