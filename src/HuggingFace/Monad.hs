{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : HuggingFace.Monad
-- Description : Concrete client computations with typed errors
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
--
-- Compose client operations in t'HuggingFaceM' and call 'runHuggingFaceM' at
-- the application's IO boundary. The constructor and transformer representation
-- are opaque; the standard 'MonadIO' and 'MonadError' operations are available.
module HuggingFace.Monad
    ( HuggingFaceM, runHuggingFaceM, liftHuggingFaceM
    , liftEither, liftIO, throwError, catchError
    ) where

import Control.Monad.Except
    ( ExceptT (..), MonadError, catchError, liftEither, runExceptT, throwError )
import Control.Monad.IO.Class (MonadIO, liftIO)
import HuggingFace.Error (HuggingFaceError)

-- | A concrete computation over @ExceptT HuggingFaceError IO@.
-- Typed errors stop subsequent actions unless handled with 'catchError'.
-- Ordinary IO exceptions and asynchronous cancellation remain IO exceptions;
-- 'catchError' does not intercept them. HTTP operations translate transport
-- exceptions into the library's redacted errors at the transport boundary.
newtype HuggingFaceM a = HuggingFaceM (ExceptT HuggingFaceError IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadError HuggingFaceError)
        via (ExceptT HuggingFaceError IO)

-- | Run a computation, returning its typed error or result. This does not
-- catch IO exceptions or asynchronous cancellation.
runHuggingFaceM :: HuggingFaceM a -> IO (Either HuggingFaceError a)
runHuggingFaceM (HuggingFaceM action) = runExceptT action

-- | Embed an IO action that already returns a typed result. Useful when
-- adapting callback-based IO APIs or existing clients. Use 'liftEither' for
-- pure parser results and 'liftIO' for IO actions without typed errors.
-- The action runs once; its IO exceptions propagate unchanged.
liftHuggingFaceM :: IO (Either HuggingFaceError a) -> HuggingFaceM a
liftHuggingFaceM = HuggingFaceM . ExceptT
