-- | This module describes application error types.
module Error
  ( Error (..)
  , toHttpError

    -- * Pure errors handling
  , MonadError
  , With
  , ErrorWithSource (..)
  , toLogMsg
  , throwError
  , catchError
  , liftError
  ) where

import Relude

import Control.Monad.Except qualified as E
import Servant.Server (ServerError (..), err400, err403, err404, err500, err501)
import Text.Pretty.Simple (pShow)

import Logger qualified

-- | Type alias for 'E.MonadError' where @err@ is wrapped in 'ErrorWithSource'.
type MonadError err m = E.MonadError (ErrorWithSource err) m

-- | Type alias for errors that has access to 'CallStack'.
type With err m = (HasCallStack, MonadError err m)

-- | Wrapper around error type with attached source code position.
data ErrorWithSource err = ErrorWithSource
  { ewsSourcePosition :: !CallStack
  , ewsLogSeverity :: !Logger.Severity
  , ewsError :: !err
  }
  deriving stock (Show, Functor)
  deriving anyclass (Exception)

-- | Errors in application.
data Error
  = -- | The server cannot process the request because of a wrong request.
    BadRequest Text
  | -- | A required permission level was not met.
    NotAllowed Text
  | -- | Some exceptional circumstance has happened to stop execution and return.
    InternalError Text
  | -- | General not found.
    NotFound
  | -- | Functionality is not implemented yet.
    NotImplemented
  deriving stock (Show, Eq)

-- | Map 'Error' into a HTTP error code.
toHttpError :: Error -> ServerError
toHttpError = \case
  BadRequest msg -> err400{errBody = encodeUtf8 msg}
  NotAllowed msg -> err403{errBody = encodeUtf8 msg}
  InternalError msg -> err500{errBody = encodeUtf8 msg}
  NotFound -> err404
  NotImplemented -> err501

-- | Converts 'ErrorWithSource' to 'Msg' for logging purpose.
toLogMsg :: (Show err) => ErrorWithSource err -> Logger.Msg Logger.Severity
toLogMsg ErrorWithSource{..} =
  Logger.Msg
    { msgSeverity = ewsLogSeverity
    , msgStack = ewsSourcePosition
    , msgText = toStrict $ pShow ewsError
    }

-- | Specialized version of 'E.throwError' that attaches source code position of
-- the place where this error was thrown.
throwError :: (With err m) => Logger.Severity -> err -> m a
throwError severity = withFrozenCallStack do
  E.throwError . ErrorWithSource callStack severity
{-# INLINE throwError #-}

-- | Specialized version of 'E.catchError'.
catchError :: (With err m) => m a -> (err -> m a) -> m a
catchError action handler = action `E.catchError` (handler . ewsError)
{-# INLINE catchError #-}

-- | Lift errors from 'Either' by re-throwing them with attached source position.
liftError :: (With err m) => Logger.Severity -> Either err a -> m a
liftError severity = either (throwError severity) pure
{-# INLINE liftError #-}
