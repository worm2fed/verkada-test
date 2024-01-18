-- | This module provides logging ability. Currently it only reexports @co-log@.
module Logger
  ( Log
  , With
  , Logger
  , setLogger

    -- * Reexport
  , module Colog
  ) where

import Relude

import Colog hiding
  ( LoggerT
  , Message
  , WithLog
  , richMessageAction
  )
import Colog qualified (Message, WithLog)

-- | Type alias to not confuse that 'Colog.Message.Message' is about logging.
type Log = Colog.Message

-- | Constrain for actions with logging.
type With env m = Colog.WithLog env Log m

-- | Alias for logger, needs to simplify signatures.
type Logger m = LogAction m Log

-- | Helper to set logging level.
--  * 'Debug' - Information useful for debug purposes
--  * 'Info' - Normal operational information
--  * 'Warning' - General warnings, non-critical failures
--  * 'Error' - General errors/severe errors
setLogger :: (MonadIO m) => Severity -> Logger m
setLogger severity = filterBySeverity severity msgSeverity richMessageAction
{-# INLINE setLogger #-}

-- | Action that constructs 'RichMessage' and prints formatted 'Log' for it
-- to 'stdout'.
richMessageAction :: (MonadIO m) => Logger m
richMessageAction =
  upgradeMessageAction defaultFieldMap $
    cmapM fmtRichMessageDefault logTextStdout
{-# INLINE richMessageAction #-}
{-# SPECIALIZE richMessageAction :: Logger IO #-}
