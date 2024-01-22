-- | This module defines application and environment.
module Application
  ( App
  , Env (..)
  , runWithNewEnv
  , runWithLog

    -- * Constraints
  , WithEffects
  , WithLog
  , WithError
  ) where

import Relude

import Control.Exception (catch, throwIO, try)
import Control.Monad.Except (MonadError (..))
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq

import Error (Error, ErrorWithSource)
import Error qualified
import Logger qualified
import Types (SumCommandReq, SumCommandResp)

-- | Contains all available effects in constraint.
-- NOTE: use it only in signature for endpoints.
type WithEffects m =
  ( MonadIO m
  , WithLog m
  , WithError m
  )

-- | Constraint to actions with logging.
type WithLog m = Logger.With Env m

-- | Constraint to actions that can throw and catch pure errors with call-stack.
type WithError m = Error.With Error m

-- | App environment.
data Env = Env
  { eLogger :: !(Logger.Logger App)
  -- ^ Logger action.
  , eQueue :: !(TVar (Seq SumCommandReq))
  , eResponses :: !(TVar (Map Int SumCommandResp))
  }

-- | Create 'Env'.
buildEnv :: IO Env
buildEnv = do
  let eLogger = Logger.setLogger Logger.Debug
  eQueue <- newTVarIO Seq.empty
  eResponses <- newTVarIO M.empty
  pure Env{..}

-- | Main application monad.
newtype App a = App
  { runApp :: ReaderT Env IO a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader Env
    )

-- | Runs provided action with new 'Env'.
runWithNewEnv :: App a -> IO a
runWithNewEnv (runApp -> action) =
  buildEnv >>= flip usingReaderT action
{-# INLINE runWithNewEnv #-}

-- | Runs some 'App' action and logs possible errors.
runWithLog
  :: (MonadIO m) => Env -> App a -> m (Either (ErrorWithSource Error) a)
runWithLog env app = liftIO do
  appRes <- run app
  logRes <- whenLeft (Right ()) appRes logger
  pure $ appRes <* logRes
  where
    run :: App a -> IO (Either (ErrorWithSource Error) a)
    run = liftIO . try . usingReaderT env . runApp

    logger :: ErrorWithSource Error -> IO (Either (ErrorWithSource Error) ())
    logger = run . Logger.logMsg . Error.toLogMsg

-- | This instance allows to throw and catch errors that are visible in type
-- definitions. The implementation relies on underlying 'IO' machinery.
instance MonadError (ErrorWithSource Error) App where
  throwError :: ErrorWithSource Error -> App a
  throwError = liftIO . throwIO
  {-# INLINE throwError #-}

  catchError :: App a -> (ErrorWithSource Error -> App a) -> App a
  catchError action handler = App . ReaderT $ \env -> do
    let run = usingReaderT env . runApp
    run action `catch` \e -> run $ handler e
  {-# INLINE catchError #-}

-- | Instance to specify how to get and update a 'Logger' stored inside the 'Env'.
instance Logger.HasLog Env Logger.Log App where
  getLogAction :: Env -> Logger.Logger App
  getLogAction = eLogger
  {-# INLINE getLogAction #-}

  setLogAction :: Logger.Logger App -> Env -> Env
  setLogAction newLogAction env = env{eLogger = newLogAction}
  {-# INLINE setLogAction #-}
