module Routes.Task
  ( TaskRoutes
  , TaskEndpoints (..)
  , taskEndpoints
  ) where

import Relude

import Data.Map.Strict qualified as M
import Servant.API
  ( Get
  , JSON
  , NamedRoutes
  , NoContent (..)
  , Post
  , QueryParam
  , ReqBody
  , (:-)
  , (:>)
  )
import Servant.Server.Generic (AsServerT)

import Application (Env (..), WithEffects)
import Colog qualified as Logger
import Control.Concurrent (threadDelay)
import Data.Sequence (ViewL (..), viewl, (|>))
import Error (Error (..), throwError)
import System.Random (randomIO)
import Types (SumCommandReq (..), SumCommandResp (..))

-- | Helper type to represent Task API in terms of Servant.
type TaskRoutes = "mock" :> NamedRoutes TaskEndpoints

-- | Represents API related to Task.
data TaskEndpoints mode = TaskEndpoints
  { _pollOperations
      :: mode
        :- "poll_for_command"
          :> Get '[JSON] SumCommandReq
  , _submitSumOperation
      :: mode
        :- "sum"
          :> QueryParam "a" Int
          :> QueryParam "b" Int
          :> Get '[JSON] SumCommandResp
  , _sendResponse
      :: mode
        :- "send_response"
          :> ReqBody '[JSON] SumCommandResp
          :> Post '[JSON] NoContent
  }
  deriving stock (Generic)

-- | Endpoints related to Task.
taskEndpoints :: (WithEffects m) => TaskEndpoints (AsServerT m)
taskEndpoints =
  TaskEndpoints
    { _pollOperations = pollOperationsHandler
    , _submitSumOperation = submitSumOperationHandler
    , _sendResponse = sendResponseHandler
    }

pollOperationsHandler :: (WithEffects m) => m SumCommandReq
pollOperationsHandler = do
  queueVar <- asks eQueue
  poll queueVar
  where
    poll :: (MonadIO m) => TVar (Seq SumCommandReq) -> m SumCommandReq
    poll queueVar = do
      queue <- readTVarIO queueVar
      case viewl queue of
        EmptyL -> do
          liftIO $ threadDelay $ 1 * 1000000
          poll queueVar
        operation :< _ -> pure operation

submitSumOperationHandler
  :: (WithEffects m) => Maybe Int -> Maybe Int -> m SumCommandResp
submitSumOperationHandler (Just cA) (Just cB) = do
  queueVar <- asks eQueue
  cId <- randomIO
  atomically $ modifyTVar' queueVar (|> SumCommandReq{..})
  responsesVar <- asks eResponses
  poll cId responsesVar
  where
    poll :: (MonadIO m) => Int -> TVar (Map Int SumCommandResp) -> m SumCommandResp
    poll cId responsesVar = do
      responses <- readTVarIO responsesVar
      case M.lookup cId responses of
        Nothing -> do
          liftIO $ threadDelay $ 1 * 1000000
          poll cId responsesVar
        Just response -> pure response
submitSumOperationHandler _ _ = throwError Logger.Error $ BadRequest "..."

sendResponseHandler :: (WithEffects m) => SumCommandResp -> m NoContent
sendResponseHandler resp@SumCommandResp{..} = do
  responsesVar <- asks eResponses
  atomically $ modifyTVar' responsesVar (M.insert scId resp)
  pure NoContent
