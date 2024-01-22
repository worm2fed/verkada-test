module Server
  ( server
  ) where

import Relude

import Control.Monad.Except (liftEither)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant.API (NamedRoutes, (:-))
import Servant.Server (Handler, hoistServer, serve)
import Servant.Server.Generic (AsServerT)

import Application (App, Env, WithEffects, runWithLog, runWithNewEnv)
import Error (ErrorWithSource (..), toHttpError)
import Logger qualified
import Routes.Task (TaskRoutes, taskEndpoints)

-- | Web server.
server :: (HasCallStack) => IO ()
server = runWithNewEnv $ do
  env <- ask
  let port = 8080
  let api = Proxy @API
  Logger.logInfo $ "Starting server on port: " <> show port
  liftIO
    . run port
    . logStdoutDev
    . serve api
    $ hoistServer api (toHandler env) routes
  where
    toHandler :: Env -> App a -> Handler a
    toHandler env app =
      runWithLog env app >>= liftEither . first (toHttpError . ewsError)

-- | Helper type to represent Union API in terms of Servant.
type API = NamedRoutes Routes

-- | Represents combination of all routes, available in Union.
newtype Routes mode = Routes
  { rTask :: mode :- TaskRoutes
  }
  deriving stock (Generic)

-- | Represents combination of all routes, available in Union.
routes :: (WithEffects m) => Routes (AsServerT m)
routes =
  Routes
    { rTask = taskEndpoints
    }
