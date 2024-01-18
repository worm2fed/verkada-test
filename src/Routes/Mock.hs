module Routes.Mock
  ( MockRoutes
  , MockEndpoints (..)
  , mockEndpoints
  ) where

import Relude

import Servant.API
  ( Description
  , Get
  , JSON
  , NamedRoutes
  , Summary
  , (:-)
  , (:>)
  )
import Servant.Server.Generic (AsServerT)

import Application (WithEffects)
import Data.Aeson (FromJSON, ToJSON)
import Util.Json (CustomJSON (..), JsonCamelCase)

-- | Helper type to represent Mock API in terms of Servant.
type MockRoutes = "mock" :> NamedRoutes MockEndpoints

-- | Represents API related to Mock.
newtype MockEndpoints mode = MockEndpoints
  { _mock
      :: mode
        :- Summary "Just a mock"
          :> Description
              "Mock endpoint description"
          :> Get '[JSON] MockResult
  }
  deriving stock (Generic)

-- | Endpoints related to Mock.
mockEndpoints :: (WithEffects m) => MockEndpoints (AsServerT m)
mockEndpoints =
  MockEndpoints
    { _mock = pure $ MockResult "Hello"
    }

newtype MockResult = MockResult
  { mrGreeting :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON JsonCamelCase MockResult
