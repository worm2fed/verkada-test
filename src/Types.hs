module Types
  ( SumCommandReq (..)
  , SumCommandResp (..)
  ) where

import Relude

import Data.Aeson (FromJSON, ToJSON)

import Util.Json (CustomJSON (..), JsonCamelCase)

data SumCommandReq = SumCommandReq
  { cA :: Int
  , cB :: Int
  , cId :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON JsonCamelCase SumCommandReq

data SumCommandResp = SumCommandResp
  { scResult :: Int
  , scId :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON JsonCamelCase SumCommandResp
