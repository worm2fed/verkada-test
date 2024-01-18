-- | This module provides tools to work with JSON.
module Util.Json
  ( -- * Helpers to create JSON instances
    JsonCamelCase
  , JsonSnakeCase
  , jsonSumTypeOptions

    -- * re-export
  , AesonOptions (..)
  , CustomJSON (..)
  ) where

import Relude

import Data.Aeson
  ( Options (..)
  , SumEncoding (..)
  , defaultOptions
  )
import Data.Char (isUpper, toLower)
import Deriving.Aeson
  ( AesonOptions (..)
  , CamelToSnake
  , CustomJSON (..)
  , FieldLabelModifier
  , OmitNothingFields
  , StringModifier (..)
  )

-- | Apply function to first 'Char' in 'String'.
applyFirst :: (Char -> Char) -> String -> String
applyFirst _ [] = []
applyFirst f [x] = [f x]
applyFirst f (x : xs) = f x : xs

-- | 'StringModifier' for camelCase.
data CamelCase

instance StringModifier CamelCase where
  getStringModifier :: String -> String
  getStringModifier = applyFirst toLower
  {-# INLINE getStringModifier #-}

-- | 'StringModifier' to strip whole lowercase prefix.
data StripLowerPrefix

instance StringModifier StripLowerPrefix where
  getStringModifier :: String -> String
  getStringModifier = dropWhile (not . isUpper)
  {-# INLINE getStringModifier #-}

-- | Options for Union JSON data types in camelCase.
type JsonCamelCase =
  '[FieldLabelModifier '[StripLowerPrefix, CamelCase]]

-- | Options for Union JSON data types in snake_case.
type JsonSnakeCase =
  '[OmitNothingFields, FieldLabelModifier '[StripLowerPrefix, CamelToSnake]]

-- | Options for JSON sum types.
jsonSumTypeOptions :: Options
jsonSumTypeOptions =
  defaultOptions
    { sumEncoding = UntaggedValue
    , constructorTagModifier = map toLower
    }
