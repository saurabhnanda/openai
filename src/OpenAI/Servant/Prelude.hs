module OpenAI.Servant.Prelude
    ( -- * JSON
      aesonOptions

      -- * Re-exports
    , module Data.Aeson
    , module Data.ByteString.Lazy
    , module Data.List.NonEmpty
    , module Data.Text
    , module GHC.Generics
    , module Servant.API
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Options(..), genericToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Servant.API
    ( Accept(..)
    , JSON
    , MimeUnrender(..)
    , OctetStream
    , Post
    , ReqBody
    , (:>)
    )

import qualified Data.Aeson as Aeson

dropTrailingUnderscore :: String -> String
dropTrailingUnderscore "_" = ""
dropTrailingUnderscore ""  = ""
dropTrailingUnderscore (c : cs) = c : dropTrailingUnderscore cs

aesonOptions :: Options
aesonOptions = Aeson.defaultOptions
    { fieldLabelModifier = dropTrailingUnderscore
    , constructorTagModifier = map toLower
    , omitNothingFields = True
    }
