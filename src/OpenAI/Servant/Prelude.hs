module OpenAI.Servant.Prelude
    ( -- * JSON
      aesonOptions

      -- * Multipart Form Data
    , input
    , renderDouble
    , getExtension

      -- * Re-exports
    , module Data.Aeson
    , module Data.ByteString.Lazy
    , module Data.List.NonEmpty
    , module Data.Text
    , module Data.Vector
    , module GHC.Generics
    , module Servant.API
    , module Servant.Multipart.API
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Options(..), genericToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Servant.API
    ( Accept(..)
    , JSON
    , MimeUnrender(..)
    , OctetStream
    , Post
    , ReqBody
    , (:<|>)(..)
    , (:>)
    )
import Servant.Multipart.API
    ( FileData(..)
    , Input(..)
    , MultipartData(..)
    , MultipartForm
    , ToMultipart(..)
    , Tmp
    )

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.RealFloat as RealFloat
import qualified System.FilePath as FilePath

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

input :: Text -> Text -> [ Input ]
input iName iValue = [ Input{..} ]

renderDouble :: Double -> Text
renderDouble double = Text.Lazy.toStrict (Builder.toLazyText builder)
  where
    builder = RealFloat.formatRealFloat RealFloat.Fixed Nothing double

getExtension :: FilePath -> Text
getExtension file = Text.pack (drop 1 (FilePath.takeExtension file))
