-- | @\/v1\/files@
module OpenAI.Servant.V1.Files
    ( -- * API
      Request(..)
    , API
    ) where

import OpenAI.Servant.Prelude
import OpenAI.Servant.V1.Files.File
import OpenAI.Servant.V1.Files.Purpose as Purpose

import qualified Data.Text as Text

-- | Request body
data Request = Request
    { file :: FilePath
    , purpose :: Purpose
    } deriving stock (Generic, Show)

instance ToMultipart Tmp Request where
    toMultipart Request{..} = MultipartData{..}
      where
        inputs = input "purpose" (Purpose.toText purpose)

        files = [ FileData{..} ]
          where
            fdInputName = "file"
            fdFileName = Text.pack file
            fdFileCType = "application/json"
            fdPayload = file

-- | API
type API = "files" :> MultipartForm Tmp Request :> Post '[JSON] File
