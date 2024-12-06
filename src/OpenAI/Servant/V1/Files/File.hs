-- | The file object
module OpenAI.Servant.V1.Files.File
    ( -- * API
      File(..)
    ) where

import OpenAI.Servant.Prelude
import OpenAI.Servant.V1.Files.Purpose

-- | The `File` object represents a document that has been uploaded to OpenAI
data File = File
    { id :: Text
    , bytes :: Natural
    , created_at :: POSIXTime
    , filename :: Text
    , object :: Text
    , purpose :: Purpose
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)
