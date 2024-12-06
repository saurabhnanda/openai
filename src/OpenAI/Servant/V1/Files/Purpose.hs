-- | The purpose for an uploaded file
module OpenAI.Servant.V1.Files.Purpose
    ( -- * API
      Purpose(..)
    , toText
    ) where

import OpenAI.Servant.Prelude

-- | The intended purpose of the uploaded file.
data Purpose
    = Assistants
    | Assistants_Output
    | Batch
    | Batch_Output
    | Fine_Tune
    | Fine_Tune_Results
    | Vision
    deriving stock (Generic, Show)

instance FromJSON Purpose where
    parseJSON = genericParseJSON aesonOptions
        { constructorTagModifier = fix . labelModifier
        }
      where
        fix "fine_tune" = "fine-tune"
        fix "fine_tune_results" = "fine-tune-results"
        fix string = string

-- | Convert a `Purpose` to its equivalent `Text` representation
toText :: Purpose -> Text
toText Assistants = "assistants"
toText Assistants_Output = "assistants"
toText Batch = "batch"
toText Batch_Output = "batch_output"
toText Fine_Tune = "fine-tune"
toText Fine_Tune_Results = "fine-tune-results"
toText Vision = "vision"
