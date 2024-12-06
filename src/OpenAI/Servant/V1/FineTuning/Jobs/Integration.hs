-- | Fine-tuning job integrations
module OpenAI.Servant.V1.FineTuning.Jobs.Integration
    ( -- * API
      WAndB(..)
    , Integration(..)
    ) where

import OpenAI.Servant.Prelude

-- | The settings for your integration with Weights and
data WAndB = WAndB
    { project :: Text
    , name :: Maybe Text
    , entity :: Maybe Text
    , tags :: Maybe (Vector Text)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | An integration to enable for your fine-tuning job
data Integration = Integration_WAndB{ wandb :: WAndB }
    deriving stock (Generic, Show)

integrationOptions :: Options
integrationOptions = aesonOptions
    { sumEncoding =
        TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

    , tagSingleConstructors = True

    , constructorTagModifier = stripPrefix "Integration_"
    }

instance FromJSON Integration where
    parseJSON = genericParseJSON integrationOptions

instance ToJSON Integration where
    toJSON = genericToJSON integrationOptions
