-- | The fine-tuning hyperparameters
module OpenAI.Servant.V1.FineTuning.Jobs.Hyperparameters
    ( -- * API
      AutoOr(..)
    , Hyperparameters(..)
    ) where

import OpenAI.Servant.Prelude

-- | A type that can also be the string @\"auto\"@
data AutoOr a = Auto | Specific a
    deriving stock (Generic, Show)

instance FromJSON a => FromJSON (AutoOr a) where
    parseJSON "auto" = pure Auto
    parseJSON value = fmap Specific (parseJSON value)

instance ToJSON a => ToJSON (AutoOr a) where
    toJSON Auto = "auto"
    toJSON (Specific a) = toJSON a

-- | The hyperparameters used for the fine-tuning job
data Hyperparameters = Hyperparameters
    { batch_size :: Maybe (AutoOr Natural)
    , learning_rate_multiplier :: Maybe (AutoOr Double)
    , n_epochs :: Maybe (AutoOr Natural)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)
