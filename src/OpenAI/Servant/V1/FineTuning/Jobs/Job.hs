-- | The fine-tuning job object
module OpenAI.Servant.V1.FineTuning.Jobs.Job
    ( -- * API
      Error(..)
    , Status(..)
    , Job(..)
    ) where

import OpenAI.Servant.Prelude
import OpenAI.Servant.V1.FineTuning.Jobs.Hyperparameters
import OpenAI.Servant.V1.FineTuning.Jobs.Integration

-- | For fine-tuning jobs that have @failed@, this will contain more
-- information on the cause of the failure.
--
-- NOTE: OpenAPI API's says that the `code` and `message` fields are required,
-- but in practice the `Error` record can be present with all fields omitted,
-- so they are all marked optional (`Maybe`) here
data Error = Error
    { code :: Maybe Text
    , message :: Maybe Text
    , param :: Maybe Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

-- | The current status of the fine-tuning job
data Status
    = Validating_Files
    | Queued
    | Running
    | Succeeded
    | Failed
    | Cancelled
    deriving stock (Generic, Show)

instance FromJSON Status where
    parseJSON = genericParseJSON aesonOptions

-- | The fine_tuning.job object represents a fine-tuning job that has been
-- created through the API.
data Job = Job
    { id :: Text
    , created_at :: POSIXTime
    , error :: Maybe Error
    , fine_tuned_model :: Maybe Text
    , finished_at :: Maybe POSIXTime
    , hyperparameters :: Hyperparameters
    , model :: Text
    , object :: Text
    , organization_id :: Text
    , result_files :: Vector Text
    , status :: Status
    , trained_tokens :: Maybe Natural
    , training_file :: Text
    , validation_file :: Maybe Text
    , integrations :: Maybe (Vector Integration)
    , seed :: Integer
    , estimated_finish :: Maybe POSIXTime
    }
    deriving stock (Generic, Show)

instance FromJSON Job where
    parseJSON = genericParseJSON aesonOptions
