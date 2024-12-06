-- | @\/v1\/fine_tuning/jobs@
module OpenAI.Servant.V1.FineTuning.Jobs
    ( -- * API
      Request(..)
    , API
    ) where

import OpenAI.Servant.Prelude
import OpenAI.Servant.V1.FineTuning.Jobs.Hyperparameters
import OpenAI.Servant.V1.FineTuning.Jobs.Integration
import OpenAI.Servant.V1.FineTuning.Jobs.Job

import qualified OpenAI.Servant.V1.FineTuning.Jobs.Id as Id

-- | Request body
data Request = Request
    { model :: Text
    , training_file :: Text
    , hyperparameters :: Maybe Hyperparameters
    , suffix :: Maybe Text
    , validation_file :: Maybe Text
    , integrations :: Maybe (Vector Integration)
    , seed :: Maybe Integer
    } deriving stock (Generic, Show)
      deriving anyclass (ToJSON)

-- | API
type API =
        "jobs"
    :>  (     ReqBody '[JSON] Request :> Post '[JSON] Job
        :<|>  Id.API
        )
