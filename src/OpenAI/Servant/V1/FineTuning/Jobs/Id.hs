-- | @\/v1\/fine_tuning\/jobs\/:id\@
module OpenAI.Servant.V1.FineTuning.Jobs.Id
    ( -- * API
      API
    ) where

import OpenAI.Servant.Prelude

import qualified OpenAI.Servant.V1.FineTuning.Jobs.Id.Cancel as Cancel

-- | API
type API = Capture "fine_tuning_job_id" Text :> Cancel.API
