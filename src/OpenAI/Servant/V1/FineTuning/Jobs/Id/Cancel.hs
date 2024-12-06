-- | @\/v1\/fine_tuning\/jobs\/:id\/cancel@
module OpenAI.Servant.V1.FineTuning.Jobs.Id.Cancel
    ( -- * API
      API
    ) where

import OpenAI.Servant.Prelude
import OpenAI.Servant.V1.FineTuning.Jobs.Job

-- | API
type API = "cancel" :> Post '[JSON] Job
