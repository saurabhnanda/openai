-- | @\/v1\/fine_tuning@
module OpenAI.Servant.V1.FineTuning
    ( -- * API
      API
    ) where

import OpenAI.Servant.Prelude
import qualified OpenAI.Servant.V1.FineTuning.Jobs as Jobs

-- | API
type API = "fine_tuning" :> Jobs.API
