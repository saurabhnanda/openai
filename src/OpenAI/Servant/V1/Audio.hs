-- | @/v1/audio@
module OpenAI.Servant.V1.Audio
    ( -- * API
      API
    ) where

import OpenAI.Servant.Prelude
import qualified OpenAI.Servant.V1.Audio.Speech as Speech

-- | API
type API = "audio" :> Speech.API
