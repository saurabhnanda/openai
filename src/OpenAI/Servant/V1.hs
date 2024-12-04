-- | @/v1@
module OpenAI.Servant.V1
    ( -- * API
      API
    ) where

import OpenAI.Servant.Prelude
import Servant.API (Header', Required, Strict)

import qualified OpenAI.Servant.V1.Audio as Audio

-- | API
type API
    = Header' [ Required, Strict ] "Authorization" Text
    :> "v1" :> Audio.API
