-- | @\/v1\/chat@
module OpenAI.Servant.V1.Chat
    ( -- * API
      API
    ) where

import OpenAI.Servant.Prelude
import qualified OpenAI.Servant.V1.Chat.Completions as Completions

-- | API
type API = "chat" :> Completions.API
