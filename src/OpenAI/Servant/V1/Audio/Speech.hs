-- | @/v1/audio/speech@
module OpenAI.Servant.V1.Audio.Speech
    ( -- * API
      Voice(..)
    , Format(..)
    , Request(..)
    , Response
    , ContentType(..)
    , API
    ) where

import OpenAI.Servant.Prelude

-- | The voice to use when generating the audio
--
-- Previews of the voices are available in the
-- [Text to speech guide](https://platform.openai.com/docs/guides/text-to-speech#voice-options).
data Voice = Alloy | Echo | Fable | Onyx | Nova | Shimmer
    deriving stock (Bounded, Enum, Generic, Show)

instance ToJSON Voice where
    toJSON = genericToJSON aesonOptions

-- | The format to audio in
data Format = MP3 | Opus | AAC | Flac | Wav
    deriving stock (Bounded, Enum, Generic, Show)

instance ToJSON Format where
    toJSON = genericToJSON aesonOptions

-- | Request body
data Request = Request
    { model :: Text
      -- ^ One of the available
      -- [TTS models](https://platform.openai.com/docs/models#tts)
    , input :: Text
      -- ^ The text to generate audio for. The maximum length is 4096
      -- characters.
    , voice :: Voice
      -- ^
    , response_format :: Maybe Format
      -- ^ Defaults to `MP3`
    , speed :: Maybe Double
      -- ^ The speed of the generated audio. Select a value from 0.25 to 4.0.
      -- 1.0 is the default.
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

-- | Response body
type Response = ByteString

-- | Content type
data ContentType = ContentType

instance Accept ContentType where
    contentTypes _ =
            "audio/mpeg"
        :|  [ "audio/flac", "audio/wav", "audio/aac", "audio/opus" ]

instance MimeUnrender ContentType ByteString where
    mimeUnrender _ bytes = Right bytes

-- | API
type API = "speech" :> ReqBody '[JSON] Request :> Post '[ContentType] Response
