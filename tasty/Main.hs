{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Data.Proxy (Proxy(..))
import Servant.API ((:<|>)(..))
import Servant.Client (ClientM)

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP.Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified OpenAI.Servant.V1 as V1
import qualified OpenAI.Servant.V1.Audio.Speech as Speech
import qualified OpenAI.Servant.V1.Audio.Transcriptions as Transcriptions
import qualified Servant.Client as Client
import qualified System.Environment as Environment
import qualified Servant.Multipart.Client as Multipart.Client
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

main :: IO ()
main = do
    let managerSettings = TLS.tlsManagerSettings
            { HTTP.Client.managerResponseTimeout =
                HTTP.Client.responseTimeoutNone
            }

    manager <- TLS.newTlsManagerWith managerSettings

    baseUrl <- Client.parseBaseUrl "https://api.openai.com"

    let clientEnv = Client.mkClientEnv manager baseUrl

    key <- Environment.getEnv "OPENAI_KEY"

    boundary <- Multipart.Client.genBoundary

    let authorization = "Bearer " <> Text.pack key

    let (v1AudioSpeech :<|> v1AudioTranscriptions) = Client.client (Proxy @V1.API) authorization

    let run :: ClientM a -> IO a
        run clientM = do
            result <- Client.runClientM clientM clientEnv
            case result of
                Left clientError -> Exception.throwIO clientError
                Right a          -> return a

    -- Test each format to make sure we're handling each possible content type
    -- correctly
    let v1AudioSpeechTest format = HUnit.testCase name do
            run do
                _ <- v1AudioSpeech Speech.Request
                    { Speech.model = "tts-1"
                    , Speech.input = "Hello, world!"
                    , Speech.voice = Speech.Nova
                    , Speech.response_format = Just format
                    , Speech.speed = Just 1.0
                    }

                return ()
          where
            name = "/v1/audio/speech - " <> show format

    let v1AudioTranscriptionsTest =
            HUnit.testCase "/v1/audio/transcriptions" do
                run do
                    _ <- v1AudioTranscriptions
                        ( boundary
                        , Transcriptions.Request
                            { Transcriptions.file =
                                "tasty/data/v1/audio/transcriptions.wav"
                            , Transcriptions.model =
                                "whisper-1"
                            , Transcriptions.language =
                                Just "en"
                            , Transcriptions.prompt =
                                Nothing
                            , Transcriptions.temperature =
                                Just 0
                            }
                        )

                    return ()

    let v1AudioSpeechTests = do
            format <- [ minBound .. maxBound ]
            return (v1AudioSpeechTest format)

    let tests =
                v1AudioSpeechTests
            <>  [ v1AudioTranscriptionsTest ]

    Tasty.defaultMain (Tasty.testGroup "Tests" tests)
