{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Data.Proxy (Proxy(..))
import OpenAI.Servant.V1.Audio.Speech (Request(..))
import Servant.Client (ClientM)

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP.Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified OpenAI.Servant.V1 as V1
import qualified OpenAI.Servant.V1.Audio.Speech as Speech
import qualified Servant.Client as Client
import qualified System.Environment as Environment
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

    let authorization = "Bearer " <> Text.pack key

    let v1AudioSpeech = Client.client (Proxy @V1.API) authorization

    let run :: ClientM a -> IO a
        run clientM = do
            result <- Client.runClientM clientM clientEnv
            case result of
                Left clientError -> Exception.throwIO clientError
                Right a          -> return a

    let v1AudioSpeechTest voice format = HUnit.testCase name do
            run do
                _ <- v1AudioSpeech Speech.Request
                    { model = "tts-1"
                    , input = "Hello, world!"
                    , voice
                    , response_format = Just format
                    , speed = Just 1.0
                    }

                return ()
          where
            name = "/v1/audio/speech - " <> show voice <> " - " <> show format

    let v1AudioSpeechTests = do
            format <- [ minBound .. maxBound ]
            return (v1AudioSpeechTest Speech.Nova format)

    let tests = v1AudioSpeechTests

    Tasty.defaultMain (Tasty.testGroup "Tests" tests)
