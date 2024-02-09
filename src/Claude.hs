{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Claude (Prompt (..), countTokens, getCompletion) where

import Data.Aeson                (FromJSON, KeyValue ((.=)), encode, object)
import Data.Time.Clock.POSIX     (getPOSIXTime)
import Network.HTTP.Client       (responseTimeoutMicro)
import Network.HTTP.Simple       (addRequestHeader, getResponseBody, httpJSONEither, parseRequest_,
                                  setRequestBodyLBS, setRequestResponseTimeout)
import Network.HTTP.Types.Header (hContentType)
import Relude.Unsafe             qualified as Unsafe
import Secrets                   (claudeKey)
import Text.Show qualified

{-| This only supports a single Human message, as this is all that is needed for this app. -}
data Prompt
  = Prompt
    { instruction ∷ Text
    , prefill     ∷ Text
    }
  deriving (Eq, Generic)

instance Show Prompt where
  show (Prompt instruction prefill)
    = toString
    $ "\n\nHuman: "     ⊕ instruction
    ⊕ "\n\nAssistant: " ⊕ prefill

getCompletion ∷ Prompt → IO Text
getCompletion prompt = do
  now ← show <$> getPOSIXTime
  resp ← "POST https://api.anthropic.com/v1/complete"
    & parseRequest_
    & addRequestHeader "x-api-key" claudeKey
    & addRequestHeader hContentType "application/json"
    & addRequestHeader "anthropic-version" "2023-06-01"
    & setRequestBodyLBS
      (encode $ object
        [ "max_tokens_to_sample" .= (4000 ∷ Int)
        , "temperature" .= (0.4 ∷ Float)
        , "model" .= ("claude-2" ∷ String)
        , "stream" .= False
        , "prompt" .= (show prompt ∷ String)
        ]
       )
    & setRequestResponseTimeout (responseTimeoutMicro 300_000_000)
    & httpJSONEither
  case getResponseBody resp of
    Left err                 → print err ≫ error ""
    Right ((Completion { completion })) → do
      writeFileText ("./playground/prompts/" ⊕ now)
        (show prompt ⊕ "\n\n#######\n\n" ⊕ completion)
      pure completion

countTokens ∷ Text → Int
countTokens input = Unsafe.read @Int (toString (getStdOut "node" ["./js/countTokens.js", input]))


-- PRIVATE

data Completion
  = Completion
    { completion ∷ Text
    }
  deriving (Eq, FromJSON, Generic, Show)
