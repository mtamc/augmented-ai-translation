{-# LANGUAGE DeriveAnyClass, OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Secrets where

import Data.Aeson (FromJSON, ToJSON)
import GHC.IO     (unsafePerformIO)

data Secrets
  = Secrets
    { anthropicKey   ∷ Text
    , wordpressToken ∷ Text
    }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

{-# NOINLINE secretFile #-}
secretFile ∷ Secrets
secretFile = unsafePerformIO $ decodeFile "./playground/secrets.json"

claudeKey ∷ ByteString
claudeKey = encodeUtf8 secretFile.anthropicKey

wpTok ∷ ByteString
wpTok = encodeUtf8 secretFile.wordpressToken
