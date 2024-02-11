module Prelude
  ( module Control.Arrow.Unicode
  , module Control.Exception.Safe
  , module Control.Monad.Except
  , module Control.Monad.Unicode
  , module Data.Bool.Unicode
  , module Data.Eq.Unicode
  , module Data.List.Unicode
  , module Data.Monoid.Unicode
  , module Data.Ord.Unicode
  , module Relude
  , applyWhen
  , decodeFile
  , echo
  , getStdOut
  , justIf
  , positJust
  , writeFileText
  , (≪)
  ) where

import Control.Arrow.Unicode
import Control.Exception.Safe (MonadCatch, MonadThrow, catchAny)
import Control.Monad.Except
import Control.Monad.Unicode
import Data.Aeson             (FromJSON, eitherDecodeFileStrict)
import Data.Bool.Unicode
import Data.Eq.Unicode
import Data.Generics.Labels   ()
import Data.List.Unicode
import Data.Monoid.Unicode
import Data.Ord.Unicode
import Data.Text              qualified as Text
import GHC.IO                 (unsafePerformIO)
import GHC.IO.Encoding        (utf8)
import Relude                 hiding (writeFileText)
import System.Exit            (ExitCode (ExitFailure, ExitSuccess))
import System.IO              (hPutStr, hSetEncoding)
import System.Process         (readProcessWithExitCode)

----------------------------------------------------------------------------------------------------

-- | Short name for putTextLn
echo ∷ MonadIO m ⇒ Text → m ()
echo = putTextLn

applyWhen ∷ Bool → (a → a) → a → a
applyWhen cond f a = if cond then f a else a

justIf ∷ Bool → a → Maybe a
justIf cond a = if cond then Just a else Nothing

positJust ∷ MonadError e m ⇒ e → Maybe a → m a
positJust _err (Just a) = pure a
positJust err  Nothing  = throwError err

(≪) ∷ Monad m ⇒ m a → m b → m a
(≪) = flip (≫)

decodeFile ∷ FromJSON a ⇒ FilePath → IO a
decodeFile f = eitherDecodeFileStrict f ≫= \case
  Right a  → pure a
  Left err → print (err ⊕ "\n" ⊕ f) ≫ error (show err ⊕ "\n" ⊕ show f)

{-# NOINLINE getStdOut #-}
getStdOut ∷ Text → [Text] → Text
getStdOut process args = unsafePerformIO do
  (exitStatus, out, _) ← readProcessWithExitCode (toString process) (map toString args) ""
  case exitStatus of
    ExitSuccess   → pure $ toText out
    ExitFailure _ → error ("failed to run: " ⊕ process ⊕ " " ⊕ show args)

writeFileText ∷ FilePath → Text → IO ()
writeFileText path text = withFile path WriteMode $ \handle → do
  hSetEncoding handle utf8
  hPutStr handle (Text.unpack text)
