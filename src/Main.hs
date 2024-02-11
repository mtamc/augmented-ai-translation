{-# LANGUAGE DeriveAnyClass, OverloadedRecordDot #-}
module Main (main) where

import GHC.IO.Encoding
import System.IO.CodePage
import Translation        (loadAndRunTask)

main ∷ IO ()
main = do
  setLocaleEncoding utf8
  withCodePage cp65001 loadAndRunTask
