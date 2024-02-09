{-# LANGUAGE DeriveAnyClass, OverloadedRecordDot #-}
module Main (main) where

import Translation (loadAndRunTask)

main ∷ IO ()
main =
  loadAndRunTask
