module Main where

import Prelude (IO)
import Model (runDevDB)
import Model.Fixtures (truncateAllTables)

main :: IO ()
main =
  runDevDB truncateAllTables
