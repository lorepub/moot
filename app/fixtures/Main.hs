module Main where

import Prelude (IO)
import Model (runDevDB)
import Model.Fixtures (wipeAndReinstallFixtures)

main :: IO ()
main =
  runDevDB wipeAndReinstallFixtures
