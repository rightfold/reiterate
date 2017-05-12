module Main
  ( main
  ) where

import Halogen.Aff.Util (awaitBody)
import Halogen.VDom.Driver (runUI)
import Reiterate.UI (ui)
import Stuff

main :: IOSync Unit
main = launchIO \ liftAff $
  runUI ui unit =<< awaitBody
