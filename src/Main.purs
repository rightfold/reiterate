module Main
  ( main
  ) where

import Control.Monad.Free (Free, foldFree)
import DOM.HTML (window)
import DOM.HTML.Window (localStorage)
import Halogen.Aff.Util (awaitBody)
import Halogen.Component (hoist)
import Halogen.VDom.Driver (runUI)
import Reiterate.Topic.Algebra (Topics)
import Reiterate.Topic.Storage (runTopics)
import Reiterate.UI (ui)
import Stuff

main :: IOSync Unit
main = launchIO \ liftAff $
  runUI (hoist (runIO' \ run) ui) unit =<< awaitBody

run :: Free Topics ~> IO
run action = liftIOSync do
  storage <- liftEff $ localStorage =<< window
  foldFree (runTopics storage) action
