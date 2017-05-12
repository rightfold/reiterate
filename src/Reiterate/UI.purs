module Reiterate.UI
  ( Query
  , Input
  , Output
  , Base
  , ui
  ) where

import Control.Monad.Free (Free)
import Control.Monad.State.Class as State
import Control.Monad.Trans.Class (lift)
import Halogen.Component (Component, ComponentDSL, ComponentHTML, lifecycleComponent)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Reiterate.Topic (Topic)
import Reiterate.Topic.Algebra (Topics, getTopics)
import Stuff

type State = Error \/ List Topic
data Query a
  = Refresh a
type Input = Unit
type Output = Void
type Base = Free Topics

ui :: Component HTML Query Input Output Base
ui = lifecycleComponent {initialState, receiver, initializer, finalizer, eval, render}

initialState :: Input -> State
initialState = const \ Right $ Nil

receiver :: Input -> Maybe (Query Unit)
receiver = const Nothing

initializer :: Maybe (Query Unit)
initializer = Just $ Refresh unit

finalizer :: Maybe (Query Unit)
finalizer = Nothing

eval :: Query ~> ComponentDSL State Query Output Base
eval (Refresh next) = next <$ (State.put =<< lift (unwrap getTopics))

render :: State -> ComponentHTML Query
render = H.text \ show
