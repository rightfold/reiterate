module Reiterate.UI
  ( Query
  , Input
  , Output
  , ui
  ) where

import Halogen.Component (Component, ComponentDSL, ComponentHTML, lifecycleComponent)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Reiterate.Topic (Topic)
import Stuff

type State = Error \/ List Topic
data Query a
  = Refresh a
type Input = Unit
type Output = Void

ui :: ∀ m. Component HTML Query Input Output m
ui = lifecycleComponent {initialState, receiver, initializer, finalizer, eval, render}

initialState :: Input -> State
initialState = const \ Right $ Nil

receiver :: Input -> Maybe (Query Unit)
receiver = const Nothing

initializer :: Maybe (Query Unit)
initializer = Just $ Refresh unit

finalizer :: Maybe (Query Unit)
finalizer = Nothing

eval :: ∀ m. Query ~> ComponentDSL State Query Output m
eval (Refresh next) = pure next

render :: State -> ComponentHTML Query
render = H.text \ show
