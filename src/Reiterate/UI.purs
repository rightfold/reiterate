module Reiterate.UI
  ( Query
  , Input
  , Output
  , Base
  , ui
  ) where

import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Data.Lens ((.=))
import Halogen.Component (Component, ComponentDSL, ComponentHTML, lifecycleComponent)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Reiterate.Topic (Topic, TopicID)
import Reiterate.Topic.Algebra (Topics, freshTopic, getTopics)
import Stuff

type State = Error \/ List (TopicID /\ Topic)
data Query a
  = Refresh a
  | NewTopic a
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
eval (Refresh next) = do
  topics <- lift \ unwrap $ getTopics
  id .= topics
  pure next
eval (NewTopic next) = do
  tid <- lift \ unwrap $ freshTopic
  eval $ Refresh next

render :: State -> ComponentHTML Query
render s =
  H.div []
    [ H.button [E.onClick \ E.input_ $ NewTopic]
        [H.text "New Topic"]
    , H.text $ show s
    ]
