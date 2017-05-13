module Reiterate.UI
  ( Query
  , Input
  , Output
  , Base
  , ui
  ) where

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Data.Lens (Lens', (.=), (?=), modifying)
import Data.Lens.Record (prop)
import Halogen.Component (Component, ComponentDSL, ComponentHTML, lifecycleComponent)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Reiterate.Topic (Topic, TopicID)
import Reiterate.Topic.Algebra (Topics, freshTopic, getTopics)
import Stuff

--------------------------------------------------------------------------------

type State =
  { errors :: List Error
  , topics :: List (TopicID /\ Topic)
  , selection :: Maybe TopicID
  }
data Query a
  = Refresh a
  | NewTopic a
type Input = Unit
type Output = Void
type Base = Free Topics

stateErrors :: Lens' State (List Error)
stateErrors = prop (SProxy :: SProxy "errors")

stateTopics :: Lens' State (List (TopicID /\ Topic))
stateTopics = prop (SProxy :: SProxy "topics")

stateSelection :: Lens' State (Maybe TopicID)
stateSelection = prop (SProxy :: SProxy "selection")

--------------------------------------------------------------------------------

ui :: Component HTML Query Input Output Base
ui = lifecycleComponent {initialState, receiver, initializer, finalizer, eval, render}

initialState :: Input -> State
initialState = const $ {errors: Nil, topics: Nil, selection: Nothing}

receiver :: Input -> Maybe (Query Unit)
receiver = const Nothing

initializer :: Maybe (Query Unit)
initializer = Just $ Refresh unit

finalizer :: Maybe (Query Unit)
finalizer = Nothing

eval :: Query ~> ComponentDSL State Query Output Base
eval (Refresh next) = next <$ try (stateTopics .= _) getTopics
eval (NewTopic next) = try (stateSelection ?= _) freshTopic *> eval (Refresh next)

try :: ∀ a q o m. Monad m => (a -> ComponentDSL State q o m Unit) ->
  ExceptT Error m a -> ComponentDSL State q o m Unit
try f = either (modifying stateErrors \ (:)) f <=< lift \ unwrap

render :: State -> ComponentHTML Query
render s =
  H.div []
    [ H.button [E.onClick \ E.input_ $ NewTopic]
        [H.text "New Topic"]
    , H.pre [] [H.text $ show s.errors]
    , H.pre [] [H.text $ show s.topics]
    , H.pre [] [H.text $ show s.selection]
    ]
