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
import Data.Lens (Lens', (%=), (.=))
import Data.Lens.Record (prop)
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Validation.Semigroup (V, invalid, unV)
import Halogen.Component (Component, ComponentDSL, ComponentHTML, lifecycleComponent)
import Halogen.Form (Form')
import Halogen.Form as F
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Reiterate.Topic (Phase(..), Topic(..), TopicID)
import Reiterate.Topic.Algebra (Topics, freshTopic, getTopics)
import Stuff

--------------------------------------------------------------------------------

type State =
  { errors :: List Error
  , topics :: Map TopicID Topic
  , editing :: Editing
  }
data Query a
  = Refresh a
  | NewTopic a
  | SetEditing Editing a
type Input = Unit
type Output = Void
type Base = Free Topics

type Editing = EditingRaw /\ V (NonEmptyList Unit) Topic
type EditingRaw = String /\ Boolean

stateErrors :: Lens' State (List Error)
stateErrors = prop (SProxy :: SProxy "errors")

stateTopics :: Lens' State (Map TopicID Topic)
stateTopics = prop (SProxy :: SProxy "topics")

stateEditing :: Lens' State Editing
stateEditing = prop (SProxy :: SProxy "editing")

--------------------------------------------------------------------------------

ui :: Component HTML Query Input Output Base
ui = lifecycleComponent {initialState, receiver, initializer, finalizer, eval, render}

initialState :: Input -> State
initialState = const
  { errors: Nil
  , topics: Map.empty
  , editing: ("" /\ false) /\ invalid (pure unit)
  }

receiver :: Input -> Maybe (Query Unit)
receiver = const Nothing

initializer :: Maybe (Query Unit)
initializer = Just $ Refresh unit

finalizer :: Maybe (Query Unit)
finalizer = Nothing

eval :: Query ~> ComponentDSL State Query Output Base
eval (Refresh next) = next <$ try (stateTopics .= _) getTopics
eval (NewTopic next) = try (const $ pure unit) freshTopic *> eval (Refresh next)
eval (SetEditing p next) = next <$ (stateEditing .= p)

try :: ∀ a q o m. Monad m => (a -> ComponentDSL State q o m Unit) ->
  ExceptT Error m a -> ComponentDSL State q o m Unit
try f = either ((stateErrors %= _) \ (:)) f <=< lift \ unwrap

render :: State -> ComponentHTML Query
render s =
  H.div []
    [ H.button [E.onClick \ E.input_ $ NewTopic]
        [H.text "New Topic"]
    , H.pre [] [H.text $ show s.errors]
    , H.pre [] [H.text $ show s.topics]
    , H.div [] $ renderInspector s
    , H.pre [] [H.text $ show s.editing]
    ]

renderInspector :: State -> Array (ComponentHTML Query)
renderInspector {editing} =
  map (map (SetEditing <@> unit)) $
    F.render editing' (F.mapErrors (const unit) topicForm)
  where
  editing' :: Either EditingRaw Topic
  editing' = unV (const $ Left (fst editing)) Right (snd editing)

topicForm :: ∀ p. Form' p (Unit \/ Void) EditingRaw Topic
topicForm = dimap set get (F.tuple F.nonEmptyString F.boolean)
  where get (name /\ phase) = Topic name (if phase then Learning else Interested)
        set (Topic name phase) = name /\ (phase == Learning)
