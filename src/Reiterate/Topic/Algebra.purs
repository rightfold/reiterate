module Reiterate.Topic.Algebra
  ( Topics(..)
  , getTopics
  , freshTopic
  , saveTopic
  ) where

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Free (Free, liftF)
import Reiterate.Topic (Topic, TopicID)
import Stuff

data Topics a
  = GetTopics (Error \/ List Topic -> a)
  | FreshTopic (Error \/ TopicID -> a)
  | SaveTopic Topic (Error \/ Unit -> a)

getTopics :: ExceptT Error (Free Topics) (List Topic)
getTopics = wrap \ liftF $ GetTopics id

freshTopic :: ExceptT Error (Free Topics) TopicID
freshTopic = wrap \ liftF $ FreshTopic id

saveTopic :: Topic -> ExceptT Error (Free Topics) Unit
saveTopic = wrap \ liftF \ (SaveTopic <@> id)
