module Reiterate.Topic.Algebra
  ( Topics(..)
  , getTopics
  , freshTopic
  , saveTopic
  ) where

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Free (Free, liftF)
import Data.Map (Map)
import Reiterate.Topic (Topic, TopicID)
import Stuff

data Topics a
  = GetTopics (Error \/ Map TopicID Topic -> a)
  | FreshTopic (Error \/ TopicID -> a)
  | SaveTopic TopicID Topic (Error \/ Unit -> a)

getTopics :: ExceptT Error (Free Topics) (Map TopicID Topic)
getTopics = wrap \ liftF $ GetTopics id

freshTopic :: ExceptT Error (Free Topics) TopicID
freshTopic = wrap \ liftF $ FreshTopic id

saveTopic :: TopicID -> Topic -> ExceptT Error (Free Topics) Unit
saveTopic tid topic = wrap \ liftF $ SaveTopic tid topic id
