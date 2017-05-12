module Reiterate.Topic.Algebra
  ( Topics(..)
  , getTopics
  , saveTopic
  ) where

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Free (Free, liftF)
import Reiterate.Topic (Topic)
import Stuff

data Topics a
  = GetTopics (Error \/ List Topic -> a)
  | SaveTopic Topic (Error \/ Unit -> a)

getTopics :: ExceptT Error (Free Topics) (List Topic)
getTopics = wrap \ liftF $ GetTopics id

saveTopic :: Topic -> ExceptT Error (Free Topics) Unit
saveTopic = wrap \ liftF \ (SaveTopic <@> id)
