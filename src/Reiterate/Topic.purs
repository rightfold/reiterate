module Reiterate.Topic
  ( TopicID(..)
  , freshTopicID

  , Topic(..)

  , Phase(..)
  ) where

import Crypt.Random (randomUUID4)
import Data.Generic (class Generic)
import Stuff

--------------------------------------------------------------------------------

newtype TopicID = TopicID String
derive instance et :: Eq TopicID
derive instance ot :: Ord TopicID
derive instance nt :: Newtype TopicID _
derive instance gt :: Generic TopicID
instance st :: Show TopicID where
  show (TopicID t) = "(TopicID " <> show t <> ")"

freshTopicID :: IOSync TopicID
freshTopicID = TopicID <$> randomUUID4

--------------------------------------------------------------------------------

data Topic = Topic String Phase
derive instance go :: Generic Topic
instance so :: Show Topic where
  show (Topic n p) = "(Topic " <> show n <> " " <> show p <> ")"

--------------------------------------------------------------------------------

data Phase
  = Interested
  | Learning
  | Understanding
derive instance ep :: Eq Phase
derive instance op :: Ord Phase
derive instance gp :: Generic Phase
instance sp :: Show Phase where
  show Interested = "Interested"
  show Learning = "Learning"
  show Understanding = "Understanding"
