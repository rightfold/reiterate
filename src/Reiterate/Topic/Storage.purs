module Reiterate.Topic.Storage
  ( runTopics
  ) where

import DOM.WebStorage.Storage (Storage)
import DOM.WebStorage.Storage as Storage
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Lens (_Right, preview)
import Reiterate.Topic (Phase(..), Topic(..), TopicID, freshTopicID)
import Reiterate.Topic.Algebra (Topics(..))
import Stuff

runTopics :: Storage -> Topics ~> IOSync
runTopics st (GetTopics next) = next \ Right <$> getTopics st
runTopics st (FreshTopic next) = next \ Right <$> freshTopic st
runTopics st (SaveTopic topic next) = next \ Right <$> saveTopic st topic

getTopics :: Storage -> IOSync (List Topic)
getTopics = map (map snd) \ getItems (preview _Right \ (gDecodeJson <=< jsonParser))

freshTopic :: Storage -> IOSync TopicID
freshTopic st = do
  tid <- freshTopicID
  let topic = Topic tid "New Topic" Interested
  saveTopic st topic
  pure tid

saveTopic :: Storage -> Topic -> IOSync Unit
saveTopic st t@(Topic tid _ _) = liftEff $
  let keyS = stringify \ gEncodeJson $ tid
      valueS = stringify \ gEncodeJson $ t
  in Storage.setItem keyS valueS st

getItems :: ∀ a. (String -> Maybe a) -> Storage -> IOSync (List (String /\ a))
getItems f st = liftEff $ tailRecM go =<< (_ /\ Nil) <$> Storage.length st
  where
  go (0 /\ acc) = pure $ Done acc
  go (n /\ acc) = do
    key <- unsafePartial fromJust <$> Storage.key (n - 1) st
    value <- unsafePartial fromJust <$> Storage.getItem key st
    let acc' = maybe acc ((_ : acc) \ (key /\ _)) $ f value
    pure $ Loop (n - 1 /\ acc')
