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
import Data.Map (Map)
import Data.Map as Map
import Data.String.NonEmpty (NonEmptyString(..))
import Reiterate.Topic (Phase(..), Topic(..), TopicID, freshTopicID)
import Reiterate.Topic.Algebra (Topics(..))
import Stuff

freshTopic' :: Topic
freshTopic' = Topic (NonEmptyString 'N' "ew Topic") Interested

runTopics :: Storage -> Topics ~> IOSync
runTopics st (GetTopics next) = next \ Right <$> getTopics st
runTopics st (FreshTopic next) = next \ Right <$> freshTopic st
runTopics st (SaveTopic tid topic next) = next \ Right <$> saveTopic st tid topic

getTopics :: Storage -> IOSync (Map TopicID Topic)
getTopics = map Map.fromFoldable \ getItems \keyS valueS -> preview _Right $
  (/\) <$> (gDecodeJson <=< jsonParser $ keyS)
       <*> (gDecodeJson <=< jsonParser $ valueS)

freshTopic :: Storage -> IOSync TopicID
freshTopic st = do
  tid <- freshTopicID
  saveTopic st tid freshTopic'
  pure tid

saveTopic :: Storage -> TopicID -> Topic -> IOSync Unit
saveTopic st tid topic = liftEff $
  let keyS = stringify \ gEncodeJson $ tid
      valueS = stringify \ gEncodeJson $ topic
  in Storage.setItem keyS valueS st

getItems :: ∀ a. (String -> String -> Maybe a) -> Storage -> IOSync (List a)
getItems f st = liftEff $ tailRecM go =<< (_ /\ Nil) <$> Storage.length st
  where
  go (0 /\ acc) = pure $ Done acc
  go (n /\ acc) = do
    key <- unsafePartial fromJust <$> Storage.key (n - 1) st
    value <- unsafePartial fromJust <$> Storage.getItem key st
    let acc' = maybe acc (_ : acc) $ f key value
    pure $ Loop (n - 1 /\ acc')
