module Data.String.NonEmpty
  ( NonEmptyString(..)
  , fromString
  , toString
  ) where

import Data.Generic (class Generic)
import Data.String as String
import Stuff

data NonEmptyString = NonEmptyString Char String
derive instance en :: Eq NonEmptyString
derive instance on :: Ord NonEmptyString
derive instance gn :: Generic NonEmptyString
instance sn :: Show NonEmptyString where
  show (NonEmptyString c cs) =
    "(NonEmptyString " <> show c <> " " <> show cs <> ")"

fromString :: String -> Maybe NonEmptyString
fromString = String.uncons >>> map \{head, tail} -> NonEmptyString head tail

toString :: NonEmptyString -> String
toString (NonEmptyString c cs) = String.singleton c <> cs
