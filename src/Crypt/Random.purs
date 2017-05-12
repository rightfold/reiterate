module Crypt.Random
  ( randomUUID4
  ) where

import Stuff

foreign import randomUUID4 :: IOSync String
