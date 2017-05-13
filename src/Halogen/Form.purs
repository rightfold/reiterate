module Halogen.Form where

import Control.Monad.State.Class as State
import Data.Int as Int
import Data.List.NonEmpty (NonEmptyList)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Validation.Semigroup (V, invalid, unV)
import Halogen.Component (Component)
import Halogen.Component as C
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.Query as Q
import Stuff

--------------------------------------------------------------------------------

data Form p e s t a b =
  Form (a -> t)
       (s -> V (NonEmptyList e) b)
       (s -> Array (HTML p t))

type Form' p e s a = Form p e s s a a

instance pf :: Profunctor (Form p e s t) where
  dimap f g (Form i o h) = Form (i \ f) (map g \ o) h

mapErrors :: ∀ p e e' s t a b. (e -> e') -> Form p e s t a b
  -> Form p e' s t a b
mapErrors f (Form i o h) = Form i (lmap (map f) \ o) h

--------------------------------------------------------------------------------

newtype Raw p e a b s t = Raw (Form p e s t a b)

derive instance nr :: Newtype (Raw p e a b s t) _

instance pr :: Profunctor (Raw p e a b) where
  dimap f g (Raw (Form i o h)) = Raw $
    Form (g \ i) (o \ f) (map (map g) \ h \ f)

--------------------------------------------------------------------------------

component :: ∀ e r a m. (∀ p. Form' p e r a)
  -> Component HTML (Tuple r <\/> Identity) a a m
component f@(Form i o h) = C.component
  { initialState: i
  , receiver: Just \ left \ (_ /\ unit) \ i
  , render: \r ->
      H.div []
        [ H.div [] $ map (map (left \ void)) (render (Left r) f)
        , H.div []
            [ H.button [E.onClick \ E.input_ $ right \ Identity]
                [H.text "Save"]
            ]
        ]
  , eval: coproduct
      (uncurry $ ($>) \ State.put)
      (((unV (const (pure unit)) Q.raise \ o =<< State.get) $> _) \ unwrap)
  }

render :: ∀ p e r a. r \/ a -> Form' p e r a
  -> Array (HTML p (r /\ V (NonEmptyList e) a))
render ra (Form i o h) = map (map \r -> r /\ o r) (h (either id i ra))

--------------------------------------------------------------------------------

boolean :: ∀ p. Form' p Void Boolean Boolean
boolean = Form id pure $ \r -> pure $
  H.input [ E.onChecked Just
          , P.checked r
          , P.type_ P.InputCheckbox
          ]

string :: ∀ p. Form' p Void String String
string = Form id pure $ \r -> pure $
  H.input [ E.onValueInput Just
          , P.value r
          ]

text :: ∀ p. Form' p Void String String
text = Form id pure $ \r -> pure $
  H.textarea [ E.onValueInput Just
             , P.value r
             ]

nonEmptyString :: ∀ p. Form' p Unit String NonEmptyString
nonEmptyString = Form NES.toString fromString $ \r -> pure $
  H.input [ E.onValueInput Just
          , P.value r
          ]
  where
  fromString :: String -> V (NonEmptyList Unit) NonEmptyString
  fromString = maybe (invalid (pure unit)) pure \ NES.fromString

int :: ∀ p. Form' p Unit String Int
int = Form show fromString $ \r -> pure $
  H.input [ E.onValueInput Just
          , P.value r
          ]
  where
  fromString :: String -> V (NonEmptyList Unit) Int
  fromString = maybe (invalid (pure unit)) pure \ Int.fromString

tuple :: ∀ p e1 e2 r1 r2 a1 a2. Form' p e1 r1 a1 -> Form' p e2 r2 a2 ->
  Form' p (e1 \/ e2) (r1 /\ r2) (a1 /\ a2)
tuple f1 f2 = case mapErrors Left f1, mapErrors Right f2 of
  Form i1 o1 h1, Form i2 o2 h2 ->
    Form (i1 *** i2) (bitraverse o1 o2) $ \(r1 /\ r2) ->
      map (map (_ /\ r2)) (h1 r1) <> map (map (r1 /\ _)) (h2 r2)
