module SomeFile where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Maybe

element = HH.h1 [] [ HH.text "Hello, World" ]

someThing = HH.div
  [ (HE.onClick \_ -> "Hello"), (HP.id "root") ]
  [ HH.input
      [ HP.placeholder "Name" ]
  , HH.button
      [ HP.classes [ HH.ClassName "btn-primary" ]
      , HP.type_ HP.ButtonSubmit
      ]
      [ HH.text "Submit" ]
  ]

header :: forall w i. Int -> HH.HTML w i
header visits =
  HH.h1_
    [ HH.text $ "You've had " <> show visits <> " visitors" ]

lakes = [ "some", "lake" ]

html :: forall w i. HH.HTML w i
html = HH.div_ (map HH.text lakes)

maybeElem :: forall w i a. Maybe a -> (a -> HH.HTML w i) -> HH.HTML w i
maybeElem m f = case m of
  Just a -> f a
  Nothing -> HH.text ""

primaryButton :: forall w i. HH.HTML w i -> HH.HTML w i
primaryButton label =
  HH.button
    [ HP.classes [ HH.ClassName "primary" ] ]
    [ label ]

