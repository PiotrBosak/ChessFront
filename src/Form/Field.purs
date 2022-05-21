module Form.Field where

import Prelude

import Component.HTML.Utils (css, maybeElem)
import Form.Validation (errorToString)
import Form.Validation as V
import DOM.HTML.Indexed (HTMLinput, HTMLtextarea)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen as H
import Data.Either
import Data.Maybe
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy)
import Formless as F
import Type.Row as Row

submit :: forall i p. String -> HH.HTML i p
submit buttonText =
  HH.input
    [ css "btn btn-lg btn-primary-black pull-xs-right"
    , HP.type_ HP.InputSubmit
    , HP.value buttonText
    ]
type StringField :: (Type -> Type -> Type -> Type) -> Type -> Type
type StringField f output = f String V.FormError output

submitButton :: forall i p. String -> HH.HTML i p
submitButton label =
  HH.input
    [ css "btn btn-lg btn-primary pull-xs-right"
    , HP.type_ HP.InputSubmit
    , HP.value label
    ]
type TextInput action output =
  { state :: F.FieldState String V.FormError output
  , action :: F.FieldAction action String V.FormError output
  }

textInput
  :: forall output action slots m
   . TextInput action output
  -> Array (HP.IProp HTMLinput action)
  -> H.ComponentHTML action slots m
textInput { state, action } props =
  HH.fieldset
    [ css "form-group" ]
    [ HH.input
        ( append
            [ css "form-control form-control-lg"
            , HP.value state.value
            , HE.onValueInput action.handleChange
            , HE.onBlur action.handleBlur
            ]
            props
        )
    , maybeElem (state.result >>= either pure (const Nothing)) \err ->
        HH.div
          [ css "error-messages" ]
          [ HH.text $ errorToString err ]
    ]

textarea
  :: forall output action slots m
   . TextInput action output
  -> Array (HP.IProp HTMLtextarea action)
  -> H.ComponentHTML action slots m
textarea { state, action } props =
  HH.fieldset
    [ css "form-group" ]
    [ HH.textarea
        ( append
            [ css "form-control form-control-lg"
            , HP.rows 8
            , HP.value state.value
            , HE.onValueInput action.handleChange
            , HE.onBlur action.handleBlur
            ]
            props
        )
    ]