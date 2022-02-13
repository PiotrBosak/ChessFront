module Page.Register where

import Prelude
import Api.Request (RegisterFields)
import Capability.Navigate (class Navigate, navigate)
import Capability.User
import Component.HTML.Header (header)
import Component.HTML.Utils (css, safeHref)
import Data.Email (Email)
import Data.Route (Route(..))
import Data.Username (Username)
import Form.Field as Field
import Form.Validation as V
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event

newtype RegisterForm (r :: Row Type -> Type) f = RegisterForm
  ( r
      ( username :: f V.FormError String Username
      , email :: f V.FormError String Email
      , password :: f V.FormError String String
      )
  )

derive instance newtypeRegisterForm :: Newtype (RegisterForm r f) _

data Action = HandleRegisterForm RegisterFields

component
  :: forall q o m
   . MonadAff m
  => ManageUser m
  => Navigate m
  => H.Component q Unit o m
component = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }

  where
  render _ =
    container
      [ HH.h1
          [ css "text-xs-center" ]
          [ HH.text "Sign up" ]
      , HH.p
          [ css "text-xs-center" ]
          [ HH.a
              [ safeHref Login ]
              [ HH.text "Already have an account? " ]
          ]
      , HH.slot F._formless unit formComponent unit HandleRegisterForm
      ]
    where
    container html =
      HH.div_
        [ header Nothing Register
        , HH.div
            [ css "auth-page" ]
            [ HH.div
                [ css "col-md-6 offset-md-3 col-xs12" ]
                html
            ]
        ]

  handleAction :: forall state output slots. Action -> H.HalogenM state Action slots output m Unit
  handleAction = case _ of
    HandleRegisterForm fields ->
      registerUser fields >>= traverse_ (\_ -> navigate Home)

data FormAction = Submit Event.Event

formComponent
  :: forall formQuery formSlots formInput m
   . MonadAff m
  => F.Component RegisterForm formQuery formSlots formInput RegisterFields m
formComponent = F.component formInput $ F.defaultSpec
  { render = renderForm
  , handleEvent = handleEvent
  , handleAction = handleAction
  }
  where
  formInput _ =
    { validators: RegisterForm
        { username: V.required >>> V.usernameFormat
        , email: V.required >>> V.minLength 3 >>> V.emailFormat
        , password: V.required >>> V.minLength 8 >>> V.maxLength 20
        }
    , initialInputs: Nothing
    }

  handleEvent = F.raiseResult
  handleAction = case _ of
    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      eval F.submit
    where
    eval act = F.handleAction handleAction handleEvent act

  renderForm { form } =
    HH.form
      [ HE.onSubmit \ev -> F.injAction $ Submit ev ]
      [ HH.fieldset_
          [ username
          , email
          , password
          ]
      , Field.submit "Sign up"
      ]
    where
    username =
      Field.input (Proxy :: Proxy "username") form
        [ HP.placeholder "Username", HP.type_ HP.InputText ]

    email =
      Field.input (Proxy :: Proxy "email") form
        [ HP.placeholder "Email", HP.type_ HP.InputEmail ]

    password =
      Field.input (Proxy :: Proxy "password") form
        [ HP.placeholder "Password", HP.type_ HP.InputPassword ]