module Page.Login where

import Prelude
import Api.Request (LoginFields)
import Capability.Navigate (class Navigate, navigate)
import Capability.User (class ManageUser, loginUser)
import Component.HTML.Header (header)
import Component.HTML.Utils (css, safeHref, whenElem)
import Data.Email (Email)
import Data.Route (Route(..))
import Form.Field as Field
import Form.Validation as V
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

data Action = HandleLoginForm LoginFields

type State =
  { redirect :: Boolean }

type Input =
  { redirect :: Boolean }

type ChildSlots =
  (formless :: F.Slot LoginForm FormQuery () LoginFields Unit)

newtype LoginForm (r :: Row Type -> Type) f = LoginForm
  ( r
      ( username :: f V.FormError String String
      , password :: f V.FormError String String
      )
  )

derive instance newtypeLoginForm :: Newtype (LoginForm r f) _

data FormQuery a = SetLoginError Boolean a

derive instance functorFormQuery :: Functor FormQuery

component
  :: forall q o m
   . MonadAff m
  => ManageUser m
  => Navigate m
  => H.Component q Input o m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    HandleLoginForm fields -> do
      loginUser fields >>= case _ of
        Nothing ->
          void $ H.query F._formless unit $ F.injQuery $ SetLoginError true unit
        Just _ -> do
          void $ H.query F._formless unit $ F.injQuery $ SetLoginError false unit
          st <- H.get
          when st.redirect (navigate Home)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render _ =
    container
      [ HH.h1
          [ css "text-xs-center" ]
          [ HH.text "Sign In" ]
      , HH.p
          [ css "text-xs-center" ]
          [ HH.a
              [ safeHref Register ]
              [ (HH.text "Need an account?"), (HH.text "Need an account?") ]
          ]
      , HH.slot F._formless unit formComponent unit HandleLoginForm
      ]
    where
    container html =
      HH.div
        [ css "auth-page" ]
        [ header Nothing Login
        , HH.div
            [ css "container page" ]
            [ HH.div
                [ css "row" ]
                [ HH.div
                    [ css "col-md-6 offset-md-3 col-xs12" ]
                    html
                ]
            ]
        ]

data FormAction = Submit Event.Event

formComponent
  :: forall i slots m
   . MonadAff m
  => F.Component LoginForm FormQuery slots i LoginFields m
formComponent = F.component formInput $ F.defaultSpec
  { render = renderLogin
  , handleEvent = handleEvent
  , handleQuery = handleQuery
  , handleAction = handleAction
  }
  where
  formInput :: i -> F.Input LoginForm (loginError :: Boolean) m
  formInput _ =
    { validators: LoginForm
        { username: V.required >>> V.minLength 3 >>> V.maxLength 20
        , password: V.required >>> V.minLength 2 >>> V.maxLength 20
        }
    , initialInputs: Nothing
    , loginError: false
    }

  handleEvent = F.raiseResult

  handleAction = case _ of
    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      eval F.submit
    where
    eval act = F.handleAction handleAction handleEvent act

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetLoginError bool a -> do
      H.modify_ _ { loginError = bool }
      pure (Just a)

  renderLogin { form, loginError } =
    HH.form
      [ HE.onSubmit \ev -> F.injAction $ Submit ev ]
      [ whenElem loginError \_ ->
          HH.div
            [ css "error-messages" ]
            [ HH.text "Username or password is invalid" ]
      , HH.fieldset_
          [ Field.input (Proxy :: Proxy "username") form
              [ HP.placeholder "Username"
              , HP.type_ HP.InputText
              ]
          , Field.input (Proxy :: Proxy "password") form
              [ HP.placeholder "Password"
              , HP.type_ HP.InputPassword
              ]
          , Field.submit "Log in"
          ]
      ]

