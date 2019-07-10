module Forms where

import Control.Category ((>>>))
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Data.Unit (Unit)
import Effect (Effect)
import Formless as F
import Menu (menu)
import Pets (Pet(..))
import Prelude (bind, pure, ($), (<$>), (<<<))
import React as React
import React.DOM as D
import React.DOM.Props as P
import Unsafe.Coerce (unsafeCoerce)
import Validation as V

type User =
  { name :: String
  , email :: V.Email
  }

-- Note: Common practice to use `Void` to represent "no error possible"
newtype MyForm r f = MyForm (r
  ( name   :: f V.FieldError String String
  , email1 :: f V.FieldError String V.Email
  , email2 :: f V.FieldError String V.Email
  , pet :: f V.FieldError Pet Pet
  ))
derive instance newtypeMyForm :: Newtype (MyForm r f) _

proxies :: F.SProxies MyForm
proxies = F.mkSProxies (F.FormProxy :: F.FormProxy MyForm)

-- Some type helpers
type InputForm = MyForm Record F.InputField
type OutputForm = MyForm Record F.OutputField
type FormForm = MyForm Record F.FormField
type Validators = MyForm Record (F.Validation MyForm Effect)
type FState = F.State MyForm Effect

initialInputs :: InputForm
initialInputs = F.wrapInputFields
  { name: ""
  , email1: ""
  , email2: ""
  , pet: Dogs
  }

validators :: Validators
validators = MyForm
  { name: V.minLength 3
  , email1: V.emailFormat
  , email2: V.equalsEmail1 >>> V.emailFormat
  , pet: V.mustLikePets
  }

initState :: InputForm -> Validators -> FState
initState form validations = F.initFormState form validations

formWidget
  :: FState
  -> (User -> Effect Unit)
  -> React.ReactClass {}
formWidget fstate' userHandler =
  React.component "Form" component
  where
    component this =
      pure { state: fstate'
           , render: render this <$> React.getState this
           }
    render this fstate =
      D.div []
        [ D.div [] [D.text "Name"]
        , D.input
          [ P.value $ F.getInput proxies.name fstate.form
          , P.onChange onChangeName
          ]
        , errorDisplay $ F.getError proxies.name fstate.form
        , D.div' [D.text "Email"]
        , D.input
          [ P.value $ F.getInput proxies.email1 fstate.form
            -- This will help us avoid hitting the server on every single key press.
          , P.onChange onChangeEmail1
          ]
        , errorDisplay $ F.getError proxies.email1 fstate.form
        , D.div' [D.text "Confirm Email"]
        , D.input
          [ P.value $ F.getInput proxies.email2 fstate.form
          , P.onChange onChangeEmail2
          ]
        , errorDisplay $ F.getError proxies.email2 fstate.form
        , D.div' [D.text "Do you like pets?"]
        , React.createLeafElement (menu fstate.form proxies.pet queryHandler) {}
        , errorDisplay $ F.getError proxies.pet fstate.form
        , D.div [] [D.button [P.onClick onSubmit] [D.text "Submit"]]
        ]
      where
        unsafeTargetValue e = (unsafeCoerce e).target.value
        queryHandler query = do
          res <- F.eval query fstate
          case res of
            Left fstateModified -> React.setState this fstateModified
            Right out -> do
              let form = F.unwrapOutputFields out
              userHandler {name: form.name, email: form.email1}
        onChangeName = queryHandler <<< F.set proxies.name <<< unsafeTargetValue
        onChangeEmail1 = queryHandler <<< F.asyncSetValidate debounceTime proxies.email1 <<< unsafeTargetValue
        onChangeEmail2 = queryHandler <<< F.asyncSetValidate debounceTime proxies.email2 <<< unsafeTargetValue
        onSubmit _ = queryHandler F.submit
        errorDisplay = maybe mempty (\err -> D.div [P.style {color: "red"}] [D.text $ V.toText err])
        debounceTime = Milliseconds 300.0
