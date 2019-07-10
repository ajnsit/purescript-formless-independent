module Forms where

import Concur.Core (Widget)
import Concur.Core.Props (Props(..))
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (ReactProps)
import Concur.React.Props as P
import Control.Applicative ((<$))
import Control.Category ((>>>))
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Formless as F
import Menu (menu)
import Pets (Pet(..))
import Prelude (bind, pure, ($), (<$>), (<<<))
import React.DOM.Props (unsafeMkProps)
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
type Validators = MyForm Record (F.Validation MyForm (Widget HTML))
type FState = F.State MyForm (Widget HTML)

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

-- valueP :: forall a. a -> Props
valueP :: forall a b. a -> ReactProps b
valueP = PrimProp <<< unsafeMkProps "value"

formWidget :: FState -> Widget HTML User
formWidget fstate = do
  query <- D.div'
    [ D.div' [D.text "Name"]
    , D.input
      [ P.value $ F.getInput proxies.name fstate.form
      , (F.set proxies.name <<< P.unsafeTargetValue) <$> P.onChange
      ]
    , errorDisplay $ F.getError proxies.name fstate.form
    , D.div' [D.text "Email"]
    , D.input
      [ P.value $ F.getInput proxies.email1 fstate.form
        -- This will help us avoid hitting the server on every single key press.
      , (F.asyncSetValidate debounceTime proxies.email1 <<< P.unsafeTargetValue) <$> P.onChange
      ]
    , errorDisplay $ F.getError proxies.email1 fstate.form
    , D.div' [D.text "Confirm Email"]
    , D.input
      [ P.value $ F.getInput proxies.email2 fstate.form
      , (F.asyncSetValidate debounceTime proxies.email2 <<< P.unsafeTargetValue) <$> P.onChange
      ]
    , errorDisplay $ F.getError proxies.email2 fstate.form
    , D.div' [D.text "Do you like pets?"]
    , menu fstate.form proxies.pet
    , errorDisplay $ F.getError proxies.pet fstate.form
    , D.div' [F.submit <$ D.button [P.onClick] [D.text "Submit"]]
    ]
  res <- F.eval query fstate
  case res of
    Left fstate' -> formWidget fstate'
    Right out -> do
      let form = F.unwrapOutputFields out
      pure {name: form.name, email: form.email1}
  where
    errorDisplay = maybe mempty (\err -> D.div [P.style {color: "red"}] [D.text $ V.toText err])
    debounceTime = Milliseconds 300.0
