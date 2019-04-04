module Main where

import Prelude

import Concur.Core.Types (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Monad.State.Class (get)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Formless as F
import Formless.Component (eval)
import Formless.Internal.Transform as Internal
import Formless.Query (set, submit)
import Formless.Types.Query (InternalState(..), ValidStatus(..))
import Validation as V

-- Form Widget
formWidget :: InputForm -> Validators -> Widget HTML Unit
formWidget initForm initValidators = do
  out <- (F.unwrapOutputFields <<< fst) <$> runStateT formWidget' (initState initForm initValidators)
  liftEffect $ log $ show out

-- Main
main :: Effect Unit
main = runWidgetInDom "root" (formWidget initialInputs formValidators)

-----
-- Formless

newtype ContactForm r f = ContactForm (r
  ( name :: f V.FieldError String String
  , text :: f Void String String
  ))
derive instance newtypeContactForm :: Newtype (ContactForm r f) _

type InputForm = ContactForm Record F.InputField
type OutputForm = ContactForm Record F.OutputField
type Validators = ContactForm Record (F.Validation ContactForm (Widget HTML))
type FState = F.State ContactForm (Widget HTML)

initialInputs :: InputForm
initialInputs = F.wrapInputFields { name: "Testr", text: "Test" }

formValidators :: Validators
formValidators = ContactForm { name: V.minLength 5, text: F.hoistFn_ identity }

-- Symbol proxies for convenience
field :: F.SProxies ContactForm
field = F.mkSProxies (F.FormProxy :: F.FormProxy ContactForm)

-- Stateful form widget
formWidget' ::  StateT FState (Widget HTML) OutputForm
formWidget' = do
  st <- get
  query <- lift do
    D.section'
      [ D.h1' [ D.text "Formless" ]
      , D.h2' [ D.text "A basic contact form." ]
      , D.p' $ pure $ D.text $
        "You can create a full Concur contact form like this in less than 100 lines of code with "
        <> "Formless, most of which is simply Concur boilerplate. The actual form spec and wiring "
        <> "consists of less than 20 lines of code."
      , D.br'
      , D.div'
          [ nameField st
          , textField st
          , submitButton st
          ]
      ]
  res <- eval query
  maybe formWidget' pure res
  where
  nameField st = D.div'
    [ D.label' [D.text "Enter your name:"]
    , set field.name <$> D.input [P._type "text", P.value (F.getInput field.name st.form), P.unsafeTargetValue <$> P.onChange, P.placeholder "Name"]
    , errorDisplay $ F.getError field.name st.form
    ]
  textField st = D.div'
    [ D.label' [D.text "Enter your name:"]
    , set field.text <$> D.input [P._type "text", P.value (F.getInput field.text st.form), P.unsafeTargetValue <$> P.onChange, P.placeholder "Some Text"]
    ]
  submitButton st = D.div' [submit <$ D.button [P.onClick] [D.text "Submit"]]
  errorDisplay = maybe mempty (\err -> D.div [P.style {color: "red"}] [D.text $ V.toText err])

initState :: InputForm -> Validators -> FState
initState form validators =
  { validity: Incomplete
  , dirty: false
  , submitting: false
  , errors: 0
  , submitAttempts: 0
  , form: Internal.inputFieldsToFormFields form
  , internal: InternalState
      { initialInputs: initialInputs
      , validators: validators
      , allTouched: false
      }
  }
