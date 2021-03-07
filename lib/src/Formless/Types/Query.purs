module Formless.Types.Query where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Variant (Variant)
import Effect.Aff (Milliseconds)
import Formless.Internal.Transform (class InputFieldsToFormFields, inputFieldsToFormFields)
import Formless.Types.Form (FormField, InputField, InputFunction, U)
import Formless.Validation (Validation)
import Prim.RowList as RL

-- | The component query type. See Formless.Query for helpers related
-- | to constructing and using these queries.
data Query :: forall k1 k2. ((Row Type -> Type) -> (k1 -> Type -> k2 -> Type) -> Type) -> Type
data Query form
  = Modify (form Variant InputFunction)
  | Validate (form Variant U)
  | ModifyValidate (Maybe Milliseconds) (form Variant InputFunction)
  | Reset (form Variant InputFunction)
  | SetAll (form Record InputField)
  | ModifyAll (form Record InputFunction)
  | ResetAll
  | ValidateAll
  | Submit
  | LoadForm (form Record InputField)
  | AndThen (Query form) (Query form)

-- | The component local state
type State form m = Record (StateRow form (internal :: InternalState form m))

-- | The component's public state
type PublicState form = Record (StateRow form ())

-- | The component's public state
type StateRow form r =
  ( validity :: ValidStatus
  , dirty :: Boolean
  , submitting :: Boolean
  , errors :: Int
  , submitAttempts :: Int
  , form :: form Record FormField
  | r
  )

-- | A newtype to make easier type errors for end users to
-- | read by hiding internal fields
newtype InternalState form m = InternalState
  { initialInputs :: form Record InputField
  , validators :: form Record (Validation form m)
  , allTouched :: Boolean
  -- AJ: TODO: Need to figure out a non/less mutaty way of doing this stuff
  -- , debounceRef :: Maybe (Ref (Maybe Debouncer))
  -- , validationRef :: Maybe (Ref (Maybe (Error -> m Unit)))
  }
derive instance newtypeInternalState :: Newtype (InternalState form m) _

-- TODO: AJ: Debouncer
-- | A type to represent a running debouncer
-- type Debouncer =
--   { var :: AVar Unit
--   , fiber :: Fiber Unit
--   }

-- | A type to represent validation status
data ValidStatus
  = Invalid
  | Incomplete
  | Valid

derive instance genericValidStatus :: Generic ValidStatus _
derive instance eqValidStatus :: Eq ValidStatus
derive instance ordValidStatus :: Ord ValidStatus

instance showValidStatus :: Show ValidStatus where
  show = genericShow

-- | Initialise the form state with default values.
-- | Passing in the initial inputs, and the validations.
initFormState
  :: ∀ ixs form is fs m
   . RL.RowToList is ixs
  => InputFieldsToFormFields ixs is fs
  => Newtype (form Record InputField) { | is }
  => Newtype (form Record FormField) { | fs }
  => form Record InputField
  -> form Record (Validation form m)
  -> State form m
initFormState form validations =
  { validity: Incomplete
  , dirty: false
  , submitting: false
  , errors: 0
  , submitAttempts: 0
  , form: inputFieldsToFormFields form
  , internal: InternalState
      { initialInputs: form
      , validators: validations
      , allTouched: false
      -- TODO
      -- , debounceRef: ...
      -- , validationRef: ...
      }
  }
