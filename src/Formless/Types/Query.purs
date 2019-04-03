module Formless.Types.Query where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Variant (Variant)
import Effect.Aff (Milliseconds)
import Formless.Types.Form (FormField, InputField, InputFunction, U)
import Formless.Validation (Validation)

-- | The component query type. See Formless.Query for helpers related
-- | to constructing and using these queries.
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
