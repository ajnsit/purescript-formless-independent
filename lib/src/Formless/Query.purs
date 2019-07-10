-- | This module exports helpers for working with Formless queries.
-- | Since many queries are used as actions and may involve injecting
-- | variants, these helpers are provided to remove any associated
-- | boilerplate. Prefer these over using data constructors from the
-- | Formless query algebra.
module Formless.Query where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Time.Duration (Milliseconds)
import Data.Variant (Variant, inj)
import Formless.Class.Initial (class Initial, initial)
import Formless.Transform.Record (WrapField, wrapInputFields, wrapInputFunctions)
import Formless.Types.Query (Query(..))
import Formless.Types.Form (InputField, InputFunction, U(..))
import Heterogeneous.Mapping as HM
import Prim.Row as Row

-- | Submit the form, which will trigger a `Submitted` result if the
-- | form validates successfully.
submit :: ∀ form. Query form
submit = Submit

-- | Replace all form inputs with a new set of inputs, and re-initialize
-- | the form to a new state. Useful to set a new "initial state" for a form,
-- | especially when filling a form with data loaded asynchronously.
loadForm
  :: ∀ form
   . form Record InputField
  -> Query form
loadForm = LoadForm

-- | `initialize` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
loadForm_
  :: ∀ form
   . form Record InputField
  -> Query form
loadForm_ = LoadForm

-- | Perform two Formless actions in sequence. Can be chained arbitrarily.
-- | Useful when a field needs to modify itself on change and also trigger
-- | validation on one or more other fields, or when a modification on one
-- | field should also modify another field.
andThen
  :: ∀ form
   . Query form
  -> Query form
  -> Query form
andThen = AndThen

-- | Set the input value of a form field at the specified label
set
  :: ∀ form inputs sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> i
  -> Query form
set sym i = Modify (wrap (inj sym (wrap (const i))))

-- | Set the input value of a form field at the specified label, also triggering
-- | validation to run on the field.
setValidate
  :: ∀ form inputs sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> i
  -> Query form
setValidate sym i = ModifyValidate Nothing (wrap (inj sym (wrap (const i))))

-- | Set the input value of a form field at the specified label, while debouncing
-- | validation so that it only runs after the specified amount of time has elapsed
-- | since the last modification. Useful when you need to avoid expensive validation
-- | but do not want to wait for a blur event to validate.
asyncSetValidate
  :: ∀ form inputs sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => Milliseconds
  -> SProxy sym
  -> i
  -> Query form
asyncSetValidate ms sym i = ModifyValidate (Just ms) (wrap (inj sym (wrap (const i))))

-- | Modify the input value of a form field at the specified label with the
-- | provided function.
modify
  :: ∀ form inputs sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> (i -> i)
  -> Query form
modify sym f = Modify (wrap (inj sym (wrap f)))

-- | Modify the input value of a form field at the specified label, also triggering
-- | validation to run on the field, with the provided function.
modifyValidate
  :: ∀ form inputs sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> (i -> i)
  -> Query form
modifyValidate sym f = ModifyValidate Nothing (wrap (inj sym (wrap f)))

-- | Modify the input value of a form field at the specified label, while debouncing
-- | validation so that it only runs after the specified amount of time has elapsed
-- | since the last modification. Useful when you need to avoid expensive validation
-- | but do not want to wait for a blur event to validate.
asyncModifyValidate
  :: ∀ form inputs sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => Milliseconds
  -> SProxy sym
  -> (i -> i)
  -> Query form
asyncModifyValidate ms sym f = ModifyValidate (Just ms) (wrap (inj sym (wrap f)))

-- | Reset the value of the specified form field to its default value
-- | according to the `Initial` type class.
reset
  :: ∀ form inputs sym t0 e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> Query form
reset sym = Reset (wrap (inj sym (wrap (const initial))))

-- | A helper to create the correct `Validate` query for Formless, given
-- | a label
validate
  :: ∀ form us sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant U) (Variant us)
  => Row.Cons sym (U e i o) t0 us
  => SProxy sym
  -> Query form
validate sym = Validate (wrap (inj sym U))

-- | Provide a record of input fields to overwrite all current
-- | inputs. Unlike `initialize`, this does not otherwise reset
-- | the form as if it were new. Similar to calling `set` on every
-- | field in the form.
setAll
  :: ∀ form is is'
   . Newtype (form Record InputField) { | is' }
  => HM.HMap WrapField { | is } { | is' }
  => { | is }
  -> Query form
setAll = SetAll <<< wrapInputFields

-- | Provide a record of input functions to modify all current
-- | inputs. Similar to calling `modify` on every field in the form.
modifyAll
  :: ∀ form ifs ifs'
   . Newtype (form Record InputFunction) { | ifs' }
  => HM.HMap WrapField { | ifs } { | ifs' }
  => { | ifs }
  -> Query form
modifyAll = ModifyAll <<< wrapInputFunctions

-- | Reset all fields to their initial values, and reset the form
-- | to its initial pristine state, no touched fields.
resetAll :: ∀ form. Query form
resetAll = ResetAll

-- | Validate all fields in the form, collecting errors
validateAll :: ∀ form. Query form
validateAll = ValidateAll

-- | Provide a record of inputs to overwrite all current inputs without
-- | resetting the form (as `initialize` does), and then validate the
-- | entire new set of fields. Similar to calling `setValidate` on every
-- | field in the form.
setValidateAll
  :: ∀ form is is'
   . Newtype (form Record InputField) { | is' }
  => HM.HMap WrapField { | is } { | is' }
  => { | is }
  -> Query form
setValidateAll is = setAll is `andThen` validateAll

-- | Provide a record of input functions to modify all current
-- | inputs, and then validate all fields.  Similar to calling
-- | `modifyValidate` on every field in the form.
modifyValidateAll
  :: ∀ form ifs ifs'
   . Newtype (form Record InputFunction) { | ifs' }
  => HM.HMap WrapField { | ifs } { | ifs' }
  => { | ifs }
  -> Query form
modifyValidateAll ifs = modifyAll ifs `andThen` validateAll
