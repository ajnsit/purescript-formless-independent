module Formless.Component where

import Prelude

import Control.Monad.State.Class (class MonadState, get, modify, modify_)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Eq (class EqRecord)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Variant (Variant)
import Formless.Data.FormFieldResult (FormFieldResult)
import Formless.Internal.Transform as Internal
import Formless.Types.Query (InternalState(..), Query(..), State, ValidStatus(..))
import Formless.Types.Form (FormField, InputField, InputFunction, OutputField, U)
import Formless.Validation (Validation)
import Prim.RowList as RL
import Unsafe.Coerce (unsafeCoerce)

eval
  :: forall form t m is ixs ivs fs fxs us vs os ifs ivfs
  .  RL.RowToList is ixs
  => RL.RowToList fs fxs
  => EqRecord ixs is
  => Internal.InputFieldsToFormFields ixs is fs
  => Internal.FormFieldsToInputFields fxs fs is
  => Internal.CountErrors fxs fs
  => Internal.AllTouched fxs fs
  => Internal.SetFormFieldsTouched fxs fs fs
  => Internal.ReplaceFormFieldInputs is fxs fs fs
  => Internal.ModifyAll ifs fxs fs fs
  => Internal.ValidateAll vs fxs fs fs m
  => Internal.FormFieldToMaybeOutput fxs fs os
  => Newtype (form Record InputField) { | is }
  => Newtype (form Record InputFunction) { | ifs }
  => Newtype (form Record FormField) { | fs }
  => Newtype (form Record OutputField) { | os }
  => Newtype (form Record (Validation form m)) { | vs }
  => Newtype (form Variant InputField) (Variant ivs)
  => Newtype (form Variant InputFunction) (Variant ivfs)
  => Newtype (form Variant U) (Variant us)
  -- AJ: TODO: Not quite happy with the dependency on MonadTrans for state
  -- Validations can run in a smaller monad 'm'
  => MonadState (State form m) (t m)
  => MonadTrans t
  => Monad m
  => Query form
  -> t m (Maybe (form Record OutputField))
eval query = case query of

  Modify variant -> do
    modify_ \st -> st {form = Internal.unsafeModifyInputVariant identity variant st.form}
    syncFormData

  Validate variant -> do
    st <- get
    form' <- lift $ Internal.unsafeRunValidationVariant variant (unwrap st.internal).validators st.form
    modify_ \s -> s {form = form'}
    syncFormData

  -- Provided as a separate query to minimize state updates / re-renders
  ModifyValidate milliseconds variant -> do
    let
      modifyWith
        :: (forall e o. FormFieldResult e o -> FormFieldResult e o)
        -> t m (form Record FormField)
      modifyWith f = do
        s <- modify \st -> st { form = Internal.unsafeModifyInputVariant f variant st.form }
        pure s.form

      validate = do
        st <- get
        let vs = (unwrap st.internal).validators
        form <- lift $ Internal.unsafeRunValidationVariant (unsafeCoerce variant) vs st.form
        modify_ \s -> s { form = form }
        pure form

    case milliseconds of
      Nothing -> do
        _ <- modifyWith identity
        _ <- validate
        syncFormData
      Just ms -> do
        -- AJ: TODO: debounce
        -- debounceForm
        --   ms
        --   (modifyWith identity)
        --   (modifyWith (const Validating) *> validate)
        --   (eval $ SyncFormData a)
        _ <- modifyWith identity
        _ <- validate
        syncFormData

  Reset variant -> do
    modify_ \st -> st
      { form = Internal.unsafeModifyInputVariant identity variant st.form
      , internal = over InternalState (_ { allTouched = false }) st.internal
      }
    syncFormData

  SetAll formInputs -> do
    new <- modify \st -> st
      { form = Internal.replaceFormFieldInputs formInputs st.form }
    -- AJ: TODO: Why is this here? Rerender??
    -- H.raise $ Changed $ getPublicState new
    syncFormData

  ModifyAll formInputs -> do
    new <- modify \st -> st
      { form = Internal.modifyAll formInputs st.form }
    -- AJ: TODO: Why is this here? Rerender??
    -- H.raise $ Changed $ getPublicState new
    syncFormData

  ValidateAll -> do
    st <- get
    form <- lift $ Internal.validateAll (unwrap st.internal).validators st.form
    modify_ _ { form = form }
    syncFormData

  -- Submit, also raising a message to the user
  Submit -> runSubmit
    -- AJ: TODO: Why is this here? Rerender??
    -- traverse_ Submitted mbForm

  -- | Should completely reset the form to its initial state
  ResetAll -> do
    modify_ \st -> st
      { validity = Incomplete
      , dirty = false
      , errors = 0
      , submitAttempts = 0
      , submitting = false
      , form = Internal.replaceFormFieldInputs (unwrap st.internal).initialInputs st.form
      , internal = over InternalState (_ { allTouched = false }) st.internal
      }
    pure Nothing
    -- AJ: TODO: Why is this here? Rerender??
    -- H.raise $ Changed $ getPublicState new

  LoadForm formInputs -> do
    modify_ \st -> st
      { validity = Incomplete
      , dirty = false
      , errors = 0
      , submitAttempts = 0
      , submitting = false
      , form = Internal.replaceFormFieldInputs formInputs st.form
      , internal = over
          InternalState
          (_
            { allTouched = false
            , initialInputs = formInputs
            }
          )
          st.internal
      }
    pure Nothing
    -- AJ: TODO: Why is this here? Rerender??
    -- H.raise $ Changed $ getPublicState new

  -- Receive { render, validators } a -> do
  --   let applyOver = over InternalState (_ { validators = validators })
  --   modifyStore_ render (\st -> st { internal = applyOver st.internal })
  --   pure a

  AndThen q1 q2 -> do
    void (eval q1)
    void (eval q2)
    pure Nothing

  where

  -- Sync the overall state of the form after an individual field change or overall validation.
  syncFormData = do
    st <- get

    let errors = Internal.countErrors st.form
        dirty = not $ eq
          (unwrap (Internal.formFieldsToInputFields st.form))
          (unwrap (unwrap st.internal).initialInputs)

    -- Need to verify the validity status of the form.
    case (unwrap st.internal).allTouched of
      true -> modify_ \s -> s
        { validity = if not (st.errors == 0) then Invalid else Valid
        , errors = errors
        , dirty = dirty
        }

      -- If not all fields are touched, then we need to quickly sync the form state
      -- to verify this is actually the case.
      _ -> case Internal.allTouched st.form of

        -- The sync revealed all fields really have been touched
        true -> modify_ \s -> s
          { validity = if not (st.errors == 0) then Invalid else Valid
          , internal = over InternalState (_ { allTouched = true }) st.internal
          , errors = errors
          , dirty = dirty
          }

        -- The sync revealed that not all fields have been touched
        _ -> modify_ \s -> s { validity = Incomplete, errors = errors, dirty = dirty }

    pure Nothing

  -- Run submission without raising messages or replies
  runSubmit = do
    init <- modify \st -> st
      { submitAttempts = st.submitAttempts + 1
      , submitting = true
      }

    -- For performance purposes, avoid running this if possible
    let internal = unwrap init.internal
    when (not internal.allTouched) do
      modify_ \st -> st
        { form = Internal.setFormFieldsTouched init.form
        , internal = over InternalState (_ { allTouched = true }) init.internal
        }

    -- Necessary to validate after fields are touched, but before parsing
    _ <- eval ValidateAll

    -- For performance purposes, only attempt to submit if the form is valid
    validated <- get
    modify_ \st -> st { submitting = false }
    pure $
      if validated.validity == Valid
      then Internal.formFieldsToMaybeOutputFields validated.form
      else Nothing
