module Formless.Component where

import Prelude

import Data.Either (Either(..))
import Data.Eq (class EqRecord)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Variant (Variant)
import Formless.Data.FormFieldResult (FormFieldResult)
import Formless.Internal.Transform as Internal
import Formless.Types.Form (FormField, InputField, InputFunction, OutputField, U)
import Formless.Types.Query (InternalState(..), Query(..), State, ValidStatus(..))
import Formless.Validation (Validation)
import Prim.RowList as RL
import Unsafe.Coerce (unsafeCoerce)

eval
  :: forall form m is ixs ivs fs fxs us vs os ifs ivfs
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
  => Monad m
  => Query form
  -> State form m
  -> m (Either (State form m) (form Record OutputField))
eval query st = case query of

  Modify variant -> do
    syncFormData $ st {form = Internal.unsafeModifyInputVariant identity variant st.form}

  Validate variant -> do
    form' <- Internal.unsafeRunValidationVariant variant (unwrap st.internal).validators st.form
    syncFormData $ st {form = form'}

  -- Provided as a separate query to minimize state updates / re-renders
  ModifyValidate milliseconds variant -> do
    let
      modifyWith
        :: (forall e o. FormFieldResult e o -> FormFieldResult e o)
        -> State form m
        -> State form m
      modifyWith f st' = st' { form = Internal.unsafeModifyInputVariant f variant st'.form }

      validate st' = do
        let vs = (unwrap st'.internal).validators
        form <- Internal.unsafeRunValidationVariant (unsafeCoerce variant) vs st'.form
        pure $ st' { form = form }

    case milliseconds of
      Nothing -> do
        let st' = modifyWith identity st
        st'' <- validate st'
        syncFormData st''
      Just ms -> do
        -- AJ: TODO: debounce
        -- debounceForm
        --   ms
        --   (modifyWith identity)
        --   (modifyWith (const Validating) *> validate)
        --   (eval $ SyncFormData a)
        let st' = modifyWith identity st
        st'' <- validate st'
        syncFormData st''

  Reset variant -> do
    syncFormData $ st
      { form = Internal.unsafeModifyInputVariant identity variant st.form
      , internal = over InternalState (_ { allTouched = false }) st.internal
      }

  SetAll formInputs -> do
    syncFormData $ st
      { form = Internal.replaceFormFieldInputs formInputs st.form }

  ModifyAll formInputs -> do
    syncFormData $ st
      { form = Internal.modifyAll formInputs st.form }

  ValidateAll -> do
    form <- Internal.validateAll (unwrap st.internal).validators st.form
    syncFormData $ st {form = form}

  -- Submit, also raising a message to the user
  Submit -> runSubmit st

  -- | Should completely reset the form to its initial state
  ResetAll -> do
    pure $ Left $ st
      { validity = Incomplete
      , dirty = false
      , errors = 0
      , submitAttempts = 0
      , submitting = false
      , form = Internal.replaceFormFieldInputs (unwrap st.internal).initialInputs st.form
      , internal = over InternalState (_ { allTouched = false }) st.internal
      }

  LoadForm formInputs -> do
    pure $ Left $ st
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

  AndThen q1 q2 -> do
    r1 <- eval q1 st
    case r1 of
      Left st' -> eval q2 st'
      Right form -> pure (Right form)

  where

  -- Sync the overall state of the form after an individual field change or overall validation.
  syncFormData stt = do

    let errors = Internal.countErrors stt.form
        dirty = not $ eq
          (unwrap (Internal.formFieldsToInputFields stt.form))
          (unwrap (unwrap stt.internal).initialInputs)

    -- Need to verify the validity status of the form.
    pure $ Left $ case (unwrap stt.internal).allTouched of
      true -> stt
        { validity = if not (stt.errors == 0) then Invalid else Valid
        , errors = errors
        , dirty = dirty
        }

      -- If not all fields are touched, then we need to quickly sync the form state
      -- to verify this is actually the case.
      _ -> case Internal.allTouched stt.form of

        -- The sync revealed all fields really have been touched
        true -> stt
          { validity = if not (stt.errors == 0) then Invalid else Valid
          , internal = over InternalState (_ { allTouched = true }) stt.internal
          , errors = errors
          , dirty = dirty
          }

        -- The sync revealed that not all fields have been touched
        _ -> stt { validity = Incomplete, errors = errors, dirty = dirty }

  -- Run submission without raising messages or replies
  runSubmit stt = do
    let init = stt
          { submitAttempts = stt.submitAttempts + 1
          , submitting = true
          }

    -- For performance purposes, avoid running this if possible
    let internal = unwrap init.internal
    let init' =
          if not internal.allTouched
            then init
              { form = Internal.setFormFieldsTouched init.form
              , internal = over InternalState (_ { allTouched = true }) init.internal
              }
            else init

    -- Necessary to validate after fields are touched, but before parsing
    est <- eval ValidateAll init'
    case est of
      -- This should never happen, because just validation can't submit the form
      Right form -> pure (Right form)
      -- This will always happen
      Left validated -> do
        -- For performance purposes, only attempt to submit if the form is valid
        let validated' = validated { submitting = false }
        pure $
          if validated'.validity == Valid
          then case Internal.formFieldsToMaybeOutputFields validated.form of
            Nothing -> Left validated'
            Just form -> Right form
          else Left validated'
