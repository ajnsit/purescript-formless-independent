module Validation where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (length) as Foldable
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString) as Int
import Data.Lens (preview)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Data.String (contains, length, null)
import Data.String.Pattern (Pattern(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Formless (FormFieldResult, _Error)
import Formless as F
import Formless.Validation (Validation(..), hoistFnE_)
import Pets (Pet(..))
import Type.Data.Symbol (SProxy(..))

data FieldError
  = EmptyField
  | InvalidEmail
  | EmailInUse
  | TooShort Int
  | TooLong Int
  | InvalidInt String
  | NotEqual String String
  | NotEnoughMoney
  | MustLikePets

derive instance genericFieldError :: Generic FieldError _
instance showFieldError :: Show FieldError where
  show = genericShow

instance toTextFieldError :: ToText FieldError where
  toText EmptyField = "This field is required."
  toText InvalidEmail = "That email is not valid."
  toText EmailInUse = "That email is already being used."
  toText (TooShort n) = "You must enter at least " <> show n <> " characters."
  toText (TooLong n) = "You must enter less than " <> show n <> " characters."
  toText (InvalidInt str) = "Could not parse \"" <> str <> "\" to a valid integer."
  toText (NotEqual str0 str1) = "This field contains \"" <> str1 <> "\" but must be equal to \"" <> str0 <> "\" to validate."
  toText (NotEnoughMoney) = "You don't have that much money."
  toText (MustLikePets) = "You MUST like pets to continue"

-- | Some useful types we'll parse to
newtype Name = Name String
derive instance newtypeName :: Newtype Name _
derive newtype instance showName :: Show Name

newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _
derive newtype instance eqEmail :: Eq Email
derive newtype instance showEmail :: Show Email

-- | Unpacks errors to render as a string
showError :: ∀ e o. ToText e => FormFieldResult e o -> Maybe String
showError = map toText <<< preview _Error

class ToText item where
  toText :: item -> String

instance toTextString :: ToText String where
  toText = identity

--------------------
-- Formless Validation
--------------------

mustLikePets :: ∀ form m. Monad m => Validation form m FieldError Pet Pet
mustLikePets = hoistFnE_ $ \pet -> case pet of
  Neither -> Left MustLikePets
  _ -> pure pet

emailFormat :: ∀ form m. Monad m => Validation form m FieldError String Email
emailFormat = hoistFnE_ $ \str ->
  if contains (Pattern "@") str
    then pure $ Email str
    else Left InvalidEmail

-- Validate that the current field is equal to another field named "email1"
equalsEmail1
  :: ∀ form m e o fields
   . Monad m
   -- Specify that the form must have an email1 field
   => Newtype (form Record F.FormField) { email1 :: F.FormField e String o | fields }
   => Validation form m FieldError String String
equalsEmail1 = F.hoistFnE $ \form str ->
  let e1 = F.getInput (SProxy :: SProxy "email1") form
   in if str == e1
        then Right str
        else Left $ NotEqual str e1

minLength :: ∀ form m. Monad m => Int -> Validation form m FieldError String String
minLength n = hoistFnE_ $ \str ->
  let n' = length str
   in if n' < n then Left (TooShort n) else Right str

-- | The opposite of minLength.
maxLength :: ∀ form m. Monad m => Int -> Validation form m FieldError String String
maxLength n = hoistFnE_ \str ->
  let n' = length str
   in if n' > n then Left (TooLong n) else Right str

exists :: ∀ form m a. Monad m => Validation form m FieldError (Maybe a) a
exists = hoistFnE_ $ maybe (Left EmptyField) Right

strIsInt :: ∀ form m. Monad m => Validation form m FieldError String Int
strIsInt = hoistFnE_ $ \str -> maybe (Left $ InvalidInt str) Right (Int.fromString str)

nonEmptyArray :: ∀ form m a. Monad m => Validation form m FieldError (Array a) (Array a)
nonEmptyArray = hoistFnE_ \arr ->
  if Foldable.length arr > 0
    then Right arr
    else Left EmptyField

-- | Validate that an input string is not empty
nonEmptyStr :: ∀ form m. Monad m => Validation form m FieldError String String
nonEmptyStr = hoistFnE_ $ \str ->
  if null str
    then Left EmptyField
    else Right str

--------------------
-- Formless Async Validation
--------------------

emailIsUsed :: ∀ form m. MonadAff m => Validation form m FieldError Email Email
emailIsUsed = Validation \_ e@(Email e') -> do
  -- Perhaps we hit the server to  if the email is in use
  _ <- liftAff $ delay $ Milliseconds 1000.0
  pure $ if (contains (Pattern "t") e')
    then pure e
    else Left EmailInUse

enoughMoney :: ∀ form m. MonadAff m => Validation form m FieldError Int Int
enoughMoney = Validation \_ i -> do
  -- Let's check if we have enough money...
  _ <- liftAff $ delay $ Milliseconds 5000.0
  pure $ if (i > 1000)
    then pure i
    else Left NotEnoughMoney
