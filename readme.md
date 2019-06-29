# Concur-Formless

Concur-Formless is a library for [Concur](https://github.com/ajnsit/purescript-concur) which helps you build forms. Provide Formless with some initial inputs, and validation to run on those inputs, and it will handle the tedious parts of managing form state, errors, submission, and more.

You can write a complete Concur form component with multiple fields, validation, parsing, and errors in less than 100 lines of code (only ~20 lines of which are from Formless).

*TODO: Write more examples*

### Attribution

Concur-Formless is basically a port of [Formless](https://github.com/thomashoneyman/purescript-halogen-formless) for Halogen to Concur.

### Installation

Install with Bower:

```sh
bower i --save purescript-concur-formless
```

*TODO: Use Spago*

# Overview

Concur makes writing forms [easy](https://github.com/ajnsit/concur-documentation#replicating-elm-architecture), but handling changes for each field and their validations, and then handling errors can quickly become tedious. Formless helps abstract away most of the messy details of managing form state without imposing any restrictions on how you render your form.

To demonstrate, let's build a signup form in Formless.

## Data Types

We'll start with the data type we want our form to result in: a `User`.

```purescript
type User =
  { id :: Int
  , name :: String
  , email :: Email
  }
```

This is the data type we'll use throughout our application, but our form will have different fields altogether: we want them to provide two email addresses for confirmation purposes, and we don't have an ID for them until the form has been submitted.

Formless requires a specific shape from your `Form` data type. You are expected to write a newtype that takes two arguments, `r` and `f` below, and a row containing the fields in your form.

<details>
  <summary>Expand to read about these two type arguments</summary>
The first argument has the kind `(# Type -> Type)` and turns a row of types into a concrete type. For example, you can fill in `Record` to get a record; `Record (name :: String)` is the same as `{ name :: String }`. However, Formless will often fill in `Variant` internally. This lets the library access the entire form at once (`Record`) or a single field (`Variant`) to perform various operations. The important thing is that you make sure this variable is left free in your `Form` newtype.

The second argument has the kind `(Type -> Type -> Type -> Type)` and will be filled in with one of many types Formless uses internally to manage your form. It expects an error type, an input type, and an output type for the field in question.

</details>

Every field should use the second argument, `f`, and provide it with three type arguments:

- an `error` type, which represents possible validation errors for the field
- an `input` type, which represents the value the user will provide when interacting with the field
- an `output` type, which represents the type you'd like to result from successful validation

Here's what our form type looks like:

```purescript
-- Note: Common practice to use `Void` to represent "no error possible"
newtype Form r f = Form (r
  ( name   :: f Error String String -- | String input to String output, or Error on failed validation
  , email1 :: f Error String Email  -- | String input to Email output, or Error on failed validation
  , email2 :: f Error String Email  -- | String input to Email output, or Error on failed validation
  ))
derive instance newtypeForm :: Newtype (Form r f) _
```

Formless will use this type to perform all kinds of transformations and track data about your form over time. You simply need to decide what fields will exist and what their error, input, and output types are.

<details>
  <summary>Expand to read a longer explanation of this form type</summary>

This can be a scary type to look at, but it's not so bad once you provide concrete types for `r` and `f`. For example, let's try providing `Record` and the `OutputType` type from Formless:

```purescript
-- This type synonym will throw away most of its arguments, preserving only the last type. Since
-- it takes three arguments, it fits the kind (Type -> Type -> Type -> Type), which is exactly what
-- we need to provide as our `Form` newtype's second argument.
type OutputType e i o = o

-- Let's fill in each occurrence of `f` with `OutputType`
myForm :: Form Record OutputType
myForm = Form
  { name   :: OutputType Error String String
  , email1 :: OutputType Error String Email
  , email2 :: OutputType Error String Email
  }

-- This isn't much less confusing, so let's take things a step further. What if we act as the
-- compiler does and erase the type synonym? After all, OutputType is equivalent to only the
-- third type argument from each field.
myForm2 :: Form Record OutputType
myForm2 = Form
  { name   :: String
  , email1 :: Email
  , email2 :: Email
  }

-- `myForm` and `myForm2` are exactly equivalent! Accepting a type that itself accepts three
-- arguments allows us to represent several different sorts of records and variants from the
-- same underlying row and can result in quite simple data types despite the admittedly
-- complicated-looking original type.

```

</details>

<details>
  <summary>Expand to see the definition of the <code>Error</code> and <code>Email</code> types</summary>

```purescript
newtype Email = Email String

data Error
  = Required
  | NotEqual String String
  | EmailIsUsed
  | EmailInvalid
```

</details>

## Form Inputs

We need a few more types to specify our form behaviour. While we'll take a closer look at each of these types in the next few sections, here's a quick primer on what these types are:

- `initialInputs`: Your `Form` newtype around a record, where each field contains its initial, starting value
- `validators`: Your `Form` newtype around a record, where each field contains a validation function which will process its input value. Our validations can run in any monad, so it can do things like make ajax calls. Validation functions can run in `Identity` if they perform no effects, or something like `Aff` for ajax calls.

```purescript
import Formless as F

initialInputs :: Form Record F.InputField
validators :: Form Record (F.Validation Form Aff)
```

### Form Inputs

The first thing Formless requires is a record of the fields in your form with their initial values. It has the type `Form Record F.InputField`. Remember: `Form` is our custom newtype we defined a moment ago, and it was awaiting a type that would be applied to the error, input, and output types we defined for each field -- like `F.InputField`!

```purescript
newtype InputField error input output = InputField input
```

Applied to our form, an `InputField` represents the input type only. We can give Formless a valid record of inputs by just supplying concrete input values for each field:

```purescript
inputs :: Form Record F.InputField
inputs = Form
  { name: InputField ""
  , email1: InputField ""
  , email2: InputField ""
  }
```

It's a little tedious writing out all those newtypes, so `Formless.Spec.Transform` provides helper functions to generate them for you:

```purescript
inputs :: Form Record F.InputField
inputs = F.wrapInputFields
  { name: ""
  , email1: ""
  , email2: ""
  }
```

In fact, you don't even have to do this: if your input types belong to the `Formless.Initial` type class (all monoidal values do), it can generate the values for you from a proxy for your form:

```purescript
proxy = F.FormProxy :: F.FormProxy Form

inputs :: Form Record F.InputField
inputs = F.mkInputFields proxy
```

### Validation

The next thing Formless requires is a record of validators: functions that will be run on the form to validate the inputs and produce the specified output types. Every field in this record ought to use the Formless `Validation` type:

```purescript
newtype Validation form m error input output
  = Validation (form Record FormField -> input -> m (Either error output))
```

This type represents a function which takes your entire form, the input for this particular field, and produces either an error or result.

- This function can be monadic, so you can do things like confirm with a server that an email is not already in use.
- This function takes your entire form as an argument, so you can use the values of other fields during validation. For example, you could verify that two password fields are equal to one another.
- If you are using `purescript-validation` and already have a composed validation function that results in `V`, then you can convert it into a Formless validator with `hoistFnE_ <<< Data.Validation.Semigroup.toEither` (or the `Semiring` module).

The `FormField` newtype represents the state of every field in the form:

```purescript
newtype FormField error input output = FormField
  { -- The value the user will input
    input :: input
    -- Whether the field has been modified yet (validators ignore untouched fields)
  , touched :: Boolean
    -- The result of validation, IF validation has been run on this field
  , result :: FormFieldResult error output
  }
```

A field's result can be in one of several states, represented by the `FormFieldResult` type:

```purescript
data FormFieldResult e o
  = NotValidated
  | Validating -- Useful to display a loading spinner during asynchronous / long validations
  | Error e
  | Success o
```

Let's see some examples of validators written in this style:

```purescript
-- This helper function lets you take any function from `input` to `output` and turns it into
-- the Validation type from Formless.
hoistFn_ :: ∀ form m e i o. Monad m => (i -> o) -> Validation form m e i o

-- For example, this validator simply transforms the input `Int` into a `String` using `hoistFn_`
-- output.
myStringValidator :: ∀ form m. Monad m => Validation form m Void Int String
myStringValidator = hoistFn_ show

-- This helper function lets you take any function from `input` to `Either error output` and turns
-- it into the Validation type from Formless.
hoistFnE_ :: ∀ form m e i o. Monad m => (i -> Either e o) -> Validation form m e i o

-- For example, this validator makes sure that the string is not empty
isNonEmpty :: ∀ form m. Monad m => Validation form m Error String String
isNonEmpty = hoistFnE_ $ \str ->
  if null str
     then Left Required
     else Right str

-- This validator transforms the input into an `Email` type if successful.
validEmail :: ∀ form m. Monad m => Validation form m Error String Email
validEmail = hoistFnE_ $ \str ->
  if contains (Pattern "@") str
     then Right (Email str)
     else Left EmailInvalid

-- Continuing the trend, this helper takes a function from `input` to a monad `m (Either error output)` and
-- turns it into the Validation type from Formless.
hoistFnME_ :: ∀ form m e i o. Monad m => (i -> m (Either e o)) -> Validation form m e i o

-- For example, this validator makes sure that an email address is not in use. Notice how it relies
-- on the input value already being an `Email` -- we'll see how to chain validators together so this
-- can be used with `validEmail` in a moment.
emailNotUsed :: ∀ form. Validation form Aff Error Email Email
emailNotUsed = hoistFnME_ $ \email -> do
  isUsed <- checkEmailIsUsed :: Email -> Aff Boolean
  pure $
    if isUsed
      then Right email
      else Left EmailIsUsed

-- Now, let's do something a little more complex. Let's validate that two fields are equal to one another.

-- This time, we want to rely on our existing `Form` as an argument for our validation, so instead of using
-- `hoistFnE_` we'll reach for `hoistFnE`, which doesn't throw away the form argument.
hoistFnE :: ∀ form m e i o. Monad m => (form Record FormField -> i -> Either e o) -> Validation form m e i o

-- We'll use `getInput` from Formless to retrieve the input value of the field "email1" from the form, and then
-- we'll validate that the current field is equal to it. Formless can prove that a "email1" field exists using
-- your form row, so you'll never access a value you don't have.
equalsEmail1 :: ∀ m. Monad m => Validation Form m Error String String
equalsEmail1 = hoistFnE $ \form str ->
  let e1 = F.getInput (SProxy :: SProxy "email1") form
   in if str == e1
        then Right str
        else Left $ NotEqual str e1
```

These validators are building blocks that you can compose together to validate any particular field. Now that we've got some validation functions we can provide our `validators` record to Formless:

```purescript
validators :: Form Record (F.Validation Form Aff)
validators = Form
  { name: isNonEmpty
  , email1: isNonEmpty >>> validEmail >>> emailNotUsed
  , email2: isNonEmpty >>> equalsEmail1 >>> emailNotUsed
  }
```

Note how validators can be composed: `validEmail` takes a `String` and produces an `Email`, which is then passed to `emailNotUsed`, which takes an `Email` and produces an `Email`. You can use this to build up validators that change a field's output type over time. Composition with `>>>` will short-circuit on the first failure.

### Building the form

The last thing you're expected to do is compose the form fields together into a form. Formless allows you to handle rendering in any way you want, and provides a few parts to plumb pieces together.

While rendering the form, you can use helper functions to get various parts of a field, given a field label; these include `getInput`, `getResult`, `getError`, and more.

User events on your form should return values of the type `Query form` which represents all the actions you can take on a form data in response to user input. You can use the various helper functions defined in `Formless.Query` to construct them. Then you can use `eval` to apply the changes to the form data.

```purescript
type Query form
eval :: Query form -> m (Maybe (form Record OutputField))
```

`eval` returns `Just output` if the form was successfully submitted, or `Nothing` if the user did not submit the form, or the submission was not successful.

Here's some more information on the query helpers:
- You should use `F.set` to set a field's value, `F.modify` to modify a field with a function, `F.validate` to validate fields, and `F.setValidate` or `F.modifyValidate` to do both at the same time
- You should use `F.submit` to submit the form
- If you want to avoid running expensive or long-running validations on each key press, use the asynchronous versions (`F.asyncSetValidate`, etc.) and provide a number of milliseconds to debounce. You can use `getResult` to show a loading spinner when the result is `Validating`.
- If you need to chain multiple operations, you can use `F.andThen` to provide multiple Formless queries

Let's write a form widget using `setValidate`, `asyncSetValidate`, and `getInput`, using symbol proxies we've defined in the `where` clause. We use `StateT` to allow access to the form state:

```purescript
formStWidget :: ∀ form. StateT (F.State form Aff) (Widget HTML) (form Record F.OutputField)
formStWidget = do
  fstate <- get
  query <- D.div'
    [ D.input
      [ P.value $ F.getInput _name fstate.form
      , (F.set _name <<< P.unsafeTargetValue) <$> P.onChange
      ]
    , D.input
      [ P.value $ F.getInput _email1 fstate.form
        -- This will help us avoid hitting the server on every single key press.
      , (F.asyncSetValidate debounceTime _email1 <<< P.unsafeTargetValue) <$> P.onChange
      ]
    , D.input
      [ P.value $ F.getInput _email2 fstate.form
      , (F.asyncSetValidate debounceTime _email2 <<< P.unsafeTargetValue) <$> P.onChange
      ]
    ]
  res <- F.eval query
  maybe formStWidget pure res
  where
    _name = SProxy :: SProxy "name"
    _email1 = SProxy :: SProxy "email1"
    _email2 = SProxy :: SProxy "email2"
    debounceTime = Milliseconds 300.0
```

It can be tedious to write out symbol proxies for every field you want to access in a form. You can instead generate a record of these proxies automatically using the `mkSProxies` function:

```purescript
prx :: F.SProxies Form
prx = F.mkSProxies (F.FormProxy :: F.FormProxy Form)

-- These are now equivalent
x = SProxy :: SProxy "name"
x = prx.name
```

Now, instead of writing out proxies over and over, you can just import the proxies record!

## Using the Widget elsewhere

Whew! Now we have a nicely encapsulated form which can be used on other parts of our application.

```purescript
import Formless as F

main :: Effect Unit
main = runWidgetInDom "form" (formWidget initialInputs formValidators)

formWidget :: InputForm -> Validators -> Widget HTML (form Record F.OutputField)
formWidget initForm initValidators = fst <$> runStateT formStWidget (initState initForm initValidators)

page :: forall a. Widget HTML a
page = do
  out <- D.div'
    [ D.h1' [D.text "My Form"]
    , formWidget initialInputs validators
    ]
  let form = F.unwrapOutputFields out
  -- Assuming some effectful computation to receive the ID
  id <- registerUser { name: form.name, email: form.email1 }
  let user = { name: form.name, email: form.email, id }
  liftEffect $ Console.log $ "Got a user! " <> show (user :: User)
```
