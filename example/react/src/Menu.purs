module Menu where

import Control.Applicative (pure)
import Data.Bounded (bottom)
import Data.Enum (class BoundedEnum, upFromIncluding)
import Data.Function (($))
import Data.Functor ((<#>), (<$>))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Unit (Unit)
import Data.Variant (Variant)
import Effect (Effect)
import Formless as F
import Prim.Row (class Cons)
import React as React
import React.DOM as D
import React.DOM.Props as P
import Unsafe.Coerce (unsafeCoerce)

class IsOption a where
  toOptionValue :: a -> String
  toOptionLabel :: a -> String
  fromOptionValue :: String -> a

menu
  :: forall opt s form e o restF restI inputs fields
   . IsSymbol s
   => IsOption opt
   => BoundedEnum opt
   => Newtype (form Record F.FormField) (Record fields)
   => Cons s (F.FormField e opt o) restF fields
   => Newtype (form Variant F.InputFunction) (Variant inputs)
   => Cons s (F.InputFunction e opt o) restI inputs
   => form Record F.FormField
  -> SProxy s
  -> (F.Query form -> Effect Unit)
  -> React.ReactClass {}
menu form field queryHandler = React.component "Menu" component
  where
    component this =
      pure { state: {}
           , render: render <$> React.getState this
           }
    render {} =
      D.select
        [ P.defaultValue $ toOptionValue $ F.getInput field form
        ,  P.onChange onChange
        ]
        (upFromIncluding (bottom :: opt) <#> \opt ->
          D.option [P.value (toOptionValue opt)] [D.text (toOptionLabel opt)])
    onChange event = queryHandler (F.set field (fromOptionValue (unsafeCoerce event).target.value))
