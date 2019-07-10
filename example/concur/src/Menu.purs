module Menu where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Category ((<<<))
import Data.Bounded (bottom)
import Data.Enum (class BoundedEnum, upFromIncluding)
import Data.Function (($))
import Data.Functor ((<#>), (<$>))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Formless as F
import Prim.Row (class Cons)

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
  -> Widget HTML (F.Query form)
menu form field = D.select
  [ P.defaultValue $ toOptionValue $ F.getInput field form
    , (F.set field <<< fromOptionValue <<< P.unsafeTargetValue) <$> P.onChange
    ]
  (upFromIncluding (bottom :: opt) <#> \opt ->
    D.option [P.value (toOptionValue opt)] [D.text (toOptionLabel opt)])
