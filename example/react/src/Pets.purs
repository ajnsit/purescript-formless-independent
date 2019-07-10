module Pets where

import Data.Bounded (class Bounded)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
import Data.Ord (class Ord)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Menu (class IsOption)

data Pet = Cats | Dogs | Neither
derive instance genericPet ∷ Generic Pet _
derive instance eqPet :: Eq Pet
derive instance ordPet :: Ord Pet
instance enumPet :: Enum Pet where
  succ = genericSucc
  pred = genericPred
instance boundedPet :: Bounded Pet where
  top = genericTop
  bottom = genericBottom
instance boundedEnumPet :: BoundedEnum Pet where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
instance showPet :: Show Pet where
  show = genericShow
instance isOptionPet :: IsOption Pet where
  toOptionValue = show
  toOptionLabel x = "I like " <> show x
  fromOptionValue "Cat" = Cats
  fromOptionValue "Dog" = Dogs
  fromOptionValue _ = Neither
