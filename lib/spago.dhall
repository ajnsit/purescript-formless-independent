{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "formless-aj"
, dependencies =
  [ "aff"
  , "datetime"
  , "effect"
  , "heterogeneous"
  , "profunctor-lenses"
  , "variant"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
