{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "formless-independent"
, dependencies =
    [ "aff"
    , "console"
    , "datetime"
    , "effect"
    , "generics-rep"
    , "heterogeneous"
    , "profunctor-lenses"
    , "psci-support"
    , "variant"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
