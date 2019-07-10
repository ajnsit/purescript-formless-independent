{-
Welcome to a Spago project!
You can edit this file as you like.
-}

let mkPackage = ../../mkPackage.dhall

in  { name =
        "purescript-formless-concur"
    , dependencies =
        [ "react", "react-dom", "web-html", "formless-aj" ]
    , packages =
            ../../packages.dhall
        //  { formless-aj =
                { repo =
                    "../../lib"
                , version =
                    ""
                , dependencies =
                    (../../lib/spago.dhall).dependencies
                }
            }
    , sources =
        [ "src/**/*.purs", "test/**/*.purs" ]
    }
