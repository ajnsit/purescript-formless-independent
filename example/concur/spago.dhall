{-
Welcome to a Spago project!
You can edit this file as you like.
-}

let mkPackage = ../../mkPackage.dhall

in  { name =
        "purescript-formless-concur"
    , dependencies =
        [ "concur-react", "formless-aj" ]
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
            , concur-react =
                mkPackage
                [ "aff"
                , "arrays"
                , "avar"
                , "console"
                , "foldable-traversable"
                , "free"
                , "nonempty"
                , "react"
                , "react-dom"
                , "tailrec"
                , "web-dom"
                , "web-html"
                ]
                "https://github.com/ajnsit/purescript-concur.git"
                "master"
            }
    , sources =
        [ "src/**/*.purs", "test/**/*.purs" ]
    }
