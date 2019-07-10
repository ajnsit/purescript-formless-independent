module Main where

import Prelude

import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Forms (formWidget, initialInputs, validators)
import Forms as F
import Partial.Unsafe (unsafePartial)
import React as React
import React.DOM as D
import ReactDOM as ReactDOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM

main :: Effect Unit
main = void $ do
  window <- DOM.window
  document <- DOM.document window
  let node = DOM.toNonElementParentNode document
  element <- DOM.getElementById "root" node
  let element' = unsafePartial (fromJust element)
  ReactDOM.render (React.createLeafElement mainClass { }) element'

mainClass :: React.ReactClass { }
mainClass = React.component "Main" component
  where
  component this =
    pure { state: {user: Nothing}
         , render: render <$> React.getState this
         }
    where
    render {user: Just user} =
      D.div [ ]
      [ D.h1 [] [D.text "Got a user!"]
      , D.h2 [] [D.text $ "Got a user! " <> show user]
      ]
    render {user: Nothing} =
      D.div [ ]
        [ D.h1 [] [D.text "My Form"]
        , React.createLeafElement initialisedFormWidget {}
        ]
    initialisedFormWidget =
      formWidget
        (F.initState initialInputs validators)
        \user -> React.setState this {user: Just user}
