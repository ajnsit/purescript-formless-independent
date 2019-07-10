module Main where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Run (runWidgetInDom)
import Data.Show (show)
import Effect (Effect)
import Forms as F
import Prelude (Unit, bind, ($), (<>))

main :: Effect Unit
main = runWidgetInDom "root" page

formWidget :: Widget HTML F.User
formWidget = F.formWidget (F.initState F.initialInputs F.validators)

page :: Widget HTML Unit
page = do
  user <- D.div'
    [ D.h1' [D.text "My Form"]
    , formWidget
    ]
  -- Assuming some effectful computation to receive the ID
  -- id <- registerUser user
  -- let user = { name: form.name, email: form.email, id }
  -- liftEffect $ Console.log $ "Got a user! " <> show (user :: F.User)
  D.h2' [D.text $ "Got a user! " <> show user]
