import Encounter

import StartApp
import Task
import Effects
import Html

app : StartApp.App Encounter.Model
app =
  StartApp.start
    { view = Encounter.view
    , update = Encounter.update
    , init = Encounter.init
    , inputs = []
    }

main : Signal Html.Html
main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
