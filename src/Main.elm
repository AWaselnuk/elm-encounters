import Encounter
import Html.App as Html

main =
  Html.program
    { view = Encounter.view
    , update = Encounter.update
    , init = Encounter.init
    , subscriptions = \_ -> Sub.none
    }
