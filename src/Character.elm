module Character exposing (Model, init, new, Msg, update, view, Context)

import Utilities exposing (..)
import StatTables
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String

-- MODEL

type alias Model =
  { level : Int
  , name : String
  }

-- UPDATE

type alias ID = Int

type Msg
  = ModifyLevel Int
  | ModifyName String

update : Msg -> Model -> (Model, Effects Msg)
update msg model =
  case msg of
    ModifyLevel level ->
      ({ model | level = level }, Effects.none)
    ModifyName name ->
      ({ model | name = name }, Effects.none)

-- VIEW

type alias Context =
  { msgs : Signal.Address Msg
  , remove : Signal.Address ()
  }

view : Context -> Model -> Html
view context model =
  div
    [ class "character" ]
    [ label
        [ for "character-level" ]
        [ text "Level" ]
    , levelOptionsView context.msgs model
    , label
        [ for "character-name" ]
        [ text "Name" ]
    , input
        [
          class "character-name"
        , type' "text"
        , value (model.name)
        , on "input" targetValue (\name -> Signal.message context.msgs (ModifyName name))
        ] []
    , button
        [ onClick context.remove () ]
        [ text "Remove" ]
    ]

levelOptionsView : Model -> Html Msg
levelOptionsView model =
  let
    levelOption level isSelected =
      option
        [ value level
        , selected isSelected
        ]
        [ text level ]
    levelOptions =
      List.map
        (\level -> levelOption (toString level) (level == model.level))
        StatTables.levelList
  in
    select
      [ name "character-level"
      , on "change" targetValue (\level -> Signal.message address (ModifyLevel (safeStrToLevel level)))
      ]
      levelOptions

init : Model
init =
  new 1 randomName

new : Int -> String -> Model
new level name =
  { level = level
  , name = if String.length name == 0 then randomName else name
  }

randomName : String
randomName =
  "PC Name"
