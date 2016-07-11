module Character exposing (Model, init, new, Msg, update, view)

import Utilities exposing (..)
import StatTables
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ModifyLevel level ->
      ({ model | level = level }, Cmd.none)
    ModifyName name ->
      ({ model | name = name }, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div
    [ class "character" ]
    [ label
        [ for "character-level" ]
        [ text "Level" ]
    , levelOptionsView model
    , label
        [ for "character-name" ]
        [ text "Name" ]
    , input
        [
          class "character-name"
        , type' "text"
        , value (model.name)
        , onInput ModifyName
        ] []
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
      , on "change" (Json.map ModifyLevel targetValueIntDecoder)
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
