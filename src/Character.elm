module Character (Model, init, new, Action, update, view, Context) where

import Utilities exposing (..)
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

type Action
  = ModifyLevel Int
  | ModifyName String

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    ModifyLevel level ->
      ({ model | level = level }, Effects.none)
    ModifyName name ->
      ({ model | name = name }, Effects.none)

-- VIEW

type alias Context =
  { actions : Signal.Address Action
  , remove : Signal.Address ()
  }

view : Context -> Model -> Html
view context model =
  div
    [ class "character" ]
    [ label
        [ for "character-level" ]
        [ text "Level" ]
    , levelOptionsView context.actions model
    , input
        [
          class "character-name"
        , type' "text"
        , value (model.name)
        , on "input" targetValue (\name -> Signal.message context.actions (ModifyName name))
        ] []
    , button
        [ onClick context.remove () ]
        [ text "Remove" ]
    ]

levelOptionsView : Signal.Address Action -> Model -> Html
levelOptionsView address model =
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
        levelList
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
