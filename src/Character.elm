module Character (Model, init, new, Action, update, view, Context) where

import Utilities exposing (restrictLevel, safeStrToLevel)
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
      ({ model | level = restrictLevel level }, Effects.none)
    ModifyName name ->
      ({ model | name = name }, Effects.none)

-- VIEW

type alias Context =
  { actions : Signal.Address Action
  , remove : Signal.Address ()
  }

view : Context -> Model -> Html
view context character =
  div
    [ class "character" ]
    [
      input
        [
          class "character-level"
        , type' "number"
        , value (toString character.level)
        , on "input" targetValue (\level -> Signal.message context.actions (ModifyLevel (safeStrToLevel level)))
        ] []
    , input
        [
          class "character-name"
        , type' "input"
        , value (character.name)
        , on "input" targetValue (\name -> Signal.message context.actions (ModifyName name))
        ] []
    , button
        [ onClick context.remove () ]
        [ text "Remove" ]
    ]

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
