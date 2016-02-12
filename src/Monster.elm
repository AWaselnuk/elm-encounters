module Monster (Model, init, new, Action, update, view, Context) where

import Utilities exposing (safeRatingToXP, safeStrToLevel, restrictXP, restrictRating)
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String

-- MODEL

type alias Model =
  { xp : Int
  , rating : Float 
  , name : String 
  }

-- UPDATE

type Action
  = ModifyXP Int
  | ModifyRating Float
  | ModifyName String

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    ModifyXP xp ->
      ({ model | xp = restrictXP xp }, Effects.none)
    ModifyRating rating ->
      ({ model | rating = restrictRating rating }, Effects.none)
    ModifyName name ->
      ({ model | name = name }, Effects.none)

-- VIEW

type alias Context =
  { actions : Signal.Address Action
  , remove : Signal.Address ()
  }

view : Context -> Model -> Html
view context monster =
  div
    [ class "monster" ]
    [
      input
        [
          class "monster-xp"
        , type' "number"
        , value (toString monster.xp)
        , on "input" targetValue (\xp -> Signal.message context.actions (ModifyXP (safeStrToLevel xp)))
        ] []
    , input
        [
          class "monster-name"
        , type' "text"
        , value (monster.name)
        , on "input" targetValue (\name -> Signal.message context.actions (ModifyName name))
        ] []
    , button
        [ onClick context.remove () ]
        [ text "Remove" ]
    ]

init : Model
init =
  new 1 randomName

new : Float -> String -> Model
new rating name =
  { xp = safeRatingToXP rating 
  , name = if String.length name == 0 then randomName else name 
  , rating = rating 
  }

randomName : String
randomName =
  "Monster Name"
