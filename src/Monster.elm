module Monster exposing (Model, init, new, Msg, update, view)

import Utilities exposing (..)
import StatTables
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import String

-- MODEL

type alias Model =
  { xp : Int
  , rating : Float
  , name : String
  }

-- UPDATE

type Msg
  = ModifyXP Int
  | ModifyRating Float
  | ModifyName String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ModifyRating rating ->
      ({ model |
           rating = rating,
           xp = safeRatingToXP rating }
       , Cmd.none)
    ModifyXP xp ->
      ({ model |
           rating = safeXPToRating xp,
           xp = xp }
       , Cmd.none)
    ModifyName name ->
      ({ model | name = name }, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div
    [ class "inline-form" ]
    [ div
        [ class "inline-form-control monster-rating-wrapper" ]
        [ monsterRatingOptionsView model ]
    , div
        [ class "inline-form-control monster-xp-wrapper" ]
        [ monsterXpOptionsView model ]
    , div
        [ class "inline-form-control monster-name-wrapper" ]
        [ input
            [
              class "monster-name"
            , type' "text"
            , value (model.name)
            , onInput ModifyName
            ] []
        ]
    ]

monsterRatingOptionsView : Model -> Html Msg
monsterRatingOptionsView model =
  let
    monsterRatingOption rating isSelected =
      option
        [ value rating
        , selected isSelected
        ]
        [ text rating ]
    monsterRatingOptions =
      List.map
        (\rating -> monsterRatingOption (toString rating) (rating == model.rating))
        StatTables.ratingList
  in
    select
      [ name "monster-rating"
      , on "change" (Json.map ModifyRating targetValueFloatDecoder)
      ]
      monsterRatingOptions

monsterXpOptionsView : Model -> Html Msg
monsterXpOptionsView model =
  let
    monsterXpOption xp isSelected =
      option
        [ value xp
        , selected isSelected
        ]
        [ text xp ]
    monsterXpOptions =
      List.map
        (\xp -> monsterXpOption (toString xp) (xp == model.xp))
        StatTables.xpList
  in
    select
      [ name "monster-xp"
      , on "change" (Json.map ModifyXP targetValueIntDecoder)
      ]
      monsterXpOptions

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
