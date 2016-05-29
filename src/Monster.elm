module Monster exposing (Model, init, new, Msg, update, view, Context)

import Utilities exposing (..)
import StatTables
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

type Msg
  = ModifyXP Int
  | ModifyRating Float
  | ModifyName String

update : Msg -> Model -> (Model, Effects Msg)
update msg model =
  case msg of
    ModifyRating rating ->
      ({ model |
           rating = rating,
           xp = safeRatingToXP rating }
       , Effects.none)
    ModifyXP xp ->
      ({ model |
           rating = safeXPToRating xp,
           xp = xp }
       , Effects.none)
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
    [ class "monster" ]
    [ label
        [ for "monster-rating" ]
        [ text "Challenge Rating" ]
    , monsterRatingOptionsView context.msgs model
    , label
        [ for "monster-xp" ]
        [ text "Experience Points" ]
    , monsterXpOptionsView context.msgs model
    , label
        [ for "monster-name" ]
        [ text "Name" ]
    , input
        [
          class "monster-name"
        , type' "text"
        , value (model.name)
        , on "input" targetValue (\name -> Signal.message context.msgs (ModifyName name))
        ] []
    , button
        [ onClick context.remove () ]
        [ text "Remove" ]
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
      , on "change" targetValue (\rating -> Signal.message address (ModifyRating (safeStrToRating rating)))
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
      , on "change" targetValue (\xp -> Signal.message address (ModifyXP (safeStrToLevel xp)))
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
