module MonsterList exposing (..)

import Utilities exposing (..)
import StatTables
import Monster
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json

-- MODEL

type alias ID = Int

type alias MonsterList = List (ID, Monster.Model)

type alias Model =
  { uid : ID
  , monsterList : MonsterList
  , monsterXpTotal : Float
  , newMonsterName : String
  , newMonsterRating : Float
  , newMonsterXP : Int
  }

init : Model
init =
  { uid = 1
  , monsterList = []
  , monsterXpTotal = 0
  , newMonsterName = ""
  , newMonsterRating = initRating
  , newMonsterXP = safeRatingToXP initRating }

-- UPDATE

type Msg
  = AddMonster Monster.Model
  | RemoveMonster ID
  | ModifyMonster ID Monster.Msg
  | SetNewMonsterName String
  | SetNewMonsterXP Int
  | SetNewMonsterRating Float

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddMonster monster ->
      let
        newMonsterList = (model.uid, monster) :: model.monsterList
      in
        ({ model |
             monsterList = newMonsterList,
             monsterXpTotal = calculateMonsterXPTotal newMonsterList,
             uid = model.uid + 1 }
        , Cmd.none)
    RemoveMonster id ->
      let
        newMonsterList = List.filter (\(monsterID, _) -> monsterID /= id) model.monsterList
      in
        ({ model |
             monsterList = newMonsterList,
             monsterXpTotal = calculateMonsterXPTotal newMonsterList }
        , Cmd.none)
    ModifyMonster id monsterMsg ->
      let
        updateMonster (monsterID, monsterModel) =
          if id == monsterID then
            (monsterID, fst <| Monster.update monsterMsg monsterModel)
          else
            (monsterID, monsterModel)
        newMonsterList = List.map updateMonster model.monsterList
      in
        ({ model |
             monsterList = newMonsterList,
             monsterXpTotal = calculateMonsterXPTotal newMonsterList }
        , Cmd.none)
    SetNewMonsterName name ->
      ({ model | newMonsterName = name }, Cmd.none)
    SetNewMonsterRating rating ->
      ({ model |
           newMonsterRating = Debug.log (toString rating) rating,
           newMonsterXP = safeRatingToXP rating }
       , Cmd.none)
    SetNewMonsterXP xp ->
      ({ model |
           newMonsterRating = safeXPToRating xp,
           newMonsterXP = xp }
       , Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div
    [ class "monsters" ]
    (List.map indexedMonsterView model.monsterList)

indexedMonsterView : (ID, Monster.Model) -> Html Msg
indexedMonsterView (id, model) =
  div
    []
    [ App.map (ModifyMonster id) (Monster.view model)
    , button
        [ onClick (RemoveMonster id) ]
        [ text "Remove" ]
    ]

summaryView : Model -> Html Msg
summaryView model =
  let
    monsters = "Monsters: " ++ (toString <| List.length model.monsterList)
    threat =
      "XP: " ++
      toString model.monsterXpTotal
  in
    div
      [ class "monster-summary" ]
      [
        text (monsters ++ " " ++ threat)
      ]

addMonsterView : Model -> Html Msg
addMonsterView model =
  div
    []
    [ label
        [ for "monster-rating" ]
        [ text "Challenge Rating" ]
    , monsterRatingOptionsView model
    , label
        [ for "monster-xp" ]
        [ text "Experience Points" ]
    , monsterXpOptionsView model
    , label
        [ for "monster-name" ]
        [ text "Name" ]
    , input
        [ type' "text"
        , value model.newMonsterName
        , onInput SetNewMonsterName
        ]
        []
    , button
        [ onClick (AddMonster (Monster.new model.newMonsterRating model.newMonsterName)) ]
        [ text "Add Monster"]
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
        (\rating -> monsterRatingOption (toString rating) (rating == model.newMonsterRating))
        StatTables.ratingList
  in
    select
      [ name "monster-rating"
      , on "change" (Json.map SetNewMonsterRating targetValueFloatDecoder)
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
        (\xp -> monsterXpOption (toString xp) (xp == model.newMonsterXP))
        StatTables.xpList
  in
    select
      [ name "monster-xp"
      , on "change" (Json.map SetNewMonsterXP targetValueIntDecoder)
      ]
      monsterXpOptions

calculateMonsterMultiplier : List (ID, Monster.Model) -> Float
calculateMonsterMultiplier taggedMonsters =
  let
    monsterCount = List.length taggedMonsters
  in
    if (monsterCount == 0 || monsterCount == 1) then
      1
    else if (monsterCount >= 2 || monsterCount < 3) then
      1.5
    else if (monsterCount >= 3 || monsterCount < 7) then
      2
    else if (monsterCount >= 7 || monsterCount < 11) then
      2.5
    else if (monsterCount >= 11 || monsterCount < 15) then
      3
    else
      4

calculateMonsterXPTotal : List (ID, Monster.Model) -> Float
calculateMonsterXPTotal taggedMonsters =
  let
    monsters = List.map snd taggedMonsters
    totalMonsterXPs = List.sum (List.map .xp monsters)
    multiplier = calculateMonsterMultiplier taggedMonsters
  in
    toFloat totalMonsterXPs * multiplier
