module MonsterList where

import Utilities exposing (..)
import StatTables
import Monster
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

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

type Action
  = AddMonster Monster.Model
  | RemoveMonster ID
  | ModifyMonster ID Monster.Action
  | SetNewMonsterName String
  | SetNewMonsterXP Int
  | SetNewMonsterRating Float

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    AddMonster monster ->
      let
        newMonsterList = (model.uid, monster) :: model.monsterList
      in
        ({ model |
             monsterList = newMonsterList,
             monsterXpTotal = calculateMonsterXPTotal newMonsterList,
             uid = model.uid + 1 }
        , Effects.none)
    RemoveMonster id ->
      let
        newMonsterList = List.filter (\(monsterID, _) -> monsterID /= id) model.monsterList 
      in
        ({ model |
             monsterList = newMonsterList,
             monsterXpTotal = calculateMonsterXPTotal newMonsterList }
        , Effects.none)
    ModifyMonster id monsterAction ->
      let
        updateMonster (monsterID, monsterModel) =
          if id == monsterID then
            (monsterID, fst <| Monster.update monsterAction monsterModel)
          else
            (monsterID, monsterModel)
        newMonsterList = List.map updateMonster model.monsterList 
      in
        ({ model |
             monsterList = newMonsterList,
             monsterXpTotal = calculateMonsterXPTotal newMonsterList }
        , Effects.none)
    SetNewMonsterName name ->
      ({ model | newMonsterName = name }, Effects.none)
    SetNewMonsterRating rating ->
      ({ model |
           newMonsterRating = Debug.log (toString rating) rating,
           newMonsterXP = safeRatingToXP rating }
       , Effects.none)
    SetNewMonsterXP xp ->
      ({ model |
           newMonsterRating = safeXPToRating xp,
           newMonsterXP = xp }
       , Effects.none)
 
-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    (List.map (monsterView address) model.monsterList)

monsterView : Signal.Address Action -> (ID, Monster.Model) -> Html
monsterView address (id, model) =
  let
    context = 
      Monster.Context
        (Signal.forwardTo address (ModifyMonster id))
        (Signal.forwardTo address (always (RemoveMonster id)))
  in
    Monster.view context model

summaryView : Model -> Html
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

addMonsterView : Signal.Address Action -> Model -> Html
addMonsterView address model =
  div
    []
    [ label
        [ for "monster-rating" ]
        [ text "Challenge Rating" ]
    , monsterRatingOptionsView address model
    , label
        [ for "monster-xp" ]
        [ text "Experience Points" ]
    , monsterXpOptionsView address model
    , label
        [ for "monster-name" ]
        [ text "Name" ]
    , input
        [ type' "text" 
        , value model.newMonsterName
        , on "input" targetValue (\name -> Signal.message address (SetNewMonsterName name))
        ]
        []
    , button
        [ onClick address (AddMonster (Monster.new model.newMonsterRating model.newMonsterName)) ]
        [ text "Add Monster"]
    ]

monsterRatingOptionsView : Signal.Address Action -> Model -> Html
monsterRatingOptionsView address model =
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
      , on "change" targetValue (\rating -> Signal.message address (SetNewMonsterRating (safeStrToRating rating))) 
      ]
      monsterRatingOptions

monsterXpOptionsView : Signal.Address Action -> Model -> Html
monsterXpOptionsView address model =
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
      , on "change" targetValue (\xp -> Signal.message address (SetNewMonsterXP (safeStrToLevel xp)))
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
