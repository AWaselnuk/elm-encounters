module Encounter where

import Utilities exposing (..)
import Character
import Monster
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import String

-- FEATURES

-- List of characters:
-- -- Have levels
-- -- Have XP thresholds for each difficulty (according table)
-- -- Defaults to 5 characters
-- -- Remembers your character list with localStorage
-- List of monsters:
-- -- Have XP values (input from user)
-- -- Have Challenge Rating (input from user)
-- -- Visual indication of difficulty?
-- -- Number of monsters adds a multiplier
-- Show Encounter difficulty
-- -- Show Party XP threshold for easy, medium, hard, deadly
-- -- Have a strong visual indication of encounter difficulty. Use cool pictures. Be creative!
--
-- Steps for encounter calculation
-- 1. Determine XP thresholds for each character
-- 2. Determine party XP thresholds by adding all character thresholds
-- 3. Total monster XP
-- 4. Modify monster total for multiple monsters
-- 5. Compare XP. Closest party threshold that is lower than monster XP determines difficulty

-- MODEL

type alias ID = Int

type alias PartyThresholds =
  { easy : Int
  , medium : Int
  , hard : Int
  , deadly : Int
  }

type alias Model =
  { uid : ID
  , party : List (ID, Character.Model)
  , partyThresholds : PartyThresholds
  , newCharacterLevel : Int
  , newCharacterName : String
  , monsters : List (ID, Monster.Model)
  , newMonsterName : String
  , newMonsterRating : Float
  }

initParty : List (ID, Character.Model)
initParty =
  [ (1, Character.init)
  , (2, Character.init)
  , (3, Character.init)
  , (4, Character.init)
  , (5, Character.init)
  ]

levelsFromParty : List (ID, Character.Model) -> List Int
levelsFromParty party =
  List.map (snd >> .level) party

initPartyThresholds : PartyThresholds
initPartyThresholds =
  calculatePartyThresholds <| levelsFromParty initParty

init : (Model, Effects Action)
init =
  ( { uid = 6
    , party = initParty
    , partyThresholds = initPartyThresholds
    , newCharacterLevel = 1
    , newCharacterName = "" 
    , monsters = []
    , newMonsterName = "" 
    , newMonsterRating = initRating }
  , Effects.none)

-- UPDATE

type Action
  = NoOp
  | AddCharacter Character.Model
  | RemoveCharacter ID
  | ModifyCharacter ID Character.Action
  | SetNewCharacterLevel Int
  | SetNewCharacterName String
  | AddMonster Monster.Model
  | SetNewMonsterName String

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    AddCharacter character ->
      let
        newParty = (model.uid, character) :: model.party
      in
        ({ model |
             party = newParty,
             partyThresholds = calculatePartyThresholds <| levelsFromParty newParty,
             uid = model.uid + 1 }
        , Effects.none)
    RemoveCharacter id ->
      let
        newParty = List.filter (\(characterID, _) -> characterID /= id) model.party 
      in
        ({ model |
             party = newParty,
             partyThresholds = calculatePartyThresholds <| levelsFromParty newParty }
        , Effects.none)
    ModifyCharacter id characterAction ->
      let
        updateCharacter (characterID, characterModel) =
          if id == characterID then
            (characterID, fst <| Character.update characterAction characterModel)
          else
            (characterID, characterModel)
        newParty = List.map updateCharacter model.party 
      in
        ({ model |
             party = newParty,
             partyThresholds = calculatePartyThresholds <| levelsFromParty newParty }
        , Effects.none)
    SetNewCharacterLevel level ->
      ({ model | newCharacterLevel = restrictLevel level }, Effects.none)
    SetNewCharacterName name ->
      ({ model | newCharacterName = name }, Effects.none)
    AddMonster monster ->
      let
        newMonsters = (model.uid, monster) :: model.monsters
      in
        ({ model |
             monsters = newMonsters,
             uid = model.uid + 1 }
        , Effects.none)
    SetNewMonsterName name ->
      ({ model | newMonsterName = name }, Effects.none)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [
      partyThresholdsView model.partyThresholds
    , addCharacterView address model
    , div
        []
        (List.map (characterView address) model.party)
    , addMonsterView address model
    ]

addMonsterView : Signal.Address Action -> Model -> Html
addMonsterView address model =
  div
    []
    [ label
        [ for "monster-rating" ]
        [ text "Challenge Rating" ]
    , monsterRatingOptionsView
    , label
        [ for "monster-xp" ]
        [ text "Experience Points" ]
    , monsterXpOptionsView
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

monsterRatingOptionsView : Html
monsterRatingOptionsView =
  let 
    monsterRatingOption rating =
      option
        [ value rating ]
        [ text rating ]
    monsterRatingOptions =
      List.map (\rating -> monsterRatingOption <| toString <| rating) ratingList
  in
    select 
      [ name "monster-rating" ]
      monsterRatingOptions

monsterXpOptionsView : Html
monsterXpOptionsView =
  let 
    monsterXpOption xp =
      option
        [ value xp ]
        [ text xp ]
    monsterXpOptions =
      List.map (\xp -> monsterXpOption <| toString <| xp) xpList
  in
    select 
      [ name "monster-xp" ]
      monsterXpOptions

partyThresholdsView : PartyThresholds -> Html
partyThresholdsView partyThresholds =
  p
    []
    [ text (partyThresholds |> toString) ]

addCharacterView : Signal.Address Action -> Model -> Html
addCharacterView address model =
  div 
    []
    [ label
        [ for "level" ]
        [ text "Level" ]
    , input
        [ type' "number" 
        , value (toString model.newCharacterLevel) 
        , on "input" targetValue (\level -> Signal.message address (SetNewCharacterLevel (safeStrToLevel level)))
        ]
        []
    , label 
        [ for "name" ]
        [ text "Name" ]
    , input
        [ type' "text"
        , value model.newCharacterName
        , on "input" targetValue (\name -> Signal.message address (SetNewCharacterName name))
        ]
        []
    , button
        [ onClick address (AddCharacter (Character.new model.newCharacterLevel model.newCharacterName)) ]
        [ text "Add Character"]
    ] 

characterView : Signal.Address Action -> (ID, Character.Model) -> Html
characterView address (id, model) =
  let
    context = 
      Character.Context
        (Signal.forwardTo address (ModifyCharacter id))
        (Signal.forwardTo address (always (RemoveCharacter id)))
  in
    Character.view context model

getThreshold : Dict Int Int -> Int -> Int
getThreshold thresholds level =
  Maybe.withDefault 0 <| Dict.get level thresholds

calculatePartyThresholds : List Int -> PartyThresholds
calculatePartyThresholds levels =
  let
    easyPartyThresholds =
      List.map (getThreshold easyThresholds) levels
    mediumPartyThresholds =
      List.map (getThreshold mediumThresholds) levels
    hardPartyThresholds =
      List.map (getThreshold hardThresholds) levels
    deadlyPartyThresholds =
      List.map (getThreshold deadlyThresholds) levels
  in
    { easy = List.sum easyPartyThresholds
    , medium = List.sum mediumPartyThresholds
    , hard = List.sum hardPartyThresholds
    , deadly = List.sum deadlyPartyThresholds
    }

easyThresholds : Dict Int Int
easyThresholds =
  Dict.fromList
    [
      (1, 25)
    , (2, 50)
    , (3, 75)
    , (4, 125)
    , (5, 250)
    , (6, 300)
    , (7, 350)
    , (8, 450)
    , (9, 550)
    , (10, 600)
    , (11, 800)
    , (12, 1000)
    , (13, 1100)
    , (14, 1250)
    , (15, 1400)
    , (16, 1600)
    , (17, 2000)
    , (18, 2100)
    , (19, 2400)
    , (20, 2800)
    ]

mediumThresholds : Dict Int Int
mediumThresholds =
  Dict.fromList
    [
      (1, 50)
    , (2, 100)
    , (3, 150)
    , (4, 250)
    , (5, 500)
    , (6, 600)
    , (7, 750)
    , (8, 900)
    , (9, 1100)
    , (10, 1200)
    , (11, 1600)
    , (12, 2000)
    , (13, 2200)
    , (14, 2500)
    , (15, 2800)
    , (16, 3200)
    , (17, 3900)
    , (18, 4200)
    , (19, 4900)
    , (20, 5700)
    ]

hardThresholds : Dict Int Int
hardThresholds =
  Dict.fromList
    [
      (1, 75)
    , (2, 150)
    , (3, 225)
    , (4, 375)
    , (5, 750)
    , (6, 900)
    , (7, 1100)
    , (8, 1400)
    , (9, 1600)
    , (10, 1900)
    , (11, 2400)
    , (12, 3000)
    , (13, 3400)
    , (14, 3800)
    , (15, 4300)
    , (16, 4800)
    , (17, 5900)
    , (18, 6300)
    , (19, 7300)
    , (20, 8500)
    ]

deadlyThresholds : Dict Int Int
deadlyThresholds =
  Dict.fromList
    [
      (1, 100)
    , (2, 200)
    , (3, 400)
    , (4, 500)
    , (5, 1100)
    , (6, 1400)
    , (7, 1700)
    , (8, 2100)
    , (9, 2400)
    , (10, 2800)
    , (11, 3600)
    , (12, 4500)
    , (13, 5100)
    , (14, 5700)
    , (15, 6400)
    , (16, 7200)
    , (17, 8800)
    , (18, 9500)
    , (19, 10900)
    , (20, 12700)
    ]

