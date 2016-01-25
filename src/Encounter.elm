module Encounter where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (..)

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

type alias Character =
  { level : Int
  , name : Maybe String
  }

type alias PartyThresholds =
  { easy : Int
  , medium : Int
  , hard : Int
  , deadly : Int
  }

type alias Model =
  { party : List Character }

initCharacter : Character
initCharacter =
  { level = 1
  , name = Nothing
  }

initCharacters : List Character
initCharacters =
  List.repeat 5 initCharacter

init : (Model, Effects Action)
init =
  ( { party = initCharacters }
  , Effects.none)

-- UPDATE

type Action
  = NoOp

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [
      p
        []
        [ text (toString (partyThresholds model.party)) ]
    , div 
        []
        (List.map (characterView address) model.party)
    ]

characterView : Signal.Address Action -> Character -> Html
characterView address character = 
  div
    [ class "character" ]
    [
      input 
        [
          class "character-level"
        , type' "number"
        , value (toString character.level)
        ] []
    , input
        [
          class "character-name"
        , type' "input"
        , value (Maybe.withDefault randomName character.name)
        ] []
    ]
  
randomName : String
randomName =
  "Random Name"

getThreshold : Dict Int Int -> Character -> Int
getThreshold thresholds character =
  Maybe.withDefault 0 <| get character.level thresholds
  
-- TODO: Implement this
partyThresholds : List Character -> PartyThresholds
partyThresholds party =
  let
    easyPartyThresholds =
      List.map (getThreshold easyThresholds) party
    mediumPartyThresholds =
      List.map (getThreshold mediumThresholds) party
    hardPartyThresholds =
      List.map (getThreshold hardThresholds) party
    deadlyPartyThresholds =
      List.map (getThreshold deadlyThresholds) party
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

(=>) : a -> b -> (a, b)
(=>) = (,)
