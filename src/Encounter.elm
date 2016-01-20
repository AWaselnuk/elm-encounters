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
  , easyThreshold : Int
  , mediumThreshold : Int
  , hardThreshold : Int
  , deadlyThreshold : Int
  }

type alias Model =
  { party : List Character }

initCharacter : Character
initCharacter =
  { level = 1
  , name = Nothing
  , easyThreshold = 1
  , mediumThreshold = 2
  , hardThreshold = 3
  , deadlyThreshold = 4
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
    (List.map (characterView address) model.party)

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
    , p []
      [ text (toString (get character.level easyThresholds)) ]
    , p []
      [ text (toString (get character.level mediumThresholds)) ]
    , p []
      [ text (toString (get character.level hardThresholds)) ]
    , p []
      [ text (toString (get character.level deadlyThresholds)) ]
    ]
  
randomName : String
randomName =
  "Random Name"

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

hardThresholds : Dict Int Int
hardThresholds =
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

deadlyThresholds : Dict Int Int
deadlyThresholds =
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

(=>) : a -> b -> (a, b)
(=>) = (,)
