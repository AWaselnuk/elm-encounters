module Encounter where

import Effects exposing (Effects)
import Html exposing (..)

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
  [ text "Hello, world!"
  ]

(=>) : a -> b -> (a, b)
(=>) = (,)
