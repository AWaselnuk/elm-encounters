module Encounter exposing (..)

import Utilities exposing (..)
import CharacterList exposing (CharacterList)
import MonsterList exposing (MonsterList)
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import String
import Debug

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

-- TODO
-- Remember char list with localStorage
-- Design UI
-- Enhance flows by shifting focus


-- MODEL

type alias ID = Int

type alias Model =
  { characters : CharacterList.Model
  , monsters : MonsterList.Model
  }

init : (Model, Effects Msg)
init =
  ( { characters = CharacterList.init
    , monsters = MonsterList.init }
  , Effects.none)

-- UPDATE

type Msg
  = NoOp
  | CharacterListMsg CharacterList.Msg
  | MonsterListMsg MonsterList.Msg

update : Msg -> Model -> (Model, Effects Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Effects.none)
    CharacterListMsg msg ->
      let
        (clModel, clEffects) = CharacterList.update msg model.characters
      in
        ({ model | characters = clModel }, Effects.map CharacterListMsg clEffects)
    MonsterListMsg msg ->
      let
        (mlModel, mlEffects) = MonsterList.update msg model.monsters
      in
        ({ model | monsters = mlModel }, Effects.map MonsterListMsg mlEffects)

-- VIEW

view : Model -> Html Msg
view model =
  div
    [ class "main" ]
    [
      titleSectionView
    , partySectionView model.characters
    , monsterSectionView model.monsters
    , encounterSummaryView model
    , debugView model
    ]

encounterSummaryView : Model -> Html Msg
encounterSummaryView model =
  section
    [ class "encounter-summary-section" ]
    [
      difficultyBadgeView model
    , CharacterList.summaryView model.characters
    , MonsterList.summaryView model.monsters
    ]

difficultyBadgeView : Model -> Html
difficultyBadgeView model =
  let
    badgeClass = "badge badge--" ++ calculateDifficulty model
  in
    div
      [ class badgeClass ]
      [ strong [ class "difficulty" ] [text <| calculateDifficulty model] ]

titleSectionView : Html
titleSectionView =
  section
    [ class "title-section" ]
    [
      h1 [ class "title" ] [ text "D&D 5th Edition Encounter Builder" ]
    , p
        [ class "description" ]
        [ text "This encounter builder allows you to easily determine the difficulty
          of your 5th edition encounters. Simply create a list of party members
          and a list of monsters, and the encounter builder will tell you if
          it will be a cake walk or a total party kill." ]
    ]

partySectionView : CharacterList.Model -> Html Msg
partySectionView model =
  section
    [ class "party-section" ]
    [
      h2 [] [text "The party"]
    , CharacterList.summaryView model
    , h3 [] [text "Add new character"]
    , map CharacterListMsg (CharacterList.addCharacterView model)
    , h3 [] [text "Current party"]
    , div
        [ class "current-party-tools" ]
        [
          button [ class "toggle-party-view" ] [ text "view current party" ]
        ]
    , map CharacterListMsg (CharacterList.view model)
    ]

monsterSectionView : MonsterList.Model -> Html Msg
monsterSectionView model =
  section
    [ class "monster-section" ]
    [
      h2 [] [text "The monsters"]
    , MonsterList.summaryView model
    , h3 [] [text "Add new monster"]
    , map MonsterListMsg (MonsterList.addMonsterView model)
    , h3 [] [text "Current monsters"]
    , div
        [ class "current-monster-tools" ]
        [
          button [ class "set-monster-cr-view" ] [ text "CR" ]
        , button [ class "set-monster-xp-view" ] [ text "XP" ]
        , button [ class "toggle-monster-view" ] [ text "view current monsters" ]
        ]
    , map MonsterListMsg (MonsterList.view model)
    ]

calculateDifficulty : Model -> String
calculateDifficulty model =
  if (round model.monsters.monsterXpTotal < model.characters.partyThresholds.easy) then
    "easy"
  else if (round model.monsters.monsterXpTotal < model.characters.partyThresholds.medium) then
    "medium"
  else if (round model.monsters.monsterXpTotal < model.characters.partyThresholds.hard) then
    "hard"
  else if (round model.monsters.monsterXpTotal < model.characters.partyThresholds.deadly) then
    "deadly"
  else
    "TPK"

debugView model =
  p [] [ text <| toString <| model ]

