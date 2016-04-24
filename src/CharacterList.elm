module CharacterList where

import Utilities exposing (..)
import StatTables
import Character
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MODEL

type alias ID = Int

-- TODO: Party will become CharacterList
type alias CharacterList = List (ID, Character.Model)

type alias PartyThresholds =
  { easy : Int
  , medium : Int
  , hard : Int
  , deadly : Int
  }

type alias Model = 
  { uid : ID
  , characterList : CharacterList 
  , partyThresholds : PartyThresholds
  , newCharacterLevel : Int
  , newCharacterName : String
  }

init : Model
init =
  { uid = 6
  , characterList = initCharacterList
  , partyThresholds = initPartyThresholds
  , newCharacterLevel = 1
  , newCharacterName = "" 
  }

-- TODO: load init character list from local storage
initCharacterList : CharacterList
initCharacterList = 
  [ (1, Character.init)
  , (2, Character.init)
  , (3, Character.init)
  , (4, Character.init)
  , (5, Character.init)
  ]

levelsFromCharacterList : CharacterList -> List Int
levelsFromCharacterList characterList =
  List.map (snd >> .level) characterList

initPartyThresholds : PartyThresholds
initPartyThresholds =
  calculatePartyThresholds <| levelsFromCharacterList initCharacterList

calculatePartyThresholds : List Int -> PartyThresholds
calculatePartyThresholds levels =
  let
    easyPartyThresholds =
      List.map getEasyThreshold levels
    mediumPartyThresholds =
      List.map getMediumThreshold levels
    hardPartyThresholds =
      List.map getHardThreshold levels
    deadlyPartyThresholds =
      List.map getDeadlyThreshold levels
  in
    { easy = List.sum easyPartyThresholds
    , medium = List.sum mediumPartyThresholds
    , hard = List.sum hardPartyThresholds
    , deadly = List.sum deadlyPartyThresholds
    }

-- UPDATE

type Action
  = NoOp
  | AddCharacter Character.Model
  | RemoveCharacter ID
  | ModifyCharacter ID Character.Action
  | SetNewCharacterLevel Int
  | SetNewCharacterName String

update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    AddCharacter character ->
      let
        newCharacterList = (model.uid, character) :: model.characterList
      in
        ({ model |
             characterList = newCharacterList,
             partyThresholds = calculatePartyThresholds <| levelsFromCharacterList newCharacterList,
             uid = model.uid + 1 }
        , Effects.none)
    RemoveCharacter id ->
      let
        newCharacterList = List.filter (\(characterID, _) -> characterID /= id) model.characterList 
      in
        ({ model |
             characterList = newCharacterList,
             partyThresholds = calculatePartyThresholds <| levelsFromCharacterList newCharacterList }
        , Effects.none)
    ModifyCharacter id characterAction ->
      let
        updateCharacter (characterID, characterModel) =
          if id == characterID then
            (characterID, fst <| Character.update characterAction characterModel)
          else
            (characterID, characterModel)
        newCharacterList = List.map updateCharacter model.characterList 
      in
        ({ model |
             characterList = newCharacterList,
             partyThresholds = calculatePartyThresholds <| levelsFromCharacterList newCharacterList }
        , Effects.none)
    SetNewCharacterLevel level ->
      ({ model | newCharacterLevel = level }, Effects.none)
    SetNewCharacterName name ->
      ({ model | newCharacterName = name }, Effects.none)
 
-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "characters" ]
    (List.map (characterView address) model.characterList)

characterView : Signal.Address Action -> (ID, Character.Model) -> Html
characterView address (id, model) =
  let
    context = 
      Character.Context
        (Signal.forwardTo address (ModifyCharacter id))
        (Signal.forwardTo address (always (RemoveCharacter id)))
  in
    Character.view context model

addCharacterView : Signal.Address Action -> Model -> Html
addCharacterView address model =
  div 
    []
    [ label
        [ for "level" ]
        [ text "Level" ]
    , levelOptionsView address model
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

levelOptionsView : Signal.Address Action -> Model -> Html
levelOptionsView address model =
  let 
    levelOption level isSelected =
      option
        [ value level 
        , selected isSelected
        ]
        [ text level ]
    levelOptions =
      List.map
        (\level -> levelOption (toString level) (level == model.newCharacterLevel))
        StatTables.levelList
  in
    select 
      [ name "character-level"
      , on "change" targetValue (\level -> Signal.message address (SetNewCharacterLevel (safeStrToLevel level))) 
      ]
      levelOptions

partyThresholdsView : PartyThresholds -> Html
partyThresholdsView partyThresholds =
  p
    []
    [ text (partyThresholds |> toString) ]

summaryView : Model -> Html
summaryView model =
  let
    members = "Members: " ++ (toString <| List.length model.characterList)
    thresholds = 
      "XP: " ++
      toString model.partyThresholds.easy ++ " | " ++
      toString model.partyThresholds.medium ++ " | " ++
      toString model.partyThresholds.hard ++ " | " ++
      toString model.partyThresholds.deadly
  in
    div
      [ class "party-summary" ]
      [
        text (members ++ " " ++ thresholds)
      ]

