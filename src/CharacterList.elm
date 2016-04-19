module CharacterList where

import Utilities exposing (..)
import StatTables
import Character
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MODEL

type alias ID = Int

-- TODO: Party will become CharacterList
type alias CharacterList = List (ID, Character.Model)

type alias Model = 
  { uid : ID
  , characterList : CharacterList 
  , newCharacterLevel : Int
  , newCharacterName : String
  }

-- UPDATE

type Action
  = NoOp
  | AddCharacter Character.Model
  | RemoveCharacter ID
  | ModifyCharacter ID Character.Action
  | SetNewCharacterLevel Int
  | SetNewCharacterName String

update: Action -> Model -> (Model, Effects.Action)
update =
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
      ({ model | newCharacterLevel = level }, Effects.none)
    SetNewCharacterName name ->
      ({ model | newCharacterName = name }, Effects.none)
 
-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "characters" ]
    (List.map (characterView address) model.party)

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

