module CharacterList exposing (..)

import Utilities exposing (..)
import StatTables
import Character
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json

-- MODEL

type alias ID = Int

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

type Msg
  = NoOp
  | AddCharacter Character.Model
  | RemoveCharacter ID
  | ModifyCharacter ID Character.Msg
  | SetNewCharacterLevel Int
  | SetNewCharacterName String

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    AddCharacter character ->
      let
        newCharacterList = (model.uid, character) :: model.characterList
      in
        ({ model |
             characterList = newCharacterList,
             partyThresholds = calculatePartyThresholds <| levelsFromCharacterList newCharacterList,
             uid = model.uid + 1 }
        , Cmd.none)
    RemoveCharacter id ->
      let
        newCharacterList = List.filter (\(characterID, _) -> characterID /= id) model.characterList
      in
        ({ model |
             characterList = newCharacterList,
             partyThresholds = calculatePartyThresholds <| levelsFromCharacterList newCharacterList }
        , Cmd.none)
    ModifyCharacter id characterMsg ->
      let
        updateCharacter (characterID, characterModel) =
          if id == characterID then
            (characterID, fst <| Character.update characterMsg characterModel)
          else
            (characterID, characterModel)
        newCharacterList = List.map updateCharacter model.characterList
      in
        ({ model |
             characterList = newCharacterList,
             partyThresholds = calculatePartyThresholds <| levelsFromCharacterList newCharacterList }
        , Cmd.none)
    SetNewCharacterLevel level ->
      ({ model | newCharacterLevel = level }, Cmd.none)
    SetNewCharacterName name ->
      ({ model | newCharacterName = name }, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div
    [ class "characters" ]
    (List.map indexedCharacterView model.characterList)

indexedCharacterView : (ID, Character.Model) -> Html Msg
indexedCharacterView (id, model) =
  div
    []
    [ App.map (ModifyCharacter id) (Character.view model)
    , button
        [ onClick (RemoveCharacter id) ]
        [ text "Remove" ]
    ]

addCharacterView : Model -> Html Msg
addCharacterView model =
  div
    []
    [ label
        [ for "level" ]
        [ text "Level" ]
    , levelOptionsView model
    , label
        [ for "name" ]
        [ text "Name" ]
    , input
        [ type' "text"
        , value model.newCharacterName
        , onInput SetNewCharacterName
        ]
        []
    , button
        [ onClick (AddCharacter (Character.new model.newCharacterLevel model.newCharacterName)) ]
        [ text "Add Character"]
    ]

levelOptionsView : Model -> Html Msg
levelOptionsView model =
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
      , on "change" (Json.map SetNewCharacterLevel targetValueIntDecoder)
      ]
      levelOptions

partyThresholdsView : PartyThresholds -> Html Msg
partyThresholdsView partyThresholds =
  p
    []
    [ text (partyThresholds |> toString) ]

summaryView : Model -> Html Msg
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

