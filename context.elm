-- Encounter.elm
viewCharacter : Signal.Address Action -> (ID, Character.Model) -> Html
viewCharacter address (id, model) =
  let
    context = 
      Character.Context
        (Signal.forwardTo address (ModifyCharacter id))
        (Signal.forwardTo address (always (RemoveCharacter id)))
  in
    Character.view context model

-- Character.elm
type alias Context =
  { actions : Signal.Address Action
  , remove : Signal.Address ()
  }

view : Context -> Model -> Html
view context character =
  div
    [ class "character" ]
    [
      input
        [
          class "character-level"
        , type' "number"
        , value (toString character.level)
        , on "input" targetValue (\level -> Signal.message context.actions (ModifyLevel (safeStrToLevel level)))
        ] []
    , input
        [
          class "character-name"
        , type' "text"
        , value (character.name)
        , on "input" targetValue (\name -> Signal.message context.actions (ModifyName name))
        ] []
    , button
        [ onClick context.remove () ]
        [ text "Remove" ]
    ]

