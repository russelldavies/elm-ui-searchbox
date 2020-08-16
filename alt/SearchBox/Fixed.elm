module SearchBox.Fixed exposing
    ( Config
    , Msg
    , State
    , empty
    , filled
    , selected
    , update
    , view
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Events
import Json.Decode as Decode
import SearchBox.Icons as Icons
import SearchBox.Shared as Shared


type alias State a =
    { query : String
    , hasFocus : Bool
    , selectionIndex : Int
    , selection : Maybe a
    }


type alias Config a =
    { toLabel : a -> String
    , filter : String -> a -> Bool
    , placeholder : Maybe (Input.Placeholder (Msg a))
    , label : Input.Label (Msg a)
    }


empty : State a
empty =
    { query = ""
    , hasFocus = False
    , selectionIndex = 0
    , selection = Nothing
    }


filled : Maybe a -> State a
filled option =
    { query = ""
    , hasFocus = False
    , selectionIndex = 0
    , selection = option
    }


selected : State a -> Maybe a
selected state =
    state.selection



-- UPDATE


type Msg a
    = TypedQuery String
    | ChangedFocus Bool
    | ChangedIndex Int
    | ChangedSelection a


update : Msg a -> State a -> State a
update msg state =
    case msg of
        TypedQuery query ->
            { state
                | query = query
                , selectionIndex = 0
                , selection = Nothing
            }

        ChangedFocus hasFocus ->
            { state | hasFocus = hasFocus }

        ChangedIndex newIndex ->
            { state | selectionIndex = newIndex }

        ChangedSelection newSelection ->
            { state | selection = Just newSelection }


view : Config a -> List a -> State a -> Element (Msg a)
view config options state =
    let
        filteredOptions =
            options
                |> List.filter (config.filter state.query)

        msgs =
            { changedIndex = ChangedIndex, changedSelection = ChangedSelection }
    in
    Input.text
        [ Events.onFocus (ChangedFocus True)
        , Events.onLoseFocus (ChangedFocus False)
        , below
            (if state.hasFocus && state.selection == Nothing then
                Shared.optionsList
                    { changedIndex = ChangedIndex, changedSelection = ChangedSelection }
                    config.toLabel
                    state.selectionIndex
                    filteredOptions

             else
                none
            )
        , Shared.onKeyDown msgs state.selectionIndex filteredOptions
        , inFront
            (if state.selection /= Nothing then
                Icons.check

             else
                Icons.search
            )
        ]
        { onChange =
            if state.selection == Nothing then
                TypedQuery

            else
                always (TypedQuery state.query)
        , text = Maybe.map config.toLabel state.selection |> Maybe.withDefault state.query
        , placeholder = config.placeholder
        , label = config.label
        }
