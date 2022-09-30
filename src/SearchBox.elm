module SearchBox exposing
    ( input, State, ChangeEvent(..), Msg
    , init, update, reset, setLoading
    )

{-|


# Definitions

@docs input, State, ChangeEvent, Msg


# Usage

To control the state transitions.

@docs init, update, reset, setLoading

-}

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import SearchBox.Icons as Icons


{-| The state of the searchbox
-}
type alias State =
    { hasFocus : Bool
    , selectionIndex : Int
    , loading : Bool
    }


{-| -}
init : State
init =
    { hasFocus = False
    , selectionIndex = 0
    , loading = False
    }


{-| Reset the searchbox. You'll want to call this at various points in your workflow.
-}
reset : State -> State
reset state =
    { state
        | selectionIndex = 0
        , loading = False
    }


{-| Show a loading icon. Useful if you are retrieving a list of options remotely.
-}
setLoading : State -> State
setLoading state =
    { state | loading = True }



-- UPDATE


{-| Message for internal state changes.
-}
type Msg
    = GotFocus
    | LostFocus
    | ChangedIndex Int


{-| This is the event that is the `onChange` wraps that you handle in your app's update function.
-}
type ChangeEvent a
    = SelectionChanged a
    | TextChanged String
    | SearchBoxChanged Msg


{-| Called when you need to update the searchbox's internal state.
-}
update : Msg -> State -> State
update msg state =
    case msg of
        GotFocus ->
            { state | hasFocus = True }

        LostFocus ->
            { state | hasFocus = False }

        ChangedIndex newIndex ->
            { state | selectionIndex = newIndex }


{-| Use this in a similar way to `Element.Input.text`.

**Note**: `Events.onClick`, `Events.onFocus` and `Events.onLoseFocus` are used
internally by the date picker. This means, that **your own `Events.onClick`,
`Events.onFocus` and `Events.onLoseFocus` attributes have no effect and will
not fire**.

-}
input :
    List (Attribute msg)
    ->
        { onChange : ChangeEvent a -> msg
        , text : String
        , selected : Maybe a
        , options : Maybe (List a)
        , label : Input.Label msg
        , placeholder : Maybe (Input.Placeholder msg)
        , toLabel : a -> String
        , filter : String -> a -> Bool
        , state : State
        }
    -> Element msg
input userAttributes config =
    let
        filteredOptions =
            List.filter (config.filter config.text) (Maybe.withDefault [] config.options)

        msgs =
            { changedIndex = config.onChange << SearchBoxChanged << ChangedIndex
            , changedSelection = SelectionChanged >> config.onChange
            }

        optionsBox =
            if config.state.hasFocus && config.selected == Nothing && config.options /= Nothing then
                optionsList
                    msgs
                    config.toLabel
                    config.state.selectionIndex
                    filteredOptions

            else
                none

        icon =
            if config.selected /= Nothing then
                Icons.check

            else if config.state.loading then
                Icons.loader

            else
                Icons.search

        defaultAttributes =
            [ Events.onFocus <| config.onChange <| SearchBoxChanged GotFocus
            , Events.onLoseFocus <| config.onChange <| SearchBoxChanged LostFocus
            , onKeyDown msgs config.state.selectionIndex filteredOptions
            , htmlAttribute (Html.Attributes.autocomplete False)
            , below optionsBox
            , inFront icon
            ]
    in
    Input.text (defaultAttributes ++ userAttributes)
        { onChange = config.onChange << TextChanged
        , text =
            Maybe.map config.toLabel config.selected
                |> Maybe.withDefault config.text
        , placeholder = config.placeholder
        , label = config.label
        }


optionsList :
    { r | changedIndex : Int -> msg, changedSelection : a -> msg }
    -> (a -> String)
    -> Int
    -> List a
    -> Element msg
optionsList msgs toLabel selectionIndex options =
    let
        optionItem index option =
            el
                [ width fill
                , padding 5
                , pointer
                , Background.color
                    (if index == selectionIndex then
                        colors.selected

                     else
                        colors.white
                    )
                , Events.onMouseDown (msgs.changedSelection option)
                , Events.onMouseEnter (msgs.changedIndex index)
                ]
                (text (toLabel option))
    in
    column
        [ Background.color colors.white
        , Border.width 1
        , Border.color (rgba255 0 0 0 0.15)
        , Border.shadow
            { offset = ( 0, 6 )
            , size = 1
            , blur = 12
            , color = rgba255 0 0 0 0.175
            }
        , height <| minimum 250 shrink
        , width <| maximum 500 fill
        , clip
        , scrollbars
        ]
        (if List.isEmpty options then
            [ el [ padding 5 ] <| text "No matches" ]

         else
            List.indexedMap optionItem options
        )


onKeyDown :
    { r | changedIndex : Int -> msg, changedSelection : a -> msg }
    -> Int
    -> List a
    -> Element.Attribute msg
onKeyDown msgs selectionIndex options =
    let
        newIndex operator =
            modBy (List.length options) (operator selectionIndex 1)
                |> msgs.changedIndex
                |> Decode.succeed

        isArrowKey keyName =
            case keyName of
                "ArrowDown" ->
                    newIndex (+)

                "ArrowUp" ->
                    newIndex (-)

                "Enter" ->
                    options
                        |> List.drop selectionIndex
                        |> List.head
                        |> Maybe.map (msgs.changedSelection >> Decode.succeed)
                        |> Maybe.withDefault (Decode.fail "invalid index")

                _ ->
                    Decode.fail "key not handled"
    in
    Decode.field "key" Decode.string
        |> Decode.andThen isArrowKey
        |> Html.Events.on "keydown"
        |> Element.htmlAttribute


colors =
    { white = Element.rgb 1 1 1
    , gray = Element.rgb 0.9 0.9 0.9
    , blue = Element.rgb 0 0 0.8
    , red = Element.rgb 0.8 0 0
    , darkBlue = Element.rgb 0 0 0.9
    , black = Element.rgb 0 0 0
    , selected = Element.rgb255 245 250 253
    }
