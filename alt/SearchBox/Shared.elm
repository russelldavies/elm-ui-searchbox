module SearchBox.Shared exposing (onKeyDown, optionsList)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Html.Events
import Json.Decode as Decode


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
