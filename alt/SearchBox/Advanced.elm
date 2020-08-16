module SearchBox.Advanced exposing
    ( Config
    , Msg
    , State
    , UpdateResult(..)
    , empty
    , filled
    , setLoading
    , setQuerying
    , setSelecting
    , setSelection
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


type alias State a =
    { query : String
    , hasFocus : Bool
    , status : Status a
    }


type Status a
    = Querying
    | Selecting Int
    | Loading
    | Selected a


type alias Config a =
    { toLabel : a -> String
    , filter : String -> a -> Bool
    , numOptions : Int
    , placeholder : Maybe (Input.Placeholder (Msg a))
    , label : Input.Label (Msg a)
    }


empty : State a
empty =
    { query = ""
    , hasFocus = False
    , status = Querying
    }


filled : a -> State a
filled option =
    { query = ""
    , hasFocus = False
    , status = Selected option
    }


setLoading : State a -> State a
setLoading state =
    { state | status = Loading }


setQuerying : State a -> State a
setQuerying state =
    { state | status = Querying }


setSelecting : State a -> State a
setSelecting state =
    { state | status = Selecting 0 }


setSelection : a -> State a -> State a
setSelection option state =
    { state | status = Selected option }



-- UPDATE


type Msg a
    = TypedQuery String
    | ChangedFocus Bool
    | StatusUpdated (Status a)


type UpdateResult a
    = QueryEntered String
    | SelectionMade a
    | StateChange


update : Msg a -> State a -> ( State a, UpdateResult a )
update msg state =
    case msg of
        TypedQuery query ->
            ( { state
                | query = query
                , status = Querying
              }
            , QueryEntered query
            )

        ChangedFocus hasFocus ->
            ( { state | hasFocus = hasFocus }
            , StateChange
            )

        StatusUpdated newStatus ->
            ( { state | status = newStatus }
            , case newStatus of
                Selected selection ->
                    SelectionMade selection

                _ ->
                    StateChange
            )


view : Config a -> List a -> State a -> Element (Msg a)
view config options { query, hasFocus, status } =
    case status of
        Querying ->
            searchBoxInput config query TypedQuery [ inFront Icons.search ]

        Selecting selectedIndex ->
            let
                filteredOptions =
                    options
                        |> List.filter (config.filter query)
                        |> List.take config.numOptions
            in
            searchBoxInput config
                query
                TypedQuery
                [ inFront Icons.search
                , below
                    (if hasFocus then
                        optionsList config.toLabel selectedIndex filteredOptions

                     else
                        none
                    )
                , onKeyDown selectedIndex filteredOptions
                ]

        Loading ->
            searchBoxInput config query TypedQuery [ inFront Icons.loader ]

        Selected selectedOption ->
            searchBoxInput config
                (config.toLabel selectedOption)
                (always <| TypedQuery query)
                [ inFront Icons.check ]


searchBoxInput :
    Config a
    -> String
    -> (String -> Msg a)
    -> List (Element.Attribute (Msg a))
    -> Element (Msg a)
searchBoxInput config query onChange attrs =
    let
        generalAttrs =
            [ Events.onFocus (ChangedFocus True)
            , Events.onLoseFocus (ChangedFocus False)
            ]
    in
    Input.text (generalAttrs ++ attrs)
        { onChange = onChange
        , text = query
        , placeholder = config.placeholder
        , label = config.label
        }


optionsList : (a -> String) -> Int -> List a -> Element (Msg a)
optionsList toLabel selectedIndex options =
    let
        optionItem index option =
            el
                [ width fill
                , clip
                , padding 5
                , pointer
                , Border.color colors.black
                , Border.width 1
                , Background.color
                    (if index == selectedIndex then
                        colors.gray

                     else
                        colors.white
                    )
                , Events.onMouseDown (StatusUpdated <| Selected option)
                , Events.onMouseEnter (StatusUpdated <| Selecting index)
                ]
                (text (toLabel option))
    in
    if List.isEmpty options then
        el
            [ padding 5
            , Border.color colors.black
            , Border.width 1
            , Background.color colors.gray
            ]
            (text "No matches")

    else
        column [] (List.indexedMap optionItem options)


onKeyDown : Int -> List a -> Element.Attribute (Msg a)
onKeyDown selectedIndex options =
    let
        newIndex operator =
            Selecting (modBy (List.length options) (operator selectedIndex 1))

        isArrowKey keyName =
            case keyName of
                "ArrowDown" ->
                    newIndex (+)
                        |> StatusUpdated
                        |> Decode.succeed

                "ArrowUp" ->
                    newIndex (-)
                        |> StatusUpdated
                        |> Decode.succeed

                "Enter" ->
                    options
                        |> List.drop selectedIndex
                        |> List.head
                        |> Maybe.map (Selected >> StatusUpdated >> Decode.succeed)
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
