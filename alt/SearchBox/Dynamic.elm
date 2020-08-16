module SearchBox.Dynamic exposing
    ( Config
    , Msg
    , State
    , UpdateResult(..)
    , empty
    , filled
    , querying
    , selected
    , selecting
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
import Process
import SearchBox.Icons as Icons
import SearchBox.Shared as Shared
import Task


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


selecting : State a -> State a
selecting state =
    { state | status = Selecting 0 }


querying : State a -> State a
querying state =
    { state | status = Querying }


selected : State a -> Maybe a
selected state =
    case state.status of
        Selected option ->
            Just option

        _ ->
            Nothing



-- UPDATE


type Msg a
    = TypedQuery String
    | ChangedFocus Bool
    | ChangedIndex Int
    | ChangedSelection a
    | TimePassed String
    | Notify (UpdateResult a)


type UpdateResult a
    = StateChange (State a)
    | QueryEntered String
    | SelectionMade a


update : Msg a -> State a -> ( UpdateResult a, Cmd (Msg a) )
update msg state =
    case msg of
        TypedQuery query ->
            ( StateChange { state | query = query, status = Querying }
            , if String.isEmpty query then
                Cmd.none

              else
                enqueueDebounceFor query
            )

        ChangedFocus hasFocus ->
            ( StateChange { state | hasFocus = hasFocus }
            , Cmd.none
            )

        ChangedIndex newIndex ->
            ( StateChange { state | status = Selecting newIndex }
            , Cmd.none
            )

        ChangedSelection newSelection ->
            ( StateChange { state | status = Selected newSelection }
            , notify (SelectionMade newSelection)
            )

        TimePassed debouncedString ->
            if debouncedString == state.query then
                ( StateChange { state | status = Loading }
                , notify (QueryEntered state.query)
                )

            else
                ( StateChange state, Cmd.none )

        Notify updateResult ->
            ( updateResult, Cmd.none )


notify : UpdateResult a -> Cmd (Msg a)
notify updateResult =
    Task.succeed (Notify updateResult) |> Task.perform identity


enqueueDebounceFor : String -> Cmd (Msg a)
enqueueDebounceFor string =
    Process.sleep 500
        |> Task.perform (always (TimePassed string))



-- VIEW


view : Config a -> List a -> State a -> Element (Msg a)
view config options { query, hasFocus, status } =
    let
        msgs =
            { changedIndex = ChangedIndex, changedSelection = ChangedSelection }
    in
    case status of
        Querying ->
            searchBoxInput config query TypedQuery [ inFront Icons.search ]

        Selecting selectedIndex ->
            let
                filteredOptions =
                    options
                        |> List.filter (config.filter query)
            in
            searchBoxInput config
                query
                TypedQuery
                [ inFront Icons.search
                , below
                    (if hasFocus then
                        Shared.optionsList
                            msgs
                            config.toLabel
                            selectedIndex
                            filteredOptions

                     else
                        none
                    )
                , Shared.onKeyDown msgs selectedIndex filteredOptions
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
