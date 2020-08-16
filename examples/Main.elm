module Main exposing (main)

import Browser
import Countries exposing (Country)
import Element exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder)
import Process
import SearchBox
import Task


type alias Model =
    { country : Maybe Country
    , countryText : String
    , countrySearchBox : SearchBox.State

    -- Other SearchBox
    , persons : Maybe (List Person)
    , person : Maybe Person
    , personText : String
    , personSearchBox : SearchBox.State
    }


init : ( Model, Cmd Msg )
init =
    ( { country = Nothing
      , countryText = ""
      , countrySearchBox = SearchBox.init

      -- Other SearchBox
      , persons = Nothing
      , person = Nothing
      , personText = ""
      , personSearchBox = SearchBox.init
      }
    , Cmd.none
    )


type Msg
    = ChangedCountrySearchBox (SearchBox.ChangeEvent Country)
    | ChangedPersonSearchBox (SearchBox.ChangeEvent Person)
    | GotPersons (Result Http.Error (List Person))
    | TimePassed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedCountrySearchBox changeEvent ->
            case changeEvent of
                SearchBox.SelectionChanged country ->
                    ( { model | country = Just country }
                    , Cmd.none
                    )

                SearchBox.TextChanged text ->
                    ( { model
                        | country = Nothing
                        , countryText = text
                        , countrySearchBox = SearchBox.reset model.countrySearchBox
                      }
                    , Cmd.none
                    )

                SearchBox.SearchBoxChanged subMsg ->
                    ( { model | countrySearchBox = SearchBox.update subMsg model.countrySearchBox }
                    , Cmd.none
                    )

        ChangedPersonSearchBox changeEvent ->
            case changeEvent of
                SearchBox.SelectionChanged person ->
                    ( { model | person = Just person }
                    , Cmd.none
                    )

                SearchBox.TextChanged text ->
                    ( { model
                        | persons = Nothing
                        , person = Nothing
                        , personText = text
                        , personSearchBox = SearchBox.reset model.personSearchBox
                      }
                    , enqueueDebounceFor text
                    )

                SearchBox.SearchBoxChanged subMsg ->
                    ( { model | personSearchBox = SearchBox.update subMsg model.personSearchBox }
                    , Cmd.none
                    )

        GotPersons (Ok persons) ->
            ( { model
                | persons = Just persons
                , personSearchBox = SearchBox.reset model.personSearchBox
              }
            , Cmd.none
            )

        GotPersons (Err err) ->
            ( { model | personSearchBox = SearchBox.reset model.personSearchBox }, Cmd.none )

        TimePassed debouncedString ->
            if debouncedString == model.personText then
                ( { model | personSearchBox = SearchBox.setLoading model.personSearchBox }
                , searchPersons model.personText
                )

            else
                ( model, Cmd.none )


searchPersons : String -> Cmd Msg
searchPersons query =
    Http.get
        { url = "https://swapi.dev/api/people/?search=" ++ query
        , expect = Http.expectJson GotPersons (Decode.field "results" (Decode.list personDecoder))
        }


enqueueDebounceFor : String -> Cmd Msg
enqueueDebounceFor string =
    if String.isEmpty string then
        Cmd.none

    else
        Process.sleep 500
            |> Task.perform (always (TimePassed string))


view : Model -> Html Msg
view model =
    Element.layout [] <|
        column []
            [ SearchBox.input []
                { onChange = ChangedCountrySearchBox
                , text = model.countryText
                , selected = model.country
                , options = Just Countries.all
                , label = Input.labelAbove [] (text "Country")
                , placeholder = Nothing
                , toLabel = \country -> country.code ++ " (" ++ country.name ++ ") " ++ country.flag
                , filter =
                    \query country ->
                        [ country.code, country.name ]
                            |> List.map String.toLower
                            |> List.any (String.contains (String.toLower query))
                , state = model.countrySearchBox
                }
            , SearchBox.input []
                { onChange = ChangedPersonSearchBox
                , text = model.personText
                , selected = model.person
                , options = model.persons
                , label = Input.labelAbove [] (text "Person")
                , placeholder = Nothing
                , toLabel = \person -> person.name ++ " " ++ genderSymbol person.gender
                , filter = \query option -> True
                , state = model.personSearchBox
                }
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- Other models


type alias Person =
    { name : String
    , gender : Gender
    }


type Gender
    = Male
    | Female
    | Unknown


personDecoder : Decoder Person
personDecoder =
    Decode.map2 Person
        (Decode.field "name" Decode.string)
        (Decode.field "gender" genderDecoder)


genderDecoder : Decoder Gender
genderDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "male" ->
                        Decode.succeed Male

                    "female" ->
                        Decode.succeed Female

                    _ ->
                        Decode.succeed Unknown
            )


genderSymbol : Gender -> String
genderSymbol gender =
    case gender of
        Male ->
            "♂"

        Female ->
            "♀"

        Unknown ->
            "×"
