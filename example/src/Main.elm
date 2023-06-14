module Main exposing
    ( Model
    , Msg(..)
    , init
    , main
    , update
    , view
    )

import Browser
import Fuzzy exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { searchQuery : String
    }


init : Model
init =
    let
        model =
            { searchQuery = ""
            }
    in
    model



-- UPDATE


type Msg
    = UpdatedSearchQuery String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdatedSearchQuery value ->
            { model | searchQuery = value }



-- VIEW


view : Model -> Html Msg
view model =
    let
        getScore : String -> String -> Int
        getScore searchInput candidate =
            Fuzzy.match
                searchInput
                candidate
                |> .score

        sorted =
            [ "apple", "banana", "orange", "pear", "pineapple", "strawberry" ]
                |> List.sortBy (getScore model.searchQuery)
                |> List.reverse

        viewResults =
            div []
                [ ul []
                    (List.map
                        (\candidate ->
                            li [] [ text candidate ]
                        )
                        sorted
                    )
                ]
    in
    div []
        [ input
            [ type_ "text"
            , placeholder "search"
            , value model.searchQuery
            , onInput UpdatedSearchQuery
            ]
            []
        , viewResults
        ]
