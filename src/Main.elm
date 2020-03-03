module Main exposing (..)

import Browser
import Html exposing (Html)
import Element exposing (Element, el, column, width, fill, centerY, spacing, image, text) 
import Element.Input as Input
import Http


---- MODEL ----


type alias Model =
    { searchText: String
    , podcast: Maybe PodcastData
    }

type alias PodcastData = 
    { name: String
    , contact: String
    }

initialModel : Model
initialModel =
    { searchText = ""
    , podcast = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = SearchChanged String
    | RunSearch
    | ItunesData (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
       SearchChanged str ->
        ( { model | searchText = str }, Cmd.none )
       RunSearch ->
        ( { model | searchText = "" }, searchItunes model.searchText )
       ItunesData _ ->
        ( { model | searchText = "OK" }, Cmd.none )


searchItunes: String -> Cmd Msg
searchItunes search =
    Http.get
        { url = "https://itunes.apple.com/search?media=podcast&lang=en_us&term=" ++ search
        , expect = Http.expectWhatever ItunesData
        }


---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [] (
        column [ width fill, centerY, spacing 30 ]
        [ image [ width ( Element.px 30 ), Element.centerX] { src = "/logo.svg", description = "Elm logo"}
        , el [ Element.centerX ] (text "Your Elm App is working!" )
        , seachElement model
        , resultElement model.podcast
        ]
    )


seachElement : Model -> Element Msg
seachElement model =
    Element.row
        [ Element.centerX, Element.spacing 10 ]
        [ Input.text [] { onChange = SearchChanged, text = model.searchText, placeholder = Nothing, label = Input.labelHidden "Podcast name" }
        , Input.button [] { onPress = Just RunSearch, label = text "Search" }
        ]

resultElement : Maybe PodcastData -> Element msg
resultElement podcastData =
    case podcastData of
       Just podcast ->
            Element.column
                [ Element.centerX ]
                [ Element.row [ Element.spacing 20 ] [ text "Podcast:", text podcast.name ]
                , Element.row [ Element.spacing 20 ] [ text "Contact:", text podcast.contact ]
                ]
       Nothing ->
            Element.none


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
