module Main exposing (..)

import Browser
import Element exposing (Element, centerY, column, el, fill, image, spacing, text, width)
import Element.Input as Input
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Http
import Json.Decode as D
import Xml.Decode as Xml


---- MODEL ----


type alias Model =
    { searchText : String
    , podcasts : List Podcast
    }


type alias Podcast =
    { feed : String
    , name : String
    , contact : String
    , owner : String
    }


initialModel : Model
initialModel =
    { searchText = ""
    , podcasts = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = SearchChanged String
    | RunSearch
    | ItunesData (Result Http.Error (List String))
    | FeedFetched String (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchChanged str ->
            ( { model | searchText = str }, Cmd.none )

        RunSearch ->
            ( initialModel, searchItunes model.searchText )

        ItunesData (Ok feeds) ->
            ( model, Cmd.batch (List.map fetchFeed feeds) )

        FeedFetched feed (Ok xml) ->
            let
                podcasts =
                    case Xml.run (podcastDecoder feed) xml of
                        Ok p -> List.append model.podcasts [p]
                        Err err ->
                            let
                                _ = Debug.log "err" err
                            in
                            
                            model.podcasts
            in
            ( { model | podcasts = podcasts }, Cmd.none )

        FeedFetched _ (Err _) ->
            ( model, Cmd.none )

        ItunesData (Err _) ->
            ( { model | podcasts = [] }, Cmd.none )

searchItunes : String -> Cmd Msg
searchItunes search =
    Http.get
        { url = "https://itunes.apple.com/search?media=podcast&lang=en_us&term=" ++ search
        , expect = Http.expectJson ItunesData itunesDecoder
        }


itunesDecoder: D.Decoder (List String)
itunesDecoder = 
    let
        decodeResultItem = D.field "feedUrl" D.string
    in
    D.field "results" (D.list decodeResultItem)

fetchFeed : String -> Cmd Msg
fetchFeed feed =
    Http.get
        { url = feed
        , expect = Http.expectString (FeedFetched feed)
        }

podcastDecoder : String -> Xml.Decoder Podcast
podcastDecoder feed =
    Xml.map3 (Podcast feed)
        (Xml.path ["channel", "title"] (Xml.single Xml.string))
        (Xml.path ["channel", "itunes:owner", "itunes:email"] (Xml.single Xml.string))
        (Xml.path ["channel", "itunes:owner", "itunes:name"] (Xml.single Xml.string))

---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout []
        (column [ width fill, centerY, spacing 30 ]
            [ seachElement model
            , column [ Element.centerX ] (List.map resultElement model.podcasts)
            ]
        )


seachElement : Model -> Element Msg
seachElement model =
    Element.row
        [ Element.centerX, Element.spacing 10 ]
        [ Input.text [] { onChange = SearchChanged, text = model.searchText, placeholder = Nothing, label = Input.labelHidden "Podcast name" }
        , Input.button [ Background.color (Element.rgb255 0 0 0), Font.color (Element.rgb255 255 255 255)] { onPress = Just RunSearch, label = text "Go!" }
        ]


resultElement : Podcast -> Element msg
resultElement podcast =
    Element.column
        [ Element.centerX ]
        [ Element.row [ Element.spacing 20 ] [ text "Podcast:", text podcast.name ]
        , Element.row [ Element.spacing 20 ] [ text "Contact:", text podcast.contact ]
        , Element.row [ Element.spacing 20 ] [ text "Feed:", text podcast.feed ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
