module Main exposing (main)

import Browser
import Element exposing (Element, centerY, column, el, fill, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as D
import Xml.Decode as Xml



---- MODEL ----


type alias Podcast =
    { feed : String
    , name : String
    , contact : String
    , owner : String
    }


type alias Model =
    { searchText : String
    , podcasts : List Podcast
    , searching : Bool
    , results : Int
    , parsed : Int
    , errors : Int
    , loadingItunes : Bool
    , itunesError : Bool
    }


initialModel : Model
initialModel =
    { searchText = ""
    , podcasts = []
    , searching = False
    , results = 0
    , parsed = 0
    , errors = 0
    , loadingItunes = False
    , itunesError = False
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = SearchChanged String
    | RunSearch
    | ItunesData (Result Http.Error String)
    | FeedFetched String (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchChanged str ->
            ( { model | searchText = str }, Cmd.none )

        RunSearch ->
            ( { initialModel | searching = True, loadingItunes = True }, searchItunes model.searchText )

        ItunesData (Ok itunesResponseString) ->
            let
                parseResult =
                    itunesResponseString
                        |> clearItunesResponse
                        |> D.decodeString itunesDecoder
            in
            case parseResult of
                Ok ( cnt, feeds ) ->
                    ( { model | results = cnt, loadingItunes = False }, Cmd.batch (List.map fetchFeed feeds) )

                Err _ ->
                    ( { model | itunesError = True, loadingItunes = False }, Cmd.none )

        FeedFetched feed (Ok xml) ->
            let
                totalParsed =
                    model.parsed + 1

                parsedXml =
                    Xml.run (podcastDecoder feed) xml

                podcasts =
                    case parsedXml of
                        Ok p ->
                            List.append model.podcasts [ p ]

                        Err _ ->
                            model.podcasts

                errors =
                    case parsedXml of
                        Ok _ ->
                            model.errors

                        Err _ ->
                            model.errors + 1
            in
            ( { model | podcasts = podcasts, parsed = totalParsed, errors = errors }, Cmd.none )

        FeedFetched _ (Err _) ->
            ( { model | errors = model.errors + 1 }, Cmd.none )

        ItunesData (Err _) ->
            ( { model | itunesError = True, loadingItunes = False }, Cmd.none )


clearItunesResponse : String -> String
clearItunesResponse res =
    res
        |> String.replace "\n" ""
        |> String.replace "StrToBeReplaced" ""
        |> String.dropLeft 1
        |> String.dropRight 2


searchItunes : String -> Cmd Msg
searchItunes search =
    Http.get
        { url = "https://itunes.apple.com/search?callback=StrToBeReplaced&media=podcast&lang=en_us&term=" ++ search

        -- , expect = Http.expectString ItunesData itunesDecoder
        , expect = Http.expectString ItunesData
        }


itunesDecoder : D.Decoder ( Int, List String )
itunesDecoder =
    let
        decodeResultItem =
            D.field "feedUrl" D.string
    in
    D.map2 (\cnt data -> ( cnt, data ))
        (D.field "resultCount" D.int)
        (D.field "results" (D.list decodeResultItem))


fetchFeed : String -> Cmd Msg
fetchFeed feed =
    Http.get
        { url = feed
        , expect = Http.expectString (FeedFetched feed)
        }


podcastDecoder : String -> Xml.Decoder Podcast
podcastDecoder feed =
    Xml.map3 (Podcast feed)
        (Xml.path [ "channel", "title" ] (Xml.single Xml.string))
        (Xml.path [ "channel", "itunes:owner", "itunes:email" ] (Xml.single Xml.string))
        (Xml.path [ "channel", "itunes:owner", "itunes:name" ] (Xml.single Xml.string))



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout []
        (column [ width fill, centerY, spacing 30 ]
            [ seachElement model
            , statusBar model
            , column [ Element.centerX, Element.spacing 20 ] (List.map resultElement model.podcasts)
            ]
        )


seachElement : Model -> Element Msg
seachElement model =
    Element.row
        [ Element.centerX, Element.spacing 10 ]
        [ Input.text [] { onChange = SearchChanged, text = model.searchText, placeholder = Nothing, label = Input.labelHidden "Podcast name" }
        , Input.button
            [ Background.color (Element.rgb255 0 0 0)
            , Font.color (Element.rgb255 255 255 255)
            , Element.padding 12
            , Border.rounded 4
            ]
            { onPress = Just RunSearch, label = text "Go!" }
        ]


statusBar : Model -> Element msg
statusBar model =
    if not model.searching then
        Element.none

    else if model.loadingItunes then
        Element.el [ Element.centerX ] (text "Loading iTunes results")

    else if model.itunesError then
        Element.el [ Element.centerX ] (text "Error fetching data from iTunes")

    else
        Element.row
            [ Element.centerX, Element.width (Element.px 300), Element.spaceEvenly ]
            [ text ("Results: " ++ String.fromInt model.results)
            , text ("Parsed: " ++ String.fromInt model.parsed)
            , text ("Errors: " ++ String.fromInt model.errors)
            ]


resultElement : Podcast -> Element msg
resultElement podcast =
    Element.column
        [ Element.centerX, Background.color (Element.rgb255 200 200 200), Element.padding 10, Border.rounded 8 ]
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
