module Main exposing (Model, Msg(..), Size, buildUnsplashQuery, controls, design, getImage, init, main, px, update, view)

import Browser
import Html exposing (Html, div, h1, img, input, text)
import Html.Attributes exposing (placeholder, src, step, style, type_, value)
import Html.Events exposing (onInput)
import Http exposing (Request)
import Validate exposing (ifNotInt, validate)



---- MODEL ----


type alias Model =
    { width : Int
    , height : Int
    , query : String
    , imageUrl : String
    }


init : ( Model, Cmd Msg )
init =
    ( { width = 1200
      , height = 630
      , query = ""
      , imageUrl = ""
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateWidth String
    | UpdateHeight String
    | UpdateQuery String
    | UpdateImage (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateWidth sizeString ->
            let
                sizeMaybe =
                    String.toInt sizeString
            in
            case sizeMaybe of
                Just size ->
                    ( { model | width = size }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateHeight sizeString ->
            let
                sizeMaybe =
                    String.toInt sizeString
            in
            case sizeMaybe of
                Just size ->
                    ( { model | height = size }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateQuery search ->
            -- ( { model | query = search }, getImage model )
            let
                updatedModel =
                    { model | query = search }

                url =
                    buildUnsplashQuery updatedModel
            in
            ( { model | imageUrl = url }, Cmd.none )

        UpdateImage result ->
            case result of
                Ok newUrl ->
                    ( { model | imageUrl = newUrl }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ controls model
        , design model
        ]


design : Model -> Html Msg
design model =
    let
        width =
            px model.width

        height =
            px model.height

        background =
            model.imageUrl
    in
    div
        [ style "border" "1px rgba(0,0,0,0.3) solid"
        , style "width" width
        , style "height" height
        , style "background-image" ("url(" ++ background ++ ")")
        ]
        []


controls : Model -> Html Msg
controls model =
    let
        width =
            String.fromInt model.width

        height =
            String.fromInt model.height
    in
    div []
        [ input
            [ type_ "number"
            , value width
            , step "50"
            , onInput UpdateWidth
            ]
            []
        , input
            [ type_ "number"
            , value height
            , step "50"
            , onInput UpdateHeight
            ]
            []
        , input
            [ type_ "text"
            , placeholder "search"
            , onInput UpdateQuery
            ]
            []
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



--


px : Int -> String
px attr =
    String.fromInt attr ++ "px"



---- OTHER ----


type alias Size =
    { width : Int
    , height : Int
    }



-- Search a term and get a url to be used in a query


buildUnsplashQuery : Model -> String
buildUnsplashQuery model =
    let
        width =
            String.fromInt model.width

        height =
            String.fromInt model.height

        search =
            model.query
    in
    "https://source.unsplash.com/" ++ width ++ "x" ++ height ++ "/?" ++ search



-- Return an image from a query (does not get a specific image via ID)


getImage : Model -> Cmd Msg
getImage model =
    Http.send UpdateImage (Http.getString (buildUnsplashQuery model))
