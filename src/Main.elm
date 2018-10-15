module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img, input)
import Html.Attributes exposing (src, style, type_, value, step)
import Html.Events exposing (onInput)
import Validate exposing (ifNotInt, validate)
import Http exposing (Request)

---- MODEL ----


type alias Model =
    { controls : Controls
    }

type alias Controls =
    { width  : Int
    , height : Int
    , query  : String
    , backgroundUrl : String
    }

init : ( Model, Cmd Msg )
init =
    ( { controls = 
        { width = 1200
        , height = 630
        , query = ""
        , backgroundUrl = ""
        } 
    }, Cmd.none )

---- UPDATE ----


type Msg
    = NoOp
    | UpdateWidth String
    | UpdateHeight String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        UpdateWidth sizeString ->
            let
                nestedData = model.controls
                sizeMaybe = String.toInt sizeString
            in
                case sizeMaybe of
                    Just(size) -> 
                        let 
                            updatedData = { nestedData | width = size }
                        in
                            ({ model | controls = updatedData }, Cmd.none)
                    Nothing ->
                        (model, Cmd.none)
        UpdateHeight sizeString ->
            let
                nestedData = model.controls
                sizeMaybe = String.toInt sizeString
            in
                case sizeMaybe of
                    Just(size) -> 
                        let 
                            updatedData = { nestedData | height = size }
                        in
                            ({ model | controls = updatedData }, Cmd.none)
                    Nothing ->
                        (model, Cmd.none)


---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ controls model
        , design model ]

design : Model -> Html Msg
design model =
    let 
        width = px model.controls.width
        height = px model.controls.height
        background = model.controls.backgroundUrl
    in
        div [ style "border" "1px rgba(0,0,0,0.3) solid"
            , style "width" width
            , style "height" height
            , style "background-image" background ] 
            [ text "asd" ]

controls : Model -> Html Msg
controls model =
    let 
        width = String.fromInt model.controls.width
        height = String.fromInt model.controls.height
    in
        div []
            [ input 
                [ type_ "number"
                , value width
                , step "50"
                , onInput UpdateWidth ] 
                []
            , input
                [ type_ "number"
                , value height
                , step "50"
                , onInput UpdateHeight ] 
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
    String.fromInt attr  ++ "px"

---- OTHER ----

type alias Size =
    { width: Int
    , height: Int
    }

-- Search a term and get a url to be used in a query
getUnsplashQuery : Model -> String
getUnsplashQuery model =
    let 
        width = String.fromInt model.controls.width
        height = String.fromInt model.controls.height
        search = model.controls.query
    in
        "https://source.unsplash.com/" ++ width ++ "x" ++ height ++ "/?" ++ search

-- Return an image from a query (does not get a specific image via ID)
getRandomImage : Model -> String -> Http.Request String
getRandomImage model query =
    Http.getString (getUnsplashQuery model)