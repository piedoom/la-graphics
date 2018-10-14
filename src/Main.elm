module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img, input)
import Html.Attributes exposing (src, style, type_, value)
import Html.Events exposing (onInput)
import Validate exposing (ifNotInt, validate)

---- MODEL ----


type alias Model =
    { design: Design
    }

type alias Design =
    { width: Int
    , height: Int
    }


init : ( Model, Cmd Msg )
init =
    ( { design = 
        { width = 1200
        , height = 630
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
                nestedData = model.design
                sizeMaybe = String.toInt sizeString
            in
                case sizeMaybe of
                    Just(size) -> 
                        let 
                            updatedData = { nestedData | width = size }
                        in
                            ({ model | design = updatedData }, Cmd.none)
                    Nothing ->
                        (model, Cmd.none)
        UpdateHeight sizeString ->
            let
                nestedData = model.design
                sizeMaybe = String.toInt sizeString
            in
                case sizeMaybe of
                    Just(size) -> 
                        let 
                            updatedData = { nestedData | height = size }
                        in
                            ({ model | design = updatedData }, Cmd.none)
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
        width = px model.design.width
        height = px model.design.height
    in
        div [ style "border" "1px rgba(0,0,0,0.3) solid"
            , style "width" width
            , style "height" height ] 
            [ text "asd" ]

controls : Model -> Html Msg
controls model =
    let 
        width = String.fromInt model.design.width
        height = String.fromInt model.design.height
    in
        div []
            [ input 
                [ type_ "number"
                , value width
                , onInput UpdateWidth ] 
                []
            , input
                [ type_ "number"
                , value height
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