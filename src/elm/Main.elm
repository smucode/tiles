module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Random exposing (map, bool)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- APP


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Surface =
    { width : Int, height : Int }


type Grid
    = List Bool
    | Nothing


type alias Model =
    { tile : Surface
    , area : Surface
    , grid : List Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { area = { width = 1000, height = 200 }
      , tile = { width = 30, height = 30 }
      , grid = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | Update (List Bool)
    | Randomize


generateMatrix : Model -> Cmd Msg
generateMatrix model =
    let
        rows =
            model.area.height // model.tile.height

        cols =
            model.area.width // model.tile.width
    in
        Random.generate Update (Random.list (rows * cols) Random.bool)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        Update g ->
            ( { model | grid = g }
            , Cmd.none
            )

        Randomize ->
            ( model
            , generateMatrix model
            )



-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


renderRow : List Bool -> Int -> Int -> List (Html msg)
renderRow list cols row =
    let
        styles =
            [ ( "display", "inline-block" ), ( "height", "25px" ), ( "width", "25px" ) ]
    in
        List.map
            (\col ->
                let
                    bgColor =
                        if Maybe.withDefault True (Array.get ((row + 1) * (col + 1)) (Array.fromList list)) then
                            "#ccc"
                        else
                            "#ddd"
                in
                    div
                        [ style (styles ++ [ ( "background", bgColor ) ])
                        ]
                        [ text "" ]
            )
            (List.range 0 cols)


tiles : Model -> List (Html msg)
tiles model =
    let
        rows =
            model.area.height // model.tile.height

        cols =
            model.area.width // model.tile.width
    in
        List.map (\n -> div [ style [ ( "height", "25px" ) ] ] (renderRow model.grid cols n)) (List.range 0 rows)


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "margin", "30px" )
            ]
        ]
        [ div
            []
            [ div
                [ style
                    [ ( "height", toString model.area.height ++ "px" )
                    , ( "width", toString model.area.width ++ "px" )
                    ]
                ]
                (tiles model)
            , button
                [ class "btn btn-primary btn-lg", onClick Randomize ]
                [ text "RANDOMIZE"
                ]
            ]
        ]



-- CSS STYLES


styles : { img : List ( String, String ) }
styles =
    { img =
        [ ( "width", "33%" )
        , ( "border", "4px solid #337AB7" )
        ]
    }



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
