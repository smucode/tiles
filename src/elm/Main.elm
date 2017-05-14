module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Random exposing (map, bool)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random.List exposing (shuffle)


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
    | RandomizeEven
    | RandomizeUneven


randomizeUneven : Model -> Cmd Msg
randomizeUneven model =
    let
        rows =
            model.area.height // model.tile.height

        cols =
            model.area.width // model.tile.width
    in
        Random.generate Update (Random.list (rows * cols) Random.bool)


randomizeEven : Model -> Cmd Msg
randomizeEven model =
    let
        length =
            (toFloat model.area.height / toFloat model.tile.height) * (toFloat model.area.width / toFloat model.tile.width)

        grid =
            (List.repeat (ceiling length // 2) True) ++ (List.repeat (ceiling length // 2) False)
    in
        -- Random.generate Update (Random.list length Random.bool)
        Random.generate Update (Random.List.shuffle grid)


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

        RandomizeUneven ->
            ( model
            , randomizeUneven model
            )

        RandomizeEven ->
            ( model
            , randomizeEven model
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
                    idx =
                        (col + 1) + (row * (cols + 1))

                    bgColor =
                        case Array.get idx (Array.fromList list) of
                            Just True ->
                                "#ccc"

                            Just False ->
                                "#ddd"

                            _ ->
                                "red"
                in
                    div
                        [ style (styles ++ [ ( "background", bgColor ) ]) ]
                        [ text ("") ]
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
        List.map
            (\n ->
                div
                    [ style [ ( "height", "25px" ) ] ]
                    (renderRow model.grid cols n)
            )
            (List.range 0 rows)


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
            , div []
                [ button [ class "btn btn-primary", onClick RandomizeUneven ] [ text "Random Uneven Count" ]
                , button [ class "btn btn-primary", onClick RandomizeEven ] [ text "Random Even Count" ]
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
