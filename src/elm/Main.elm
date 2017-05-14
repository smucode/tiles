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
    , rows : Int
    , cols : Int
    , numTiles : Int
    }


init : ( Model, Cmd Msg )
init =
    let
        areaWidth =
            1000

        areaHeight =
            200

        tileWidth =
            30

        tileHeight =
            30
    in
        ( { area = { width = areaWidth, height = areaHeight }
          , tile = { width = tileWidth, height = tileHeight }
          , grid = []
          , rows = areaHeight // tileHeight
          , cols = areaWidth // tileWidth
          , numTiles = (areaHeight // tileHeight) * (areaWidth // tileWidth)
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
    Random.generate Update (Random.list (model.numTiles + 1) Random.bool)


randomizeEven : Model -> Cmd Msg
randomizeEven model =
    let
        length =
            (((model.area.height // model.tile.height) + 1) * ((model.area.width // model.tile.width)) + 1) // 2

        grid =
            (List.repeat length True) ++ (List.repeat length False)
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


renderRow : Model -> Int -> List (Html msg)
renderRow model row =
    let
        styles =
            [ ( "display", "inline-block" ), ( "height", "25px" ), ( "width", "25px" ) ]
    in
        List.map
            (\col ->
                let
                    idx =
                        col + (row - 1) * model.cols

                    bgColor =
                        case Array.get idx (Array.fromList model.grid) of
                            Just True ->
                                "#bbb"

                            Just False ->
                                "#ddd"

                            _ ->
                                "black"
                in
                    div
                        [ style (styles ++ [ ( "background", bgColor ) ]) ]
                        [ text ("") ]
            )
            (List.range 1 model.cols)


tiles : Model -> List (Html msg)
tiles model =
    List.map
        (\n ->
            div
                [ style [ ( "height", "25px" ) ] ]
                (renderRow model n)
        )
        (List.range 1 model.rows)


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
                [ button [ class "btn btn-primary", onClick RandomizeUneven ] [ text "Random" ]
                , button [ class "btn btn-primary", onClick RandomizeEven ] [ text "Random (Equal)" ]
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
