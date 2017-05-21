module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Random exposing (map, bool)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random.List exposing (shuffle)
import Html.Events exposing (onInput)


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


type alias Dimension =
    { width : Int, height : Int }


type Grid
    = List Bool
    | Nothing


type alias Model =
    { tile : Dimension
    , area : Dimension
    , grid : List Bool
    , rows : Int
    , cols : Int
    , numTiles : Int
    }


numRows : Model -> Int
numRows model =
    model.area.height // model.tile.height


numCols : Model -> Int
numCols model =
    model.area.width // model.tile.width


numTiles : Model -> Int
numTiles model =
    numRows model * numCols model


recalcFields : Model -> Model
recalcFields model =
    { model
        | rows = numRows model
        , cols = numCols model
        , numTiles = numTiles model
    }


updateAreaHeight : String -> Model -> Model
updateAreaHeight height model =
    recalcFields
        { model
            | area =
                { height = Result.withDefault model.area.height (String.toInt height)
                , width = model.area.width
                }
        }


updateAreaWidth : String -> Model -> Model
updateAreaWidth width model =
    recalcFields
        { model
            | area =
                { width = Result.withDefault model.area.width (String.toInt width)
                , height = model.area.height
                }
        }


updateTileHeight : String -> Model -> Model
updateTileHeight height model =
    recalcFields
        { model
            | tile =
                { height = Result.withDefault model.tile.height (String.toInt height)
                , width = model.tile.width
                }
        }


updateTileWidth : String -> Model -> Model
updateTileWidth width model =
    recalcFields
        { model
            | tile =
                { width = Result.withDefault model.tile.width (String.toInt width)
                , height = model.tile.height
                }
        }


countBool : Bool -> Model -> String
countBool bool model =
    (toString
        (List.foldl
            (\a b ->
                (if a == bool then
                    b + 1
                 else
                    b
                )
            )
            0
            model.grid
        )
    )


init : ( Model, Cmd Msg )
init =
    let
        model =
            { area = { width = 1000, height = 200 }
            , tile = { width = 30, height = 30 }
            , grid = []
            , rows = 0
            , cols = 0
            , numTiles = 0
            }
    in
        ( recalcFields model
        , Cmd.none
        )



-- UPDATE


type Msg
    = NoOp
    | Update (List Bool)
    | RandomizeEven
    | RandomizeUneven
    | ChangeAreaWidth String
    | ChangeAreaHeight String
    | ChangeTileWidth String
    | ChangeTileHeight String


randomizeUneven : Model -> Cmd Msg
randomizeUneven model =
    Random.generate Update (Random.list (model.numTiles + 1) Random.bool)


randomizeEven : Model -> Cmd Msg
randomizeEven model =
    let
        length =
            (model.rows + 1) * (model.cols + 1) // 2

        grid =
            (List.repeat length True) ++ (List.repeat length False)
    in
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

        ChangeAreaWidth width ->
            ( updateAreaWidth width model
            , Cmd.none
            )

        ChangeAreaHeight height ->
            ( updateAreaHeight height model
            , Cmd.none
            )

        ChangeTileWidth width ->
            ( updateTileWidth width model
            , Cmd.none
            )

        ChangeTileHeight height ->
            ( updateTileHeight height model
            , Cmd.none
            )



-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


renderRow : Model -> Int -> List (Html msg)
renderRow model row =
    let
        styles =
            [ ( "display", "inline-block" ), ( "height", (toString model.tile.height) ++ "px" ), ( "width", (toString model.tile.width) ++ "px" ) ]
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
                [ style [ ( "height", (toString model.tile.height) ++ "px" ) ] ]
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
            , fieldset []
                [ input [ type_ "number", step "10", value (toString model.area.width), placeholder "Area Width", onInput ChangeAreaWidth ] []
                , input [ type_ "number", step "10", value (toString model.area.height), placeholder "Area Height", onInput ChangeAreaHeight ] []
                , input [ type_ "number", step "10", value (toString model.tile.width), placeholder "Tile Width", onInput ChangeTileWidth ] []
                , input [ type_ "number", step "10", value (toString model.tile.height), placeholder "Tile Hiehgt", onInput ChangeTileHeight ] []
                , div [ class "pad-r-ch pad-t" ]
                    [ span [] [ text ("rows:" ++ (toString model.rows)) ]
                    , span [] [ text ("cols:" ++ (toString model.cols)) ]
                    , span [] [ text ("tiles:" ++ (toString model.numTiles)) ]
                    , span [] [ text ("light:" ++ (countBool True model)) ]
                    , span [] [ text ("dark:" ++ (countBool False model)) ]
                    ]
                ]
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
