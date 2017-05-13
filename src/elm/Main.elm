module Main exposing (..)

import Html exposing (..)
import Random exposing (map, bool)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- component import example

import Components.Hello exposing (hello)


-- APP


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Surface =
    { width : Int, height : Int }


type alias Model =
    { number : Int
    , tile : Surface
    , area : Surface
    }


model : Model
model =
    { number = 10
    , area = { width = 1000, height = 200 }
    , tile = { width = 30, height = 30 }
    }



-- UPDATE


type Msg
    = NoOp
    | Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Increment ->
            { model | number = model.number + 1 }



-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


renderRow : Int -> Int -> List (Html msg)
renderRow cols n =
    let
        bgColor =
            Random.map
                (\b ->
                    if b then
                        "#eee"
                    else
                        "#ddd"
                )
                bool

        -- if MAth.random() > .5 then '#eee' else '#ddd'
        styles =
            [ ( "display", "inline-block" ), ( "height", "25px" ), ( "width", "25px" ), ( "background", (Random.generate (\s -> s) bgColor) ) ]
    in
        List.map (\n -> div [ style styles ] []) (List.range 0 cols)


tiles : Model -> List (Html msg)
tiles model =
    let
        rows =
            model.area.height // model.tile.height

        cols =
            model.area.width // model.tile.width
    in
        List.map (\n -> div [] (renderRow cols n)) (List.range 0 rows)


view : Model -> Html Msg
view model =
    div [ class "container", style [ ( "margin-top", "30px" ), ( "text-align", "center" ) ] ]
        [ -- inline CSS (literal)
          div [ class "row" ]
            [ div [ class "col-xs-12" ]
                [ div [ style [ ( "height", toString model.area.height ++ "px" ), ( "width", toString model.area.width ++ "px" ), ( "background", "#eee" ) ] ] (tiles model)
                , div [ class "jumbotron" ]
                    [ hello model.number
                      -- ext 'hello' component (takes 'model' as arg)
                    , p [] [ text ("Elm Webpack Starter") ]
                    , button [ class "btn btn-primary btn-lg", onClick Increment ]
                        [ -- click handler
                          span [ class "glyphicon glyphicon-star" ] []
                          -- glyphicon
                        , span [] [ text "FTW!" ]
                        ]
                    ]
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
