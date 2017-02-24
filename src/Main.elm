module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, int)
import Json.Decode.Pipeline as JSP
import Svg
import Svg.Attributes as SA
import Svg.Events as SE
import List
import Hull exposing (..)
import Polygon exposing (..) 


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { points : List Point
    , hull : Maybe (List Point)
    }


init : Model
init =
    { points = []
    , hull = Nothing
    }


type Msg
    = AddPoint Int Int
    | Clear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ points } as model) =
    case msg of
        AddPoint x y ->
            let
                newPoints =
                    (Point x y) :: points
            in
                if List.length newPoints > 2 then
                    { model | points = newPoints, hull = findHull newPoints } ! []
                else
                    { model | points = newPoints, hull = Nothing } ! []

        Clear ->
            { model | points = [], hull = Nothing } ! []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view { points, hull } =
    div []
        [ Svg.svg [ SA.width "600", SA.height "600", SA.viewBox "0 0 600 600", onClickXY AddPoint ]
            ((Svg.polyline [ SA.points "0,0 0,600 600,600 600,0 0,0", SA.style "fill:none;stroke-width:1;stroke:rgb(0,0,0)" ] [])
                :: ((List.map viewPoint points)
                        ++ (case hull of
                                Just hull ->
                                    List.map svgLine (edges hull)

                                Nothing ->
                                    []
                           )
                   )
            )
        , button [ onClick Clear ] [ text "clear" ]
        ]


svgLine : ( Point, Point ) -> Svg.Svg msg
svgLine ( Point x1 y1, Point x2 y2 ) =
    Svg.line
        [ SA.x1 <| toString x1
        , SA.y1 <| toString y1
        , SA.x2 <| toString x2
        , SA.y2 <| toString y2
        , SA.style "stroke:rgb(255,0,0);stroke-width:2"
        ]
        []


viewPoint : Point -> Svg.Svg msg
viewPoint (Point x y) =
    Svg.circle [ SA.cx <| toString x, SA.cy <| toString y, SA.r "5" ] []


decodeOnClick : Decoder ( Int, Int )
decodeOnClick =
    JSP.decode (\x y -> ( x, y ))
        |> JSP.required "pageX" int
        |> JSP.required "pageY" int



-- what is the difference between clientX/Y, pageX/Y, layerX/Y


onClickXY : (Int -> Int -> msg) -> Svg.Attribute msg
onClickXY toMsgFn =
    SE.on "click" (Json.Decode.map (\( x, y ) -> toMsgFn x y) decodeOnClick)
