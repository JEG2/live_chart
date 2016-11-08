module Graph exposing(..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String
import Json.Decode exposing (..)


type alias Model =
    { limit : Int
    , width : Int
    , height : Int }

type Msg =
    ChangeLimit String |
    ChangeHeight String |
    ChangeWidth String


init : Model
init =
    { limit = 10, width = 600, height = 200 }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeLimit newLimit ->
            {model | limit = (Result.withDefault 2 (String.toInt newLimit)) }
        ChangeHeight newHeight ->
            {model | height = (Result.withDefault 2 (String.toInt newHeight)) }
        ChangeWidth newWidth ->
            {model | width = (Result.withDefault 2 (String.toInt newWidth)) }


view : Model -> List Int -> Html Msg
view model measurements =
    let
        viewBox' = [-15, 0, (model.width + 15), (model.height + 15)] |> List.map toString |> String.join " "
        maxY = Maybe.withDefault 1 (List.maximum measurements)
    in
        div [ Html.Attributes.style [("padding-left", "5px"), ("padding-top", "5px")] ]
            [ div [] [ label [ ] [ Html.text "Limit" ], input [Html.Attributes.type' "range", Html.Attributes.value (toString model.limit), onInput ChangeLimit] [ ] ]
            , div [] [ label [ ] [ Html.text "Height" ], input [Html.Attributes.type' "range", Html.Attributes.value (toString model.height), Html.Attributes.min "50", Html.Attributes.max "500", Html.Attributes.step "10", onInput ChangeHeight] [ ] ]
            , div [] [ label [ ] [ Html.text "Width" ], input [Html.Attributes.type' "range", Html.Attributes.value (toString model.width), Html.Attributes.min "100", Html.Attributes.max "1500", Html.Attributes.step "10", onInput ChangeWidth] [ ] ]
            , Svg.svg [ Svg.Attributes.height (toString model.height)
                      , Svg.Attributes.width (toString model.width)
                      , Svg.Attributes.viewBox viewBox']
                      [ drawXScale model.width model.height model.limit
                      , drawYScale model.height maxY
                      , drawLine model.width model.height model.limit measurements ]
            , div [] (List.map viewMessage (List.reverse measurements))
            ]


onChange : (String -> a) -> Html.Attribute a
onChange message =
    on "change" (Json.Decode.map message targetValue)


viewMessage : Int -> Html msg
viewMessage msg =
    div [] [ Html.text (toString msg) ]


drawXScale : Int -> Int -> Int -> Svg a
drawXScale width height limit =
    g [ ] [ Svg.line [ x1 "0", y1 (toString (height)), x2 (toString (width)), y2 (toString height), stroke "black"] [ ]
          , drawXTicks width height limit [ ]
          ]


drawXTicks : Int -> Int -> Int -> List (Svg a)-> Svg a
drawXTicks width height limit acc =
    let
        gap = round ((toFloat width) / ((toFloat limit) - 1))
        xCoord = (List.length acc) * gap
    in
        if (xCoord <= width) then
            drawXTicks width height limit ( (g [ ] [ Svg.line [ x1 (toString xCoord), y1 (toString (height + 2 )), x2 (toString xCoord), y2 (toString (height - 2)), stroke "black" ] [ ]
                                                   , Svg.text' [ x (toString xCoord), y (toString (height + 10)), fontSize "10px", stroke "black"] [ Svg.text (toString (List.length acc)) ] ] :: acc) )
        else
            g [ ] acc

drawYScale : Int -> Int -> Svg a
drawYScale height maxY =
    g [ ] [Svg.line [ x1 "0", y1 (toString height), x2 "0", y2 "0", stroke "black"] [ ]
          , drawYTicks height 10 [ ] ]


drawYTicks : Int -> Int -> List (Svg a) -> Svg a
drawYTicks height count acc =
    let
        gap = round ((toFloat height) / (toFloat count))
        yCoord = (List.length acc) * gap
    in
        if (yCoord <= height) then
            drawYTicks height count ( (g [ ] [ Svg.line [ x1 "-2", y1 (toString yCoord), x2 "2", y2 (toString yCoord), stroke "black"] []
                                             , Svg.text' [ x "-20", y (toString (yCoord + 10)), fontSize "10px", stroke "black" ] [ Svg.text (toString (gap * (count - (List.length acc))))] ]) :: acc)
        else
            g [] acc


drawLine : Int -> Int -> Int -> List Int -> Svg a
drawLine width height limit measurements =
    let
        coords = List.indexedMap (,) measurements
    in
        g [ ] (drawSegments width height limit coords)


drawSegments : Int -> Int -> Int -> List (Int, Int) -> List (Svg a)
drawSegments width height limit coords =
  case coords of
    coord1 :: coord2 :: [ ]  -> [drawSegment width height limit coord1 coord2]
    coord1 :: coord2 :: tail -> [(drawSegment width height limit coord1 coord2)] ++ (drawSegments width height limit (coord2 :: tail))
    _ -> [ ]


drawSegment : Int -> Int -> Int -> (Int, Int) -> (Int, Int) -> Svg a
drawSegment width height limit coord1 coord2 =
  let
    (xs1, ys1) = coord1
    (xs2, ys2) = coord2
  in
    line [ x1 (adjustX width limit xs1), y1 (adjustY height ys1), x2 (adjustX width limit xs2), y2 (adjustY height ys2), stroke "lime" ] []


adjustX : Int -> Int -> Int -> String
adjustX width limit x =
    let
        gap = round ((toFloat width) / (toFloat (limit - 1)))
    in
        toString (x * gap)


adjustY : Int -> Int -> String
adjustY height y =
    toString (height - y)
