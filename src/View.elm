module View exposing (..)

import Char exposing (fromCode, toCode)
import Debug
import Dict
import Html exposing (div, hr, program)
import Html.Attributes as Hattr exposing (class, id, style)
import Html.Events as Hevent
import List
import Set
import String
import Svg exposing (Svg, polygon, g)
import Svg.Attributes as Sattr exposing (x, y, stroke, fill, points)
import Svg.Events as Sevent exposing (onClick, onMouseOut, onMouseOver)

import Hexagons.Hex as Hex exposing (Hex(..))
import Hexagons.Layout exposing (hexToPoint, polygonCorners)
import Hexagons.Map as HexMap

import Types exposing (..)

-- VIEW
rootView model =
  div [ class "row" ]
    [ div [ class "large-8 columns" ]
          [ Svg.svg [ Sattr.class "havannah-board" ]
                    (havannahBoard model)
          ]
    , div [ class "large-2 columns" ]
          [ Html.text (gameStateText model) ]
    ]

gameStateText model =
  case model.gameState of 
    P1Wins ->
      "Player 1 wins"

    P2Wins ->
      "Player 2 wins"

    Draw ->
      "Draw"

    Aborted ->
      "Game aborted"

    NotStarted ->
      ""

    Started ->
      if model.turn == Player1 then
         "Player 1's turn"
      else
        "Player 2's turn"

havannahBoard model =
  let
    gHexs = Dict.values model.board
  in
    List.map (gData model) gHexs

-- Given the model and a hex cell, produce the corresponding
-- g element suitable for use within an SVG 
gData model hex =
  let
    hash = HexMap.hashHex hex

    cornerPoints = 
      polygonCorners model.layout hex

    (centerX, centerY) = hexToPoint model.layout hex

    (letter, number) = cartesianHex model.boardSize hash

    isHover = model.hoverCell == Just hex

    fillColor =
      if Set.member hash model.p1Moves then
        model.p1Color
      else if Set.member hash model.p2Moves then
        model.p2Color
      else if model.turn == Player1 && isHover then
        model.p1Color
      else if model.turn == Player2 && isHover then
        model.p2Color
      else
        "DodgerBlue"

    fillOpacity =
      if isHover then
        "0.5"
      else
        "1.0"

    strokeOpacity = if isHover then "0.1" else "1.0"

    isMove = Set.member hash model.p1Moves || 
             Set.member hash model.p2Moves

    gClass =
      let
        qualifier =
          case model.gameState of
            Started ->
              if isMove then "taken" else "available"

            _ ->
              "no-game-play"

      in 
        "hex-cell-" ++ qualifier
   
    clickHandlers = 
      if model.gameState /= Started then
         []
      else
        [ onClick (PlaceCell hex)
        , onMouseOver (HoverCell hex)
        , onMouseOut UnhoverCell
        ]

  in
    g ([ Sattr.class gClass
       , Sattr.id (idString hex)
       ] ++ clickHandlers)

      [ polygon [ points (pointsString cornerPoints)
                , fill fillColor
                , Sattr.strokeOpacity strokeOpacity
                ]
                []
      , Svg.text_
          [ Sattr.stroke "none"
          , Sattr.fill "white"
          , Sattr.x (toString <| centerX)
          , Sattr.y (toString <| centerY + 5)
          , Sattr.textAnchor "middle"
          , Hattr.style [ ( "font-size", "8px" )
                        ]             
          ]
          [ Svg.text (cartesianString (letter, number)) ]
      ]

coordinateText model =
  []

-- HELPERS
printPoint (x, y) = (toString x) ++ "," ++ (toString y)

pointsString = 
  String.join " " << List.map printPoint

idString hex =
  let 
    (q, r, s) = HexMap.hashHex hex
  in
    String.join "," <| List.map (toString) [q, r, s]

cartesianHex : BoardSize -> HexMap.Hash -> (Char, Int)
cartesianHex boardSize (x, y, z) =
  let
      size = if boardSize == Ten then 9 else 7

      startLetterZ = -size

      startNumY = size

      startCharCode = toCode 'A'
  in
    ( fromCode (z + startCharCode + size)
    , -y + size + 1
    )

cartesianString (letter, number) =
  (String.fromChar letter) ++ (toString number)
