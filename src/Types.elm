module Types exposing (..)

{--
  Types for implementing the game of Havannah
--}
import Dict 
import Set

import Hexagons.Hex as Hex exposing (Hex(..))
import Hexagons.Layout exposing (Layout)
import Hexagons.Map as HexMap

-- Message types for the update function
type HavannahMsg
  = PlaceCell Hex
  -- Make a move by putting a stone / piece in a particular cell
  | HoverCell Hex
  -- Mouse over a particular cell
  | UnhoverCell 
  -- NoOp
  | NoOp

type BoardSize
  = Ten
  | Eight

type HavannahTurn
  = Player1
  | Player2

type HavannahGameState
  = NotStarted
  | Started
  | Aborted
  | P1Wins
  | P2Wins
  | Draw

type alias HavannahConnections =
  Dict.Dict HexMap.Hash (Set.Set HexMap.Hash)

type alias HavannahGame =
  { layout : Layout
  , boardSize : BoardSize
  , board : HexMap.Map
  , corners : Set.Set HexMap.Hash
  , edges : List (Set.Set HexMap.Hash)
  , p1Connections : HavannahConnections
  , p2Connections : HavannahConnections
  , turn : HavannahTurn
  , gameState : HavannahGameState
  , moves : List HexMap.Hash
  , p1Moves : Set.Set HexMap.Hash
  , p2Moves : Set.Set HexMap.Hash
  , hoverCell : Maybe Hex
  , p1Color : String
  , p2Color : String
  }
