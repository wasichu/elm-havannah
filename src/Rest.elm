module Rest exposing (..)

import Dict
import List
import Set exposing (Set)

import Hexagons.Hex as Hex exposing (Hex(..), Direction(..))
import Hexagons.Map as HexMap

areNeighbors board hh1 hh2 =
  let
      defaultHex = IntCubeHex (0, 0, 0)

      hex1 = HexMap.getHex defaultHex board hh1

      hex2 = HexMap.getHex defaultHex board hh2
  in
     Set.member hh1 (neighbors board hex2)

neighbors : HexMap.Map -> Hex -> Set HexMap.Hash
neighbors board hex =
  let
      dirs = [ NE, E, SE, SW, W, NW] 

      neighborsHashed = 
        List.map (Hex.neighbor hex >> HexMap.hashHex) dirs

      onBoard hexHash = Dict.member hexHash board

      neighborsOnBoard =
        List.filter onBoard neighborsHashed
  in
     Set.fromList neighborsOnBoard

