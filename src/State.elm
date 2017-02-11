module State exposing (..)

{--
  State for the Havannah game app which includes things like

    -- init
    -- update
    -- subscriptions
--}

import Debug
import Dict
import Dom.Scroll exposing (toBottom)
import List exposing ((::))
import Maybe exposing (Maybe(..))
import Set exposing (Set(..))
import Task

import Hexagons.Hex as Hex exposing (Hex(..), Direction(..))
import Hexagons.Layout exposing 
  ( Orientation
  , orientationLayoutPointy
  )
import Hexagons.Map as HexMap

import Rest exposing (..)
import Types exposing (..)

-- CONSTANTS
orientationLayoutFlat =
  { forward_matrix =
      ( 3.0 / 2.0, 0.0, sqrt(3.0) / 2.0, sqrt(3.0) )
  , inverse_matrix =
      ( 2.0 / 3.0, 0.0, -1.0 / 3.0, sqrt(3.0) )
  , start_angle = 0.0
  }

-- UPDATE
update msg model =

  case msg of 

    PlaceCell hex ->
      let
          hash = HexMap.hashHex hex 

          newP1Moves =
            if model.turn == Player1 then
              Set.insert hash model.p1Moves
            else if model.turn == Player2 &&
                    Set.member hash model.p1Moves then
              Set.empty
            else
              model.p1Moves
  
          newP2Moves =
            if model.turn == Player2 then
               Set.insert hash model.p2Moves
            else
              model.p2Moves

          newMoves = 
            model.moves ++ [hash]

          nextTurn = if model.turn == Player1 then Player2 else Player1

          newP1Connections =
            if List.member hash model.moves then
               Dict.empty
            else if model.turn == Player2 then
               model.p1Connections
            else if Dict.isEmpty model.p1Connections then
              Dict.insert hash (Set.fromList [hash]) model.p1Connections
            else
              updateConnections hex model.board model.p1Connections

          newP2Connections =
            if model.turn == Player1 then
               model.p2Connections
            else if Dict.isEmpty model.p2Connections then
              Dict.insert hash (Set.fromList [hash]) model.p2Connections
            else
              updateConnections hex model.board model.p2Connections

          win = isWin { model 
                      | p1Connections = newP1Connections
                      , p2Connections = newP2Connections
                      }

          draw = isDraw { model | moves = newMoves }

          newState =
            if win && model.turn == Player1 then
              P1Wins
            else if win && model.turn == Player2 then
              P2Wins
            else if draw then
              Draw
            else
              model.gameState

      in
        ( { model 
          | p1Moves = newP1Moves
          , p2Moves = newP2Moves
          , turn = nextTurn
          , moves = newMoves
          , p1Connections = newP1Connections
          , p2Connections = newP2Connections
          , gameState = newState
          }
        , Task.attempt (\_ -> NoOp) (toBottom "moves-list")
        )

    HoverCell hex ->
      let 
          hash = HexMap.hashHex hex

          newHoverCell =
            -- For the swap rule, allow the first placed
            -- stone to be hovered over
            if List.length model.moves /= 1 &&
               List.member hash model.moves then
               Nothing
            else
              Just hex

          newModel =
            { model
                | hoverCell = newHoverCell
            }
      in
         ( newModel, Cmd.none )

    UnhoverCell ->
      ( { model | hoverCell = Nothing }, Cmd.none )

    NoOp ->
      ( model, Cmd.none )

initModel = 
  let 
      layout =
              { orientation = orientationLayoutFlat
                --orientation = orientationLayoutPointy,
              , size = ( 20.0, 20.0 )
              , origin = ( 300.0, 340.0 )
              }

      size = Ten

      board = makeGameBoard size

      (corners, edges) = findCornersAndEdges board size

  in
     { layout = layout
     , boardSize = size
     , board = board
     , corners = corners
     , edges = edges
     , p1Connections = Dict.empty 
     , p2Connections = Dict.empty
     , turn = Player1
     , gameState = Started
     , moves = []
     , p1Moves = Set.empty
     , p2Moves = Set.empty
     , hoverCell = Nothing
     , p1Color = "DodgerBlue"
     , p2Color = "gray"
     }

init =
  ( initModel
  , Cmd.none
  )

-- SUBSCRIPTIONS
subscriptions model =
  Sub.none

-- HELPER FUNCTIONS
makeGameBoard : BoardSize -> HexMap.Map
makeGameBoard boardSize =
  let
    mapRadius = if boardSize == Ten then 9 else 7 

    qs = List.range -mapRadius mapRadius

    rs q =
      let 
          r1 = max -mapRadius (-q - mapRadius)
          r2 = min mapRadius (-q + mapRadius)
      in
        List.range r1 r2

    hexBuild (q, r) =
      let
          s = -q - r
      in
         ( (q, r, s)
         , IntCubeHex (q, r, s)
         )

    qrs = List.concatMap (\q -> List.map ((,) q) (rs q)) qs

  in 
    Dict.fromList <| List.map hexBuild qrs

findCornersAndEdges : HexMap.Map -> BoardSize -> ( Set HexMap.Hash, List (Set HexMap.Hash ) )
findCornersAndEdges board boardSize =
  let
    neighborCount hex =
        ( HexMap.hashHex hex
        , Set.size (neighbors board hex)
        )

    neighborCounts =
      let
        neighborCountsList =
          List.map neighborCount (Dict.values board)
      in
        Dict.fromList neighborCountsList

    isCorner _ count = count == 3

    isEdge _ count = count == 4

    corners = Dict.filter isCorner neighborCounts

    edges = Dict.keys (Dict.filter isEdge neighborCounts)

    n = if boardSize == Ten then 9 else 7

    xEquals v (x, _, _) = x == v

    yEquals v (_, y, _) = y == v

    zEquals v (_, _, z) = z == v

    -- Now need to separate the edges into distinct
    -- groups based on shared x, y, and z values
    edgeGroups = [ List.filter (xEquals n) edges
                 , List.filter (xEquals -n) edges
                 , List.filter (yEquals n) edges
                 , List.filter (yEquals -n) edges
                 , List.filter (zEquals n) edges
                 , List.filter (zEquals -n) edges
                 ]
  in
    ( Set.fromList (Dict.keys corners)
    , List.map Set.fromList edgeGroups
    )

-- updateConnections assumes that the connections map
-- is non empty
updateConnections : Hex -> HexMap.Map -> HavannahConnections -> HavannahConnections
updateConnections hex board connections =
  let
    hash = 
      HexMap.hashHex hex 

    ns = 
      neighbors board hex

    isNeighbor hexHash =
      Set.member hexHash ns

    hasConnection _ hexHashSet = 
      List.any isNeighbor (Set.toList hexHashSet)

    -- newConnectedGroups is a dictionary of the extant groups
    -- connected to the newly placed hex
    nowConnectedGroups =
      Dict.filter hasConnection connections

    -- Once we have the connected groups, need to merge in
    -- the hex to all of them and potentially merge multiple
    -- groups already in the connections dict
    newConnectedGroup =
      List.foldl 
        Set.union 
        (Set.fromList [hash])
        (Dict.values nowConnectedGroups)

    hashToUpdate =
      Maybe.withDefault hash (List.head (Dict.keys nowConnectedGroups))

    updateBeforeDelete =
      Dict.insert hashToUpdate newConnectedGroup connections

    hashesToRemove = 
      Maybe.withDefault [] (List.tail (Dict.keys nowConnectedGroups))

  in
    List.foldl Dict.remove updateBeforeDelete hashesToRemove

isWin model =
  isBridge model || isFork model || isRing model

isRing model =
  let
    connections =
      if model.turn == Player1 then
         model.p1Connections
      else
        model.p2Connections

    enoughForRing _ connection =
      Set.size connection >= 6

    potentialConnections =
      Dict.filter enoughForRing connections

    neighborCountDicts =
      let
        connectionGroupList = 
          List.map Set.toList (Dict.values potentialConnections)
      in
        List.map neighborCountDict connectionGroupList

    neighborCountDict group =
      Dict.fromList (List.map (neighborCount group) group)

    neighborCount group hexHash =
      ( hexHash
      , List.filter (areNeighbors model.board hexHash) group
      )

    enoughNeighborsForRing _ neighbors = 
      case neighbors of
        [] ->
          False

        loneNeighbor::[] ->
          False

        n1::n2::[] ->
          not (areNeighbors model.board n1 n2)

        _ ->
          True
          
    filterNeighborCountDict ncd =
      let
        origSize = 
          Dict.size ncd

        filteredDict = 
          Dict.filter enoughNeighborsForRing ncd

        removeBadNeighbors dict =
          Dict.map removeBadNeighbor dict

        removeBadNeighbor _ group =
          List.filter ((flip Dict.member) filteredDict) group

      in
        if Dict.size filteredDict == origSize then
          ncd
        else
          filterNeighborCountDict (removeBadNeighbors filteredDict)

    filteredNeighborCountDicts =
      List.map (filterNeighborCountDict >> Dict.size) neighborCountDicts

  in
     List.any ((<=) 6) filteredNeighborCountDicts

isFork model =
  let
    connections =
      if model.turn == Player1 then
         model.p1Connections
      else
        model.p2Connections

    onEdge connection edgeGroup =
      List.any 
        ((flip Set.member) edgeGroup)
        (Set.toList connection)

    edgesOn connection =
      List.length (List.filter (onEdge connection) model.edges)

    connectionEdgeCounts =
      List.map edgesOn (Dict.values connections)

  in
     List.any ((==) 3) connectionEdgeCounts

isBridge model =
  let
    connections =
      if model.turn == Player1 then
         model.p1Connections
      else
        model.p2Connections

    isCorner hexHash =
      Set.member hexHash model.corners

    numCorners = Set.size << Set.filter isCorner

    numCornersInConnections =
      List.map numCorners (Dict.values connections)

  in
    List.any ((==) 2) numCornersInConnections

isDraw model =
  let 
    n = if model.boardSize == Ten then 10 else 8

    totalCells = (3 * n * n) - (3 * n) - 1

  in
    List.length model.moves == totalCells

