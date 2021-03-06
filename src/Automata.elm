module Automata exposing (..)

import List.Extra exposing (..)

cartesian : List a -> List b -> List (a,b)
cartesian xs ys =
  List.concatMap
    ( \x -> List.map ( \y -> (x, y) ) ys )
    xs

type alias Automaton = 
  { liveCells : List Cell
  , deadCells : List Cell
  , liveRules : List (Int -> Cell -> Maybe Cell)
  , deadRules : List (Int -> Cell -> Maybe Cell)
  , thing : List Cell
  , thing2 : List Cell
  }

type alias Cell = (Int, Int)



allNeighbors : List (Int, Int) -> (Int, Int) -> List (Int, Int)
allNeighbors state (x, y) = 
  cartesian (List.range (x-1) (x+1)) (List.range (y-1) (y+1))

liveneighbors : List (Int, Int) -> (Int, Int) -> List (Int, Int)
liveneighbors state (x, y) = 
  allNeighbors state (x, y)
  |> List.filter (\a -> List.member a state)


automataStep : Automaton -> Automaton
automataStep automaton = 
  let
    applyLiveRule rule cell = 
      let 
        neighbors = liveneighbors automaton.liveCells cell
        neighborCount = (List.length neighbors) - 1
      in
        rule neighborCount cell

    liveRuleApplications = List.map applyLiveRule automaton.liveRules
    liveRuleResults = List.map (\x -> 
                                 if (List.length <| List.filterMap identity (List.map (\f -> f x) liveRuleApplications)) == List.length automaton.liveRules then [x]
                                 else [] 
                               ) automaton.liveCells
    newLive = List.Extra.unique <| List.foldr (++) [] liveRuleResults

    applyDeadRule rule cell = 
      let 
        neighbors = liveneighbors automaton.liveCells cell
        neighborCount = (List.length neighbors)
      in
        rule neighborCount cell

    deadRuleApplications = List.map applyDeadRule automaton.deadRules
    deadRuleResults = List.map (\x -> 
                                 if (List.length <| List.filterMap identity (List.map (\f -> f x) deadRuleApplications)) == List.length automaton.deadRules then [x]
                                 else [] 
                               ) automaton.deadCells
    newDead = List.Extra.unique <| List.foldr (++) [] deadRuleResults
  in
    { automaton | liveCells = automaton.liveCells
                , deadCells = automaton.deadCells -- List.drop 1 automaton.deadCells 
                , thing = newLive
                , thing2 = newDead} 

buildAutomaton : Int -> Int -> Int -> Automaton
buildAutomaton width height cellDim = 
  { liveCells = [(1,0),(1,1),(1,2),(0,1)] -- []
  , deadCells = [(0,0),(0,2),(2,0),(2,1),(2,2),(3,0),(3,1),(3,2),(4,0),(4,1),(4,2)] -- cartesian (List.range 0 (width-1)) (List.range 0 (height-1))
  , liveRules = []
  , deadRules = []
  , thing = []
  , thing2 = []
  }
  
setCell : Cell -> Automaton -> Automaton
setCell cell automaton  = 
  { automaton | liveCells = cell :: automaton.liveCells
              , deadCells = remove cell automaton.deadCells }

killCell : Cell -> Automaton -> Automaton
killCell cell automaton  = 
  { automaton | liveCells = remove cell automaton.liveCells
              , deadCells = cell :: automaton.deadCells }

buildGameOfLife : Int -> Int -> Int -> Automaton
buildGameOfLife width height cellDim = 
  let
    automaton = buildAutomaton width height cellDim
    
    r1 neighborCount cell =
      if neighborCount < 2 then Nothing else Just cell

    r2 neighborCount cell =
      if neighborCount == 2 || neighborCount == 3 then Just cell else Nothing

    r3 neighborCount cell =
      if neighborCount > 3 then Nothing else Just cell

    r4 neighborCount cell =
      if neighborCount == 3 then Just cell else Nothing
  in
    { automaton | liveRules = [ r1, r2, r3 ]
                , deadRules = [ r4 ] }
