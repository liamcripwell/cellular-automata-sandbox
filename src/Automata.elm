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
  }

type alias Cell = (Int, Int)

automataStep : Automaton -> Automaton
automataStep automaton = 
  { automaton | liveCells = automaton.liveCells
              , deadCells = automaton.deadCells } 

buildAutomaton : Int -> Int -> Int -> Automaton
buildAutomaton width height cellDim = 
  { liveCells = []
  , deadCells = cartesian (List.range 0 (width-1)) (List.range 0 (height-1))
  , liveRules = []
  , deadRules = []
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
