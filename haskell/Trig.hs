{-# LANGUAGE TupleSections #-}

module Trig (trig0) where

import Base
import Simplify (simplify)
import Parser
import qualified Data.Map.Strict as M
import Data.Ratio (numerator, denominator)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)

------------------------------------------------------------
-- Lookup tables
------------------------------------------------------------

trigSinTable :: M.Map (Integer,Integer) TreeNode
trigSinTable = M.map (sortTree . simplify) $ M.fromList
  [ ((0,1), treeForm "d_0")
  , ((1,6), parse "1/2")
  , ((1,4), parse "2^(1/2)/2")
  , ((1,3), parse "3^(1/2)/2")
  , ((1,2), parse "1")
  , ((2,3), parse "3^(1/2)/2")
  , ((3,4), parse "2^(1/2)/2")
  , ((5,6), parse "1/2")
  , ((1,1), treeForm "d_0")
  ]

trigCosTable :: M.Map (Integer,Integer) TreeNode
trigCosTable = M.map (sortTree . simplify) $ M.fromList
  [ ((0,1), treeForm "d_1")
  , ((1,6), parse "(3^(1/2))/2")
  , ((1,4), parse "2^(1/2)/2")
  , ((1,3), parse "1/2")
  , ((1,2), treeForm "d_0")
  , ((2,3), parse "-1/2")
  , ((3,4), parse "-(2^(1/2))/2")
  , ((5,6), parse "-1/2")
  , ((1,1), treeForm "d_-1")
  ]

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

-- Check if a TreeNode is a negative integer
isNeg :: TreeNode -> Bool
isNeg t = case name t of
  ('d':'_':xs) -> case reads xs of
                    [(n,"")] -> n < 0
                    _ -> False
  _ -> False

-- Safe head
headSafe :: [a] -> a
headSafe []    = error "Empty children in TreeNode"
headSafe (x:_) = x

singlePi :: [TreeNode] -> Maybe (Integer,Integer)
singlePi lst
  | any (== treeForm "d_0") lst = Just (0,1)
  | countPi /= 1                = Nothing
  | otherwise =
      case frac (simplify (fDiv (mulAll lst) s_pi)) of
        Just r | r >= 0 ->
          let a0 = numerator r
              b  = denominator r
              a1 = a0 `mod` (2*b)
              a2 = if a1 > b then 2*b - a1 else a1
          in Just (a2,b)
        _ -> Nothing
  where
    countPi = length [x | x <- lst, x == s_pi]

------------------------------------------------------------
-- Main trig0 function
------------------------------------------------------------

trig0 :: TreeNode -> TreeNode
trig0 eq = case name eq of

  "f_arccosec" -> fDiv (treeForm "d_1") (headSafe (children eq)) `fx` "arcsin"

  "f_arctan" ->
    let t = headSafe (children eq)
    in if name t == "d_0" then treeForm "d_0" else TreeNode (name eq) [trig0 t]

  "f_log" ->
    let t = headSafe (children eq)
    in if name t == "d_1" then treeForm "d_0" else TreeNode (name eq) [trig0 t]

  "f_tan" ->
    let t = headSafe (children eq)
    in case name t of
         "f_arctan" -> headSafe (children t)
         _          -> fDiv (t `fx` "sin") (t `fx` "cos")

  "f_sec"   -> fPow (headSafe (children eq) `fx` "cos") (treeForm "d_-1")
  "f_cosec" -> fPow (headSafe (children eq) `fx` "sin") (treeForm "d_-1")
  "f_cot"   -> let t = headSafe (children eq)
               in fDiv (t `fx` "cos") (t `fx` "sin")

  "f_sin" ->
    let t = headSafe (children eq)
    in case name t of
         "f_arcsin" -> headSafe (children t)
         _ ->
           let lst = factorGeneration t
           in if any isNeg lst
              then neg (fMul t (treeForm "d_-1") `fx` "sin")
              else case singlePi lst >>= (`M.lookup` trigSinTable) of
                     Just v  -> v
                     Nothing -> TreeNode (name eq) [trig0 t]

  "f_cos" ->
    let t = headSafe (children eq)
        lst = factorGeneration t
        v   = singlePi lst >>= (`M.lookup` trigCosTable)
    in case name t of
         "f_arccos" -> headSafe (children t)
         _ | any isNeg lst -> fMul t (treeForm "d_-1") `fx` "cos"
           | Just val <- v  -> val
           | otherwise      -> TreeNode (name eq) [trig0 t]

  _ -> TreeNode (name eq) (map trig0 (children eq))
