{-# LANGUAGE TupleSections #-}

module Trig (trig0, trig1) where

import Base
import Simplify (simplify)
import Parser
import qualified Data.Map.Strict as M
import Data.Ratio (numerator, denominator)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)
import Expand

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

trig4 :: TreeNode -> TreeNode
trig4 eq =
  case eq of

    -- sin(arccos x) = sqrt(1 - x^2)
    TreeNode "f_sin" [TreeNode "f_arccos" [x]] ->
      fPow (fAdd (dR 1) (neg (fPow (trig4 x) (dR 2))))
           (fPow (dR 2) (dR (-1)))

    -- cos(arcsin x) = sqrt(1 - x^2)
    TreeNode "f_cos" [TreeNode "f_arcsin" [x]] ->
      fPow (fAdd (dR 1) (neg (fPow (trig4 x) (dR 2))))
           (fPow (dR 2) (dR (-1)))

    -- sin(arctan x) = x / sqrt(1 + x^2)
    TreeNode "f_sin" [TreeNode "f_arctan" [x]] ->
      fDiv (trig4 x)
           (fPow (fAdd (dR 1) (fPow (trig4 x) (dR 2)))
                 (fPow (dR 2) (dR (-1))))

    -- cos(arctan x) = 1 / sqrt(1 + x^2)
    TreeNode "f_cos" [TreeNode "f_arctan" [x]] ->
      fDiv (dR 1)
           (fPow (fAdd (dR 1) (fPow (trig4 x) (dR 2)))
                 (fPow (dR 2) (dR (-1))))

    -- arcsin(sin x) → x   (no range checking)
    TreeNode "f_arcsin" [TreeNode "f_sin" [x]] ->
      trig4 x

    -- arccos(cos x) → x
    TreeNode "f_arccos" [TreeNode "f_cos" [x]] ->
      trig4 x

    -- arctan(tan x) → x
    TreeNode "f_arctan" [TreeNode "f_tan" [x]] ->
      trig4 x

    -- recursive fallback
    TreeNode n cs ->
      TreeNode n (map trig4 cs)

productToSum :: TreeNode -> TreeNode
productToSum expr =
  let factors = factorGeneration expr
      (trigs, other) =
        foldr
          (\x (ts, os) ->
             if name x == "f_sin" || name x == "f_cos"
               then (x:ts, os)
               else (ts, x:os))
          ([], [])
          factors
  in case length trigs of

       -- ≤ 1 trig factor
       n | n <= 1 ->
           dowhile expr cog

       -- exactly 2 trig factors
       2 ->
         let [a0, b0] = trigs
             (a, b) = if name a0 < name b0 then (b0, a0) else (a0, b0)
             aArg = children a !! 0
             bArg = children b !! 0
             half = dR 2

             out =
               case (name a, name b) of
                 ("f_sin", "f_sin") ->
                   TreeNode "f_div"
                     [ TreeNode "f_add"
                         [ TreeNode "f_cos" [TreeNode "f_sub" [aArg, bArg]]
                         , TreeNode "f_neg"
                             [TreeNode "f_cos" [TreeNode "f_add" [aArg, bArg]]]
                         ]
                     , half
                     ]

                 ("f_cos", "f_cos") ->
                   TreeNode "f_div"
                     [ TreeNode "f_add"
                         [ TreeNode "f_cos" [TreeNode "f_sub" [aArg, bArg]]
                         , TreeNode "f_cos" [TreeNode "f_add" [aArg, bArg]]
                         ]
                     , half
                     ]

                 ("f_sin", "f_cos") ->
                   TreeNode "f_div"
                     [ TreeNode "f_add"
                         [ TreeNode "f_sin" [TreeNode "f_add" [aArg, bArg]]
                         , TreeNode "f_sin" [TreeNode "f_sub" [aArg, bArg]]
                         ]
                     , half
                     ]

                 _ -> expr
         in mulAll (out : other)

       -- more than 2 trig factors
       _ ->
         let (rest, trigs') =
               if odd (length trigs)
                 then (head trigs, tail trigs)
                 else (dR 1, trigs)

             paired =
               [ productToSum (mulAll [trigs' !! i, trigs' !! (i+1)])
               | i <- [0,2 .. length trigs' - 2]
               ]

             newExpr = mulAll (paired ++ [rest] ++ other)
         in dowhile newExpr cog
trig1 :: TreeNode -> TreeNode
trig1 eq =
  productToSum (nonegPow eq)
nonegPow :: TreeNode -> TreeNode
nonegPow eq
  | name eq == "f_pow"
  , [_base, expn] <- children eq
  , Just r <- frac expn
  , r < 0 =
      TreeNode "f_pow"
        [ TreeNode "f_pow"
            [ children eq !! 0
            , simplify (TreeNode "f_neg" [expn])
            ]
        , dR (-1)
        ]
  | otherwise =
      TreeNode (name eq) (map nonegPow (children eq))

cog :: TreeNode -> TreeNode
cog expr =
  let expr1 = TreeNode (name expr) (map productToSum (children expr))
      expr2 = trig0 (simplify expr1)
      expr3 = expand (simplify expr2) "*"
  in expr3
