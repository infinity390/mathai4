module Limit
  ( substituteVal
  , subslimit
  , lhospital
  , lhospital2
  , limit0
  , limit1
  , limit2
  , limit3
  , limit
  ) where

import Base
import Simplify
import Expand
import Diff
import Trig
import Fraction
import Data.Maybe (fromMaybe, fromJust)

--------------------------------------------------
-- substitution
--------------------------------------------------

substituteVal :: TreeNode -> Int -> TreeNode -> TreeNode
substituteVal eq val var =
  replace eq var (treeForm ("d_" ++ show val))

--------------------------------------------------
-- substitute limit variable with 0
--------------------------------------------------

subslimit :: TreeNode -> TreeNode -> Maybe TreeNode
subslimit equation var =
  let equation2 = trig0 (replace equation var (treeForm "d_0"))
  in Just (simplify (expand (simplify equation2) "*"))

--------------------------------------------------
-- helper for l'Hôpital
--------------------------------------------------

check :: TreeNode -> TreeNode -> TreeNode -> Maybe TreeNode
check num den var =
  let step e =
        dowhile
          (replace e var (treeForm "d_0"))
          (\x -> trig0 (simplify x))

      n = step num
      d = step den
  in case (n, d) of
       _ | n == treeForm "d_0" && d == treeForm "d_0" -> Nothing
       _ | d /= treeForm "d_0" ->
             Just (simplify (TreeNode "f_div" [n, d]))
       _ -> Nothing

--------------------------------------------------
-- l'Hôpital rule
--------------------------------------------------

lhospital :: TreeNode -> TreeNode -> Int -> TreeNode -> TreeNode
lhospital num den steps var =
  case check num den var of
    Just out -> out
    Nothing ->
      go steps num den
  where
    go 0 n d =
      simplify (fraction (simplify (TreeNode "f_div" [n, d])))

    go k n d =
      let n' = simplify (diff n var)
          d' = simplify (diff d var)
      in case check n' d' var of
           Nothing -> go (k - 1) n' d'
           Just out -> out

--------------------------------------------------
-- l'Hôpital wrapper
--------------------------------------------------

lhospital2 :: TreeNode -> TreeNode -> TreeNode
lhospital2 eq var =
  let eq' = simplify eq
  in if not (contain eq' var)
        then eq'
        else
          let (num, den) = numDen eq'
          in lhospital num den 10 var

--------------------------------------------------
-- limit rules
--------------------------------------------------

limit0 :: TreeNode -> TreeNode
limit0 eq
  | name eq `elem` ["f_limit", "f_limitpinf", "f_limitninf"]
  , let expr = head (children eq)
  , let wrt  = children eq !! 1
  , contain expr wrt =
      let
        lst    = factorGeneration expr
        consts = filter (not . (`contain` wrt)) lst
      in
        if not (null consts)
        then
          let
            expr'  = mulAll (filter (`contain` wrt) lst)
            const  = simplify (mulAll consts)
            lim    = TreeNode (name eq) [expr', wrt]
          in
            TreeNode "f_mul" [lim, const]
        else
          TreeNode (name eq) (map limit0 (children eq))

  | otherwise =
      TreeNode (name eq) (map limit0 (children eq))

--------------------------------------------------

limit2 :: TreeNode -> TreeNode
limit2 eq
  | name eq `elem` ["f_limit","f_limitpinf","f_limitninf"]
  , let expr = head (children eq)
  , name expr == "f_add" =
      summation
        [ TreeNode (name eq) [c, children eq !! 1]
        | c <- children expr
        ]
  | otherwise =
      TreeNode (name eq) (map limit2 (children eq))

--------------------------------------------------

limit1 :: TreeNode -> TreeNode
limit1 eq
  | name eq == "f_limit" =
      let (a, ok) = limit (children eq !! 0) (children eq !! 1)
      in if ok then a else eq
  | otherwise =
      TreeNode (name eq) (map limit1 (children eq))

--------------------------------------------------
-- infinity handling
--------------------------------------------------

fxinf :: TreeNode -> Maybe TreeNode
fxinf eq
  | name eq == "f_add"
  , treeForm "s_inf" `elem` children eq
  , neg (treeForm "s_inf") `elem` children eq = Nothing

  | name eq == "f_add"
  , treeForm "s_inf" `elem` children eq = Just (treeForm "s_inf")

  | name eq == "f_add"
  , neg (treeForm "s_inf") `elem` children eq = Just (neg (treeForm "s_inf"))

  | otherwise =
      let ch = map fxinf (children eq)
      in if Nothing `elem` ch
           then Nothing
           else Just (TreeNode (name eq) (map fromJust ch))

--------------------------------------------------

limit3 :: TreeNode -> TreeNode
limit3 eq
  | name eq == "f_limitpinf" =
      let expr = replace (children eq !! 0)
                         (children eq !! 1)
                         (treeForm "s_inf")
          expr' = dowhile expr (fromMaybe expr . fxinf)
      in if not (contain expr' (treeForm "s_inf"))
            then simplify expr'
            else eq
  | otherwise =
      TreeNode (name eq) (map limit3 (children eq))

--------------------------------------------------
-- main limit
--------------------------------------------------

limit :: TreeNode -> TreeNode -> (TreeNode, Bool)
limit equation var =
  let eq2 =
        dowhile
          (replace equation var (treeForm "d_0"))
          (\x -> trig0 (simplify x))
  in if not (contain equation var)
        then (eq2, True)
        else
          let eq3 = lhospital2 equation var
              eq4 = simplify (expand (simplify eq3) "*")
          in if not (contain eq4 var)
                then (eq4, True)
                else (eq4, False)
