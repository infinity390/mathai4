module Factor where

import Data.List (foldl')
import Data.Ratio
import qualified Data.Map as M
import Base
import Simplify
import Expand

_mergeSqrt :: TreeNode -> TreeNode
_mergeSqrt eq =
  let (sq, rest) = foldl step ([], []) (factorGeneration eq)

      step (eq2, lst) child =
        case frac child of
          -- numeric constant
          Just r | denominator r == 1 ->
            if r > 0
              then (fPow child (dR 2) : eq2, lst)
              else if r /= (-1)
                then (fPow (neg child) (dR 2) : eq2, treeForm "d_-1" : lst)
                else (eq2, treeForm "d_-1" : lst)

          -- sqrt(x)
          _ | name child == "f_pow"
            , frac (children child !! 1) == Just (1 % 2) ->
              (children child !! 0 : eq2, lst)

          -- everything else
          _ -> (eq2, child : lst)

  in if length sq > 1
       then
         let rest' = if null rest then [treeForm "d_1"] else rest
         in simplify $
              fMul
                (fPow
                  (mulAll sq)
                  (fPow (treeForm "d_2") (dR (-1))))
                (mulAll rest')
       else
         TreeNode (name eq) (map _mergeSqrt (children eq))

sqrtToASqrtB :: Integer -> (Integer, Integer)
sqrtToASqrtB 0 = (0, 0)
sqrtToASqrtB n =
  let (sign, m0) =
        if n < 0 then (-1, -n) else (1, n)

      go :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
      go m p a b
        | p * p > m =
            if m > 1 then (a, b * m) else (a, b)
        | m `mod` p == 0 =
            let (m', e) = count m p 0
                a' = a * p ^ (e `div` 2)
                b' = if odd e then b * p else b
                p' = if p == 2 then 3 else p + 2
            in go m' p' a' b'
        | otherwise =
            go m (if p == 2 then 3 else p + 2) a b

      count :: Integer -> Integer -> Integer -> (Integer, Integer)
      count m p e
        | m `mod` p == 0 = count (m `div` p) p (e + 1)
        | otherwise     = (m, e)

      (a, b) = go m0 2 1 1
  in (sign * a, b)


mergeSqrt :: TreeNode -> TreeNode
mergeSqrt = helper . _mergeSqrt
  where
    helper :: TreeNode -> TreeNode
    helper eq =
      case eq of

        -- sqrt(d_n)
        TreeNode "f_pow" [base, expo]
          | frac expo == Just (1 % 2)
          , take 2 (name base) == "d_" ->
              let n = read (drop 2 (name base)) :: Integer
                  (a,b) = sqrtToASqrtB n
              in fMul
                   (treeForm ("d_" ++ show a))
                   (fPow
                     (treeForm ("d_" ++ show b))
                     (fPow (treeForm "d_2") (dR (-1))))

        -- d^(-1/2)
        TreeNode "f_pow" [base, expo]
          | frac expo == Just ((-1) % 2)
          , Just r <- frac base ->
              let num = treeForm ("d_" ++ show (numerator r))
                  den = treeForm ("d_" ++ show (denominator r))
              in fDiv
                   (fPow den (fPow (treeForm "d_2") (dR (-1))))
                   (fPow num (fPow (treeForm "d_2") (dR (-1))))

        -- recurse
        _ ->
          TreeNode (name eq) (map helper (children eq))

_factorConst :: TreeNode -> TreeNode
_factorConst eq =
  let
    hcfList :: [Integer] -> Maybe Integer
    hcfList [] = Nothing
    hcfList xs =
      let sign = if product xs < 0 then -1 else 1
          hcf  = foldl gcd (abs (head xs)) (map abs (tail xs))
      in Just (sign * hcf)

    extractNum :: TreeNode -> Integer
    extractNum e =
      case [ read (drop 2 (name x))
           | x <- factorGeneration e
           , take 2 (name x) == "d_"
           ] of
        (n:_) -> n
        []    -> 1
  in
    case name eq of
      "f_add" ->
        case hcfList (map extractNum (children eq)) of
          Just n | n /= 1 ->
            let dNode = treeForm ("d_" ++ show n)
                newEq =
                  TreeNode "f_add"
                    [ fDiv child dNode | child <- children eq ]
            in fMul dNode newEq
          _ ->
            TreeNode "f_add" (map _factorConst (children eq))

      _ ->
        TreeNode (name eq) (map _factorConst (children eq))

rationalizeSqrt :: TreeNode -> TreeNode
rationalizeSqrt eq =
  let
    eq' =
      case eq of
        TreeNode "f_pow" [base, expo]
          | frac expo == Just ((-1) % 2) ->
              fDiv
                (fPow base (fPow (treeForm "d_2") (dR (-1))))
                base
        _ -> eq

    term :: TreeNode -> Maybe TreeNode
    term t =
      case t of
        TreeNode "f_add" cs ->
          let hasSqrt c =
                any (\x -> name x == "f_pow"
                        && frac (children x !! 1) == Just (1 % 2))
                    (factorGeneration c)
          in Just $
               summation
                 [ if hasSqrt c then simplify (neg c) else c
                 | c <- cs
                 ]
        _ -> Nothing

    (n, d) = numDen eq'
    n' = simplify n
    d' = simplify d
  in
    if d' /= treeForm "d_1"
      then
        case term d' of
          Just t | t /= treeForm "d_1" ->
            let n2 = simplify (expand (simplify (fMul n' t)) "*")
                d2 = simplify (expand (simplify (fMul d' t)) "*")
            in _mergeSqrt (simplify (fDiv n2 d2))
          _ ->
            TreeNode (name eq') (map rationalizeSqrt (children eq'))
      else
        TreeNode (name eq') (map rationalizeSqrt (children eq'))
factorConst :: TreeNode -> TreeNode
factorConst = simplify . _factorConst
