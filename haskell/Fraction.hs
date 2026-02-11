module Fraction (fraction) where

import Base
import Simplify (simplify)
import Expand (expand)
import Data.List (isPrefixOf)

fraction :: TreeNode -> TreeNode
fraction expr =
  let expr' = simplify expr
  in case children expr' of
       [] -> expr'
       _  ->
         let children' = map fraction (children expr')
         in case name expr' of
              "f_add" -> fractionAdd expr' children'
              _       -> TreeNode (name expr') children'


--------------------------------------------------
-- ADDITION CASE
--------------------------------------------------

fractionAdd :: TreeNode -> [TreeNode] -> TreeNode
fractionAdd _ children =
  let terms = map splitTerm children
  in if not (any (not . null . snd) terms)
        then TreeNode "f_add" children
        else buildFraction terms


--------------------------------------------------
-- SPLIT TERM INTO (NUMERATOR FACTORS, DENOMINATOR FACTORS)
--------------------------------------------------

splitTerm :: TreeNode -> ([TreeNode], [TreeNode])
splitTerm c =
  case name c of
    "f_mul" ->
      foldr splitFactor ([], []) (children c)

    "f_pow"
      | isNegativePower c ->
          let (base, n) = powInfo c
          in ([], [powDen base n])

    _ -> ([c], [])


splitFactor :: TreeNode -> ([TreeNode], [TreeNode]) -> ([TreeNode], [TreeNode])
splitFactor f (num, den)
  | isNegativePower f =
      let (base, n) = powInfo f
      in (num, powDen base n : den)
  | otherwise = (f : num, den)


--------------------------------------------------
-- NEGATIVE POWER HELPERS
--------------------------------------------------

isNegativePower :: TreeNode -> Bool
isNegativePower (TreeNode "f_pow" [_, e]) =
  case name e of
    s | "d_" `isPrefixOf` s -> read (drop 2 s) < 0
    _ -> False
isNegativePower _ = False


powInfo :: TreeNode -> (TreeNode, Int)
powInfo (TreeNode "f_pow" [b, e]) =
  (b, read (drop 2 (name e)))
powInfo _ = error "powInfo: not a power node"


powDen :: TreeNode -> Int -> TreeNode
powDen base n
  | n == -1   = base
  | otherwise = TreeNode "f_pow" [base, treeForm ("d_" ++ show (-n))]


--------------------------------------------------
-- BUILD FINAL FRACTION
--------------------------------------------------

buildFraction :: [([TreeNode], [TreeNode])] -> TreeNode
buildFraction terms =
  let numTerms =
        [ buildMul (num ++ concat [denj | (j, (_, denj)) <- zip [0..] terms, i /= j])
        | (i, (num, _)) <- zip [0..] terms
        ]

      numerator = TreeNode "f_add" numTerms

      denAll = concatMap snd terms
      denomMul =
        case denAll of
          [d] -> d
          _   -> TreeNode "f_mul" denAll

      denom = TreeNode "f_pow" [denomMul, treeForm "d_-1"]

  in simplify $
       TreeNode "f_mul"
         [ simplify (expand numerator "*")
         , denom
         ]


buildMul :: [TreeNode] -> TreeNode
buildMul []  = treeForm "d_1"
buildMul [x] = x
buildMul xs  = TreeNode "f_mul" xs
