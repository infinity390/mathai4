{-# LANGUAGE TupleSections #-}

module Structure
  ( structure
  , transformFormula
  ) where

import Base
import Simplify (simplify)

import Data.List (permutations, nub)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M

import Control.Applicative ((<|>))

import qualified Data.Map.Strict as M

extractVars :: TreeNode -> TreeNode -> Maybe (M.Map String TreeNode)
extractVars eq pat = go eq pat M.empty
  where
    go :: TreeNode -> TreeNode -> M.Map String TreeNode
       -> Maybe (M.Map String TreeNode)

    -- variable pattern v_-k
    go e p vars
      | take 2 (name p) == "v_"
      , let k = read (drop 2 (name p)) :: Int
      , k < 0
      = case M.lookup (name p) vars of
          Nothing -> Just (M.insert (name p) e vars)
          Just v  -> if v == e then Just vars else Nothing

    -- mismatch
    go e p _
      | name e /= name p
      = Nothing
      | length (children e) /= length (children p)
      = Nothing

    -- recurse
    go e p vars =
      foldl
        (\acc (x,y) -> acc >>= \m -> go x y m)
        (Just vars)
        (zip (children e) (children p))


------------------------------------------------------------
-- structure
------------------------------------------------------------

structure
  :: TreeNode
  -> TreeNode
  -> Maybe TreeNode
  -> Bool
  -> Maybe TreeNode
  -> Maybe (Either (M.Map String TreeNode) TreeNode)
structure equation formula formulaOut onlyConst wrt =
  let eq'  = conv equation
      fo'  = fmap conv formulaOut
  in tryForms eq' fo'
  where
    --------------------------------------------------------
    -- variable matching
    --------------------------------------------------------
    match :: TreeNode -> TreeNode -> M.Map String TreeNode -> Maybe (M.Map String TreeNode)
    match eq fm env
      | take 2 (name fm) == "v_"
      , let i = read (drop 2 (name fm))
      , i < 0 =
          case M.lookup (name fm) env of
            Just v  -> if v == eq then Just env else Nothing
            Nothing -> Just (M.insert (name fm) eq env)

      | name eq /= name fm = Nothing
      | length (children eq) /= length (children fm) = Nothing
      | otherwise =
          foldl
            (\acc (a,b) -> acc >>= match a b)
            (Just env)
            (zip (children eq) (children fm))

    --------------------------------------------------------
    -- generate permutations
    --------------------------------------------------------
    gen :: TreeNode -> [TreeNode]
    gen node
      | null (children node) = [node]
      | otherwise =
          let childGroups =
                if name node `elem` ["f_addw","f_mulw"]
                   then permutations (children node)
                   else [children node]
          in do
              grp <- childGroups
              subs <- mapM gen grp
              pure (TreeNode (name node) subs)

    --------------------------------------------------------
    -- conversion helpers
    --------------------------------------------------------
    conv :: TreeNode -> TreeNode
    conv (TreeNode n ch) =
      let n' = case n of
                 "f_add" -> "f_addw"
                 "f_mul" -> "f_mulw"
                 x       -> x
      in TreeNode n' (map conv ch)

    convRev :: TreeNode -> TreeNode
    convRev (TreeNode n ch) =
      let n' = case n of
                 "f_addw" -> "f_add"
                 "f_mulw" -> "f_mul"
                 x        -> x
      in TreeNode n' (map convRev ch)

    --------------------------------------------------------
    -- main loop
    --------------------------------------------------------
    tryForms eq fo =
      let forms = gen (conv formula)
      in foldr (<|>) Nothing $
           map (tryOne eq fo) forms

    tryOne eq fo fm =
      case match eq fm M.empty of
        Nothing -> Nothing
        Just env ->
          if onlyConst &&
             maybe False
               (\v -> any (`contain` treeForm (show wrt)) (M.elems env))
               wrt
          then Nothing
          else case fo of
                 Nothing -> Just (Left env)
                 Just out ->
                   let out' =
                         foldl
                           (\acc (k,v) -> replace acc (treeForm k) v)
                           out
                           (M.toList env)
                   in Just (Right (convRev out'))

------------------------------------------------------------
-- transformFormula
------------------------------------------------------------

transformFormula :: TreeNode -> TreeNode -> [TreeNode]
transformFormula equation wrt =
  let
    eq0 = simplify equation

    matches :: [M.Map String TreeNode]
    matches =
      [ vars
      | sub <- subTrees eq0
      , contain sub wrt
      , Just vars <- [extractVars sub wrt]
      ]

    combos :: [[(String, TreeNode)]]
    combos = map M.toList matches

    applyCombo :: [(String, TreeNode)] -> TreeNode
    applyCombo =
      simplify . foldl
        (\acc (k,v) -> replace acc (treeForm k) v)
        eq0

  in
    nub (map applyCombo combos)
