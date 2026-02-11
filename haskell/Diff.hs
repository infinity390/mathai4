{-# LANGUAGE FlexibleInstances #-}

module Diff (diff, diff2) where

import Base
import Data.Char (isUpper)

diff :: TreeNode -> TreeNode -> TreeNode
diff t wrt = diff2 (TreeNode "f_pdif" [t, wrt])

diff2 :: TreeNode -> TreeNode
diff2 t = dowhile t diffStep

isVar :: TreeNode -> TreeNode -> Bool
isVar vNode node
  | node == vNode = True
  | otherwise     = any (isVar vNode) (children node)

isConst :: TreeNode -> TreeNode -> Bool
isConst vNode node = not (isVar vNode node)

diffStep :: TreeNode -> TreeNode
diffStep t@(TreeNode "f_pdif" (x:v:xs))
  | isConst v x = dR 0
  | x == v      = dR 1
  | TreeNode "f_add" children <- x =
      TreeNode "f_add" [TreeNode "f_pdif" [c, v] | c <- children]
  | TreeNode "f_mul" children <- x =
      TreeNode "f_add"
        [ TreeNode "f_mul"
            [ if i == j then TreeNode "f_pdif" [c, v] else c
            | (j,c) <- zip [0..] children
            ]
        | i <- [0..length children - 1]
        ]
  | TreeNode "f_sin" [arg] <- x =
      TreeNode "f_mul"
        [ TreeNode "f_cos" [arg]
        , TreeNode "f_pdif" [arg, v]
        ]
  | TreeNode "f_cos" [arg] <- x =
      TreeNode "f_mul"
        [ dR (-1)
        , TreeNode "f_mul"
            [ TreeNode "f_sin" [arg]
            , TreeNode "f_pdif" [arg, v]
            ]
        ]
  | TreeNode "f_pow" [base, expo] <- x =
      let db = TreeNode "f_pdif" [base, v]
          de = TreeNode "f_pdif" [expo, v]
          lnBase = TreeNode "f_log" [base]
          term1 = TreeNode "f_mul" [de, lnBase]                  -- g'(x) * log(f)
          term2 = TreeNode "f_mul" [expo, TreeNode "f_mul" [db, TreeNode "f_pow" [base, dR (-1)]]] -- g * f'/f
          sumTerms = TreeNode "f_add" [term1, term2]
      in TreeNode "f_mul" [TreeNode "f_pow" [base, expo], sumTerms]
  | TreeNode f _ <- x, length f >= 3, isUpper (f !! 2) =
      TreeNode "f_pdif" [x, v]
  | TreeNode "f_pdif" _ <- x =
      TreeNode "f_pdif" [diffStep x, v]
  | otherwise = t

diffStep (TreeNode f xs) =
  TreeNode f (map diffStep xs)

diffStep t = t

