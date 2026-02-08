module Expand where

import Base
import Data.Maybe (fromMaybe)
import Data.Ratio (numerator, denominator)

------------------------------------------------------------
-- expand_nc
------------------------------------------------------------

expandNC :: TreeNode -> String -> TreeNode
expandNC expr label
  | name expr `notElem` ["f_add", label, "f_pow"] = expr
  | otherwise =
      case name expr of

        --------------------------------------------------
        -- f_pow
        --------------------------------------------------
        "f_pow" ->
          let [base, expn] = map (`expandNC` label) (children expr)
          in case frac expn of
               Just r
                 | denominator r == 1
                 , numerator r > 1 ->
                     let n = fromIntegral (numerator r)
                         factors = replicate n base
                     in expandNC (TreeNode label factors) label
               _ -> TreeNode "f_pow" [base, expn]

        --------------------------------------------------
        -- f_add (flatten)
        --------------------------------------------------
        "f_add" ->
          let cs = map (`expandNC` label) (children expr)
              out = concatMap
                      (\c -> if name c == "f_add"
                             then children c
                             else [c])
                      cs
          in TreeNode "f_add" out

        --------------------------------------------------
        -- multiplication-like label
        --------------------------------------------------
        _ | name expr == label ->
            let cs = map (`expandNC` label) (children expr)
                factors =
                  concatMap
                    (\c -> if name c == label
                           then children c
                           else [c])
                    cs
            in distribute factors label

        _ -> expr


------------------------------------------------------------
-- distribute helper (core non-commutative expansion)
------------------------------------------------------------

distribute :: [TreeNode] -> String -> TreeNode
distribute factors label =
  case break ((== "f_add") . name) factors of
    (_, []) ->
      TreeNode label factors

    (left, addNode:right) ->
      let terms =
            [ expandNC (TreeNode label (left ++ [term] ++ right)) label
            | term <- children addNode
            ]
      in TreeNode "f_add" terms


------------------------------------------------------------
-- expand2
------------------------------------------------------------

expand2 :: TreeNode -> String -> TreeNode
expand2 eq over =
  let label = case over of
                "@" -> "f_wmul"
                "." -> "f_dot"
                "*" -> "f_mul"
                _   -> "f_mul"
  in expandNC eq label


------------------------------------------------------------
-- expand (recursive post-pass)
------------------------------------------------------------

expand :: TreeNode -> String -> TreeNode
expand eq over =
  let eq' = expand2 eq over
  in TreeNode (name eq') (map (\c -> expand c over) (children eq'))
