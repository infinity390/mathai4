{-# LANGUAGE FlexibleInstances #-}

module Base where

import Data.Ratio (Rational, (%), numerator, denominator)
import Data.List (sort, nub, sortOn, intersperse, isPrefixOf)
import Data.Maybe (mapMaybe)

------------------------------------------------------------
-- TreeNode definition
------------------------------------------------------------

data TreeNode = TreeNode
  { name     :: String
  , children :: [TreeNode]
  } deriving (Eq)

instance Show TreeNode where
  show = stringEquation

------------------------------------------------------------
-- Tree construction from indented string (TCO-friendly)
------------------------------------------------------------

treeForm :: String -> TreeNode
treeForm input = foldl addLine (TreeNode "Root" []) (lines input)
  where
    addLine root line = fst (go [] root line 0)
    go stack parent line level =
      let lvl = length (takeWhile (== ' ') line)
          nodeName = dropWhile (== ' ') line
          node = TreeNode nodeName []
          (newParent, newStack) = if lvl <= length stack
            then let s = take lvl stack in (last s, s ++ [node])
            else (parent, stack ++ [node])
      in (attachNode newParent node, newStack)
    attachNode (TreeNode n cs) c = TreeNode n (cs ++ [c])

------------------------------------------------------------
-- Tree → string
------------------------------------------------------------

strForm :: TreeNode -> String
strForm t = go 0 t
  where
    go depth (TreeNode n ch) = replicate depth ' ' ++ n ++ concatMap (\c -> "\n" ++ go (depth + 1) c) ch

------------------------------------------------------------
-- Operators (tree construction)
------------------------------------------------------------

fAdd, fMul, fSub, fDiv, fPow, fAnd, fOr :: TreeNode -> TreeNode -> TreeNode
fAdd x y = TreeNode "f_add" [x, y]
fMul x y = TreeNode "f_mul" [x, y]
fSub x y = fAdd x (fMul (d (-1)) y)
fDiv x y = fMul x (fPow y (d (-1)))
fPow x y = TreeNode "f_pow" [x, y]
fAnd x y = TreeNode "f_and" [x, y]
fOr  x y = TreeNode "f_or" [x, y]
neg x = fMul (d (-1)) x

------------------------------------------------------------
-- Normalize, sort, and tree equality
------------------------------------------------------------

normalize :: TreeNode -> TreeNode
normalize = sortTree

eqNorm :: TreeNode -> TreeNode -> Bool
eqNorm a b = normalize a == normalize b

eqTree :: TreeNode -> TreeNode -> Bool
eqTree (TreeNode n1 c1) (TreeNode n2 c2) =
  n1 == n2 && length c1 == length c2 && all (uncurry eqTree) (zip c1 c2)

sortTree :: TreeNode -> TreeNode
sortTree (TreeNode n ch) =
  let ch' = map sortTree ch
  in case n of
       "f_add" -> TreeNode n (sortOn strForm ch')
       "f_mul" -> TreeNode n (sortOn strForm ch')
       "f_or"  -> TreeNode n (sortOn strForm ch')
       "f_and" -> TreeNode n (sortOn strForm ch')
       "f_dot" -> TreeNode n (sortOn strForm ch')
       _       -> TreeNode n ch'

------------------------------------------------------------
-- String equation printer
------------------------------------------------------------

drop2 :: String -> String
drop2 = drop 2

sign :: [(String, String)]
sign =
  [ ("f_not","~"), ("f_and","&"), ("f_or","|")
  , ("f_add","+"), ("f_mul","*"), ("f_div","/")
  , ("f_pow","^"), ("f_sub","-"), ("f_eq","=")
  ]

lookupSign :: String -> String
lookupSign n = maybe "," id (lookup n sign)

funcLike :: [String]
funcLike = [ [c] | c <- ['A'..'Z'] ] ++ ["limit","integrate","sum","dif","pdif","exist","forall","covariance"]

stringEquation :: TreeNode -> String
stringEquation = stripParens . clean . stringEquationHelper . sortTree

stringEquationHelper :: TreeNode -> String
stringEquationHelper (TreeNode n []) 
  | "g_" `isPrefixOf` n = "\"" ++ drop2 n ++ "\""
  | "d_" `isPrefixOf` n = drop2 n
  | "v_" `isPrefixOf` n = drop2 n
  | otherwise           = n
stringEquationHelper (TreeNode "f_neg" [c]) = "-" ++ stringEquationHelper c
stringEquationHelper (TreeNode "f_not" [c]) = "~" ++ stringEquationHelper c
stringEquationHelper (TreeNode "f_list" cs) = "[" ++ comma cs ++ "]"
  where comma = concat . intersperse "," . map stringEquationHelper
stringEquationHelper (TreeNode "f_index" (x:cs)) = stringEquationHelper x ++ "[" ++ comma cs ++ "]"
  where comma = concat . intersperse "," . map stringEquationHelper
stringEquationHelper (TreeNode n cs) =
  let prefix
        | length cs == 1 || drop2 n `elem` funcLike = drop2 n
        | otherwise = ""
      joined = concat (intersperse (lookupSign n) (map stringEquationHelper cs))
  in prefix ++ "(" ++ joined ++ ")"

clean :: String -> String
clean = replaceStr "+-" "-"

replaceStr :: String -> String -> String -> String
replaceStr old new = go
  where
    go [] = []
    go s@(x:xs)
      | old `isPrefixOf` s = new ++ go (drop (length old) s)
      | otherwise         = x : go xs

stripParens :: String -> String
stripParens ('(' : xs)
  | not (null xs) && last xs == ')' = init xs
stripParens s = s

------------------------------------------------------------
-- Constructors
------------------------------------------------------------

d :: Integer -> TreeNode
d n = TreeNode ("d_" ++ show n) []

v :: String -> TreeNode
v x = TreeNode ("v_" ++ x) []

------------------------------------------------------------
-- Rational ↔ Tree
------------------------------------------------------------

fracToTree :: Rational -> TreeNode
fracToTree f
  | numerator f == 0 = d 0
  | numerator f == 1 && denominator f == 1 = d 1
  | numerator f == 1 = fPow (d (denominator f)) (d (-1))
  | denominator f == 1 = d (numerator f)
  | otherwise = fDiv (d (numerator f)) (d (denominator f))

perfectRoot :: Integer -> Integer -> (Bool, Maybe Integer)
perfectRoot n r
  | r <= 0 = (False, Nothing)
  | n < 0 && even r = (False, Nothing)
  | otherwise = go 0 (max 1 n)
  where
    go lo hi
      | lo > hi = (False, Nothing)
      | otherwise =
          let mid = (lo + hi) `div` 2
              p = mid ^ r
          in case compare p n of
               EQ -> (True, Just mid)
               LT -> go (mid + 1) hi
               GT -> go lo (mid - 1)

frac :: TreeNode -> Maybe Rational
frac (TreeNode n [])
  | "d_" `isPrefixOf` n = Just (read (drop 2 n) % 1)
frac (TreeNode "f_add" cs) = fmap sum (mapM frac cs)
frac (TreeNode "f_mul" cs) = fmap product (mapM frac cs)
frac (TreeNode "f_pow" [a,b]) = do
  base <- frac a
  expo <- frac b
  if base == 0 && expo <= 0 then Nothing
  else if denominator expo == 1 then Just (base ^ numerator expo)
  else
    let (fc, mc) = perfectRoot (numerator base) (denominator expo)
        (fd, md) = perfectRoot (denominator base) (denominator expo)
    in case (fc, mc, fd, md) of
         (True, Just c, True, Just d) -> Just ((c % d) ^ numerator expo)
         _ -> Nothing
frac _ = Nothing

------------------------------------------------------------
-- Factor generation (tail-recursive)
------------------------------------------------------------

factorGeneration :: TreeNode -> [TreeNode]
factorGeneration eq = go [eq] []
  where
    go [] acc = reverse acc
    go (TreeNode "f_mul" cs:xs) acc = go (cs ++ xs) acc
    go (TreeNode "f_pow" [b,e]:xs) acc =
      case frac e of
        Just r | denominator r == 1 ->
          let n = numerator r
              fs = if n < 0 then map (fPow b . d) [-1] else replicate (fromIntegral n) b
          in go xs (fs ++ acc)
        _ -> go xs (TreeNode "f_pow" [b,e] : acc)
    go (x:xs) acc = go xs (x:acc)

------------------------------------------------------------
-- Numeric evaluation (fully tail-recursive)
------------------------------------------------------------

compute :: TreeNode -> Maybe Double
compute root = go [(root, Nothing)] []
  where
    go [] [res] = Just res
    go [] _ = Nothing
    go ((TreeNode n [], _):stack) acc =
      let val = case n of
                  "s_e" -> Just (exp 1)
                  "s_pi" -> Just pi
                  _ | "d_" `isPrefixOf` n -> Just (read (drop 2 n))
                  _ -> Nothing
      in case val of
           Just v  -> go stack (v:acc)
           Nothing -> Nothing
    go ((TreeNode n cs, parent):stack) acc =
      go (map (\c -> (c, Just n)) (reverse cs) ++ stack) acc >>= \_ ->
        let vs = take (length cs) acc
            rest = drop (length cs) acc
            res = case (n, vs) of
                    ("f_add", xs) -> sum xs
                    ("f_mul", xs) -> product xs
                    ("f_div", [a,b]) -> a / b
                    ("f_pow", [a,b]) -> a ** b
                    ("f_neg", [x]) -> -x
                    ("f_abs", [x]) -> abs x
                    ("f_sin", [x]) -> sin x
                    ("f_cos", [x]) -> cos x
                    ("f_tan", [x]) -> tan x
                    ("f_log", [x]) -> log x
                    _ -> error "Invalid tree for compute"
        in go stack (res:rest)

------------------------------------------------------------
-- Numerator / Denominator split
------------------------------------------------------------

numDen :: TreeNode -> (TreeNode, TreeNode)
numDen eq = foldl step (d 1, d 1) (factorGeneration eq)
  where
    step (num, den) t@(TreeNode "f_pow" [b,e]) =
      case frac e of
        Just r | r < 0 ->
          let p = fracToTree (-r)
          in if p == d 1 then (num, fMul den b)
             else (num, fMul den (fPow b p))
        _ -> (fMul num t, den)
    step (num, den) t = (fMul num t, den)

------------------------------------------------------------
-- Boolean helpers
------------------------------------------------------------

orAll :: [TreeNode] -> TreeNode
orAll xs
  | any ((=="s_true") . name) xs = TreeNode "s_true" []
  | otherwise = case filter ((/="s_false") . name) xs of
                  [] -> TreeNode "s_false" []
                  [y] -> y
                  ys -> TreeNode "f_or" ys

andAll :: [TreeNode] -> TreeNode
andAll xs
  | any ((=="s_false") . name) xs = TreeNode "s_false" []
  | otherwise = case filter ((/="s_true") . name) xs of
                  [] -> TreeNode "s_true" []
                  [y] -> y
                  ys -> TreeNode "f_and" ys

------------------------------------------------------------
-- Sum / Product helpers
------------------------------------------------------------

summation :: [TreeNode] -> TreeNode
summation [] = d 0
summation xs = TreeNode "f_add" xs

mulAll :: [TreeNode] -> TreeNode
mulAll [] = d 1
mulAll xs = TreeNode "f_mul" xs

------------------------------------------------------------
-- Variable list (tail-recursive)
------------------------------------------------------------

vlist :: TreeNode -> [String]
vlist t = sort . nub $ go [t] []
  where
    go [] acc = acc
    go (TreeNode n cs : xs) acc
      | "v_" `isPrefixOf` n = go (cs ++ xs) (drop2 n : acc)
      | otherwise = go (cs ++ xs) acc

------------------------------------------------------------
-- Flatten tree (tail-recursive)
------------------------------------------------------------

flattenTree :: TreeNode -> TreeNode
flattenTree t = go t
  where
    go (TreeNode n cs)
      | n `elem` ["f_add","f_mul","f_and","f_or","f_wmul"] =
          TreeNode n (concatMap (\c -> let fc = go c in if name fc == n then children fc else [fc]) cs)
      | otherwise = TreeNode n (map go cs)
