{-# LANGUAGE FlexibleInstances #-}

module Base where

import Data.Ratio (Rational, (%), numerator, denominator)
import Data.List (sort, nub, sortOn, intersperse, isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Function (fix)

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
treeForm input
  | null input      = TreeNode "" []
  | '\n' `elem` input = foldl addLine (TreeNode "Root" []) (lines input)
  | otherwise       = TreeNode input []
  where
    addLine :: TreeNode -> String -> TreeNode
    addLine root line = fst (go [] root line 0)

    -- stack keeps (level, TreeNode)
    go :: [(Int, TreeNode)] -> TreeNode -> String -> Int -> (TreeNode, [(Int, TreeNode)])
    go stack parent line _ =
      let lvl = length (takeWhile (== ' ') line)
          nodeName = dropWhile (== ' ') line
          node = TreeNode nodeName []
          (newParent, newStack) = case dropWhile (\(l,_) -> l >= lvl) stack of
                                    [] -> (parent, stack ++ [(lvl, node)])
                                    xs -> (snd (last xs), xs ++ [(lvl, node)])
      in (attachNode newParent node, newStack)

    attachNode :: TreeNode -> TreeNode -> TreeNode
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
fSub x y = fAdd x (fMul (treeForm "d_-1") y)
fDiv x y = fMul x (fPow y (treeForm "d_-1"))
fPow a b = TreeNode "f_pow" [a,b]
fAnd x y = TreeNode "f_and" [x, y]
fOr  x y = TreeNode "f_or" [x, y]
neg x = fMul (treeForm "d_-1") x

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
  | "s_" `isPrefixOf` n = drop2 n
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

-- Returns (found, root) like Python's (True/False, value)
perfectRoot :: Integer -> Integer -> (Bool, Integer)
perfectRoot n r
    | r <= 0                 = (False, 0)
    | n < 0 && even r        = (False, 0)
    | otherwise              = go lo hi
  where
    lo = 0
    hi = max n 1
    go l h
      | l > h     = (False, 0)
      | powVal == n = (True, mid)
      | powVal < n  = go (mid + 1) h
      | otherwise   = go l (mid - 1)
      where
        mid = (l + h) `div` 2
        powVal = integerPower mid r

-- Integer exponentiation
integerPower :: Integer -> Integer -> Integer
integerPower x 0 = 1
integerPower x k = go x k 1
  where
    go _ 0 acc = acc
    go b e acc = go b (e-1) (acc * b)


------------------------------------------------------------
-- CONVERT TO BASIC (like convert_to_basic in Python)
------------------------------------------------------------
convertToBasic :: TreeNode -> TreeNode
convertToBasic t
  | not ("f_" `isPrefixOf` name t) = t
  | otherwise =
      let t' = t { children = map convertToBasic (children t) }
      in case name t of
           "f_sub"  -> TreeNode "f_add" [children t' !! 0, TreeNode "f_neg" [children t' !! 1]]
           "f_neg"  -> TreeNode "f_mul" [children t' !! 0, dR (-1)]
           "f_div"  -> TreeNode "f_mul" [children t' !! 0, TreeNode "f_pow" [children t' !! 1, dR (-1)]]
           "f_sqrt" -> TreeNode "f_pow" [children t' !! 0, TreeNode "f_pow" [dR 2, dR (-1)]]
           _        -> t'

dR :: Rational -> TreeNode
dR r
  | denominator r == 1 = d (numerator r)
  | otherwise = TreeNode ("d_" ++ show (numerator r) ++ "/" ++ show (denominator r)) []
  
 
frac :: TreeNode -> Maybe Rational
-- Base: numeric literal "d_<num>"
frac t@(TreeNode n [])
    | "d_" `isPrefixOf` n = Just $ fromIntegral (read (drop 2 n) :: Integer) % 1
    | otherwise            = Nothing

-- Addition: sum fractions of children, fail if any child is non-fractional
frac (TreeNode "f_add" xs) = foldl combine (Just 0) xs
  where
    combine acc x = do
        a <- acc
        b <- frac x
        return (a + b)

-- Multiplication: multiply fractions of children, fail if any child is non-fractional
frac (TreeNode "f_mul" xs) = foldl combine (Just 1) xs
  where
    combine acc x = do
        a <- acc
        b <- frac x
        return (a * b)

-- Power: compute fractional power if possible
frac (TreeNode "f_pow" [base, expn]) = do
    a <- frac base
    b <- frac expn
    if a == 0 && b <= 0
        then Nothing  -- 0^0 or 0^negative is invalid
        else if denominator b == 1
             then return (a ^^ numerator b)  -- integer exponent
             else do
                 let (n, d) = (numerator a, denominator a)
                 (foundC, c) <- Just (perfectRoot n (denominator b))
                 (foundD, d') <- Just (perfectRoot d (denominator b))
                 if foundC && foundD
                     then return ((c % d') ^^ numerator b)
                     else Nothing

-- Anything else: not a simple fraction
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
              fs = if n < 0
                   then replicate (fromIntegral (abs n)) (TreeNode "f_pow" [b, dR (-1)])
                   else replicate (fromIntegral n) b
          in go xs (fs ++ acc)
        _ -> go xs (TreeNode "f_pow" [b,e] : acc)
    go (x:xs) acc = go xs (x:acc)

------------------------------------------------------------
-- Numeric evaluation (fully tail-recursive)
------------------------------------------------------------

compute :: TreeNode -> Maybe Double
compute (TreeNode n children) = case children of
    [] -> case n of
        "s_e"       -> Just (exp 1)
        "s_pi"      -> Just pi
        _ | "d_" `isPrefixOf` n -> Just (read (drop 2 n))
        _ -> Nothing
    _  -> case n of
        "f_add" -> fmap sum (mapM compute children)
        "f_mul" -> fmap product (mapM compute children)
        "f_div" -> case mapM compute children of
                      Just [a,b] -> Just (a / b)
                      _          -> Nothing
        "f_pow" -> case mapM compute children of
                      Just [a,b] -> Just (a ** b)
                      _          -> Nothing
        "f_neg" -> fmap negate (compute (head children))
        "f_abs" -> fmap abs (compute (head children))
        "f_sin" -> fmap sin (compute (head children))
        "f_cos" -> fmap cos (compute (head children))
        "f_tan" -> fmap tan (compute (head children))
        "f_log" -> fmap log (compute (head children))
        _       -> Nothing

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
mulAll [] = treeForm "d_1"
mulAll xs = TreeNode "f_mul" (concatMap flatten xs)
  where
    flatten t@(TreeNode "f_mul" cs) = cs  -- flatten inner multiplications
    flatten t = [t]

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

dowhile :: TreeNode -> (TreeNode -> TreeNode) -> TreeNode
dowhile x f =
  fix (\rec v ->
    let next = f v
    in if norm next == norm v then next else rec next
  ) x
  where
    norm = sortTree . flattenTree

copyTree :: TreeNode -> TreeNode
copyTree (TreeNode n ch) = TreeNode n (map copyTree ch)

-- | Replace all occurrences of a subtree with another subtree
replace :: TreeNode -> TreeNode -> TreeNode -> TreeNode
replace t pat val
  | t == pat = val
  | otherwise =
      TreeNode
        (name t)
        (map (\c -> replace c pat val) (children t))

-- | Check whether a TreeNode contains another TreeNode as a subtree
contain :: TreeNode -> TreeNode -> Bool
contain t sub
  | t == sub = True
  | otherwise = any (`contain` sub) (children t)

subTrees :: TreeNode -> [TreeNode]
subTrees n = n : concatMap subTrees (children n)

combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations k (x:xs)
  | k < 0     = []
  | otherwise =
      map (x:) (combinations (k - 1) xs)
      ++ combinations k xs

child0 :: TreeNode -> TreeNode
child0 = head . children

child1 :: TreeNode -> TreeNode
child1 n = children n !! 1

-- Apply a function to a TreeNode
fx :: TreeNode -> String -> TreeNode
fx t fname = TreeNode ("f_" ++ fname) [t]

noneNode :: TreeNode
noneNode = TreeNode "s_none" []

s_pi, s_i, s_e :: TreeNode
s_pi = TreeNode "s_pi" []
s_i  = TreeNode "s_i"  []
s_e  = TreeNode "s_e"  []