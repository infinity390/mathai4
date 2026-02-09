{-# LANGUAGE FlexibleInstances #-}

module Simplify where

import Data.Ratio
import Data.Maybe (fromMaybe, isJust, fromJust)
import Base 
import Data.Foldable (foldl')
import Data.List (isPrefixOf, partition)

import Debug.Trace (traceShow, trace)

clearDiv :: TreeNode -> Bool -> (TreeNode, Bool)
clearDiv eq denom =
  let lst = factorGeneration eq
      -- check if zero is present
      hasZero = dR 0 `elem` lst

      -- negative factors (excluding variables)
      negs = [ x | x <- lst
                 , not ("v_" `isPrefixOf` name x)
                 , maybe False (<0) (frac x)
                 ]
      -- sign flips if odd number of negatives
      sign = if odd (length negs) then False else denom

      -- split numeric and non-numeric factors
      (eq2, eq3) = partition (\x -> frac x /= Nothing) lst

      -- remove powers with exponent -1
      lst4 = [ x | x <- lst
                 , name x /= "f_pow" || frac (children x !! 1) /= Just (-1)
                 ]

      -- keep only symbolic factors
      lst2 = [ x | x <- lst4, frac x == Nothing ]
  in if hasZero
        then (dR 0, True)
     else if denom
        then if null eq3
                then (mulAll eq2, True)
                else (mulAll eq3, sign)
     else if null lst2
        then (mulAll lst4, sign)
        else (mulAll lst2, sign)

multiplyNode :: TreeNode -> TreeNode
multiplyNode t@(TreeNode name children)
  | name == "f_mul" =
      let children' = map multiplyNode children  -- recurse first

          -- separate numeric constants from symbolic terms
          (nums, syms) = partition (\c -> frac c /= Nothing) children'
          coef = product [r | Just r <- map frac nums]  -- multiply numeric constants

      in if coef == 0
         then dR 0  -- 0 * anything = 0
         else
           let -- convert symbolic children to (base, exponent)
               basePowers = foldl insertPower [] syms

               insertPower :: [(TreeNode, TreeNode)] -> TreeNode -> [(TreeNode, TreeNode)]
               insertPower acc node =
                 let (base, pow) = case node of
                                     TreeNode "f_pow" [b,e] -> (b,e)
                                     _ -> (node, dR 1)
                 in case lookup base acc of
                      Just oldPow -> map (\(b',p) -> if b' == base then (b', TreeNode "f_add" [p,pow]) else (b',p)) acc
                      Nothing     -> (base,pow):acc

               -- rebuild multiplication, skip powers = 0
               rebuilt = [ if p == dR 1 then b else TreeNode "f_pow" [b,p]
                         | (b,p) <- basePowers, p /= dR 0 ]

               -- add coefficient if not 1
               finalChildren = if coef /= 1 then rebuilt ++ [fracToTree coef] else rebuilt
           in case finalChildren of
                []  -> dR 1
                [x] -> x
                xs  -> TreeNode "f_mul" xs

  | name == "f_div" =
      case children of
        [num, denom] ->
          multiplyNode $ TreeNode "f_mul" [multiplyNode num, TreeNode "f_pow" [multiplyNode denom, dR (-1)]]
        _ -> TreeNode name (map multiplyNode children)

  | otherwise = TreeNode name (map multiplyNode children)



additionNode :: TreeNode -> TreeNode
additionNode eq
  | eq == noneNode = noneNode
  | otherwise =
      let children' = map additionNode (children eq)
      in case name eq of

  ------------------------------------------------------------
  -- ADDITION
  ------------------------------------------------------------
  "f_add" ->
    let
        -- Step 1: collect numeric constants
        (con, nonNums) = foldr
          (\c (accNum, accSym) ->
             case frac c of
               Just r  -> (accNum + r, accSym)
               Nothing -> (accNum, c:accSym))
          (0, [])
          children'

        -- Step 2: split term into (base, multiplier)
        splitTerm :: TreeNode -> (TreeNode, TreeNode)
        splitTerm t
          | name t == "f_mul" =
              let (nums, others) = foldr
                    (\x (ns, os) ->
                       case frac x of
                         Just _  -> (x:ns, os)
                         Nothing -> (ns, x:os))
                    ([], [])
                    (children t)
                  multiplier =
                    case nums of
                      []  -> dR 1
                      [x] -> x
                      xs  -> TreeNode "f_mul" xs
                  base =
                    case others of
                      []  -> dR 1
                      [x] -> x
                      xs  -> TreeNode "f_mul" xs
              in (base, multiplier)
          | otherwise = (t, dR 1)

        -- Step 3: merge like bases
        mergeTerms :: [(TreeNode, TreeNode)] -> [(TreeNode, TreeNode)]
        mergeTerms [] = []
        mergeTerms ((b,m):xs) =
          let (same, rest) = span (\(b',_) -> sortTree b == sortTree b') xs
              totalM = foldr (\(_,m') acc -> TreeNode "f_add" [m', acc]) m same
          in (b, totalM) : mergeTerms rest

        baseTerms = mergeTerms (map splitTerm nonNums)

        -- Step 4: rebuild terms
        rebuildTerm :: (TreeNode, TreeNode) -> TreeNode
        rebuildTerm (b,m)
          | m == dR 0 = dR 0
          | m == dR 1 = b
          | b == dR 1 = m
          | otherwise  = TreeNode "f_mul" [b,m]

        rebuiltTerms = map rebuildTerm baseTerms

        -- Step 5: add numeric constant using fracToTree
        conTree = fracToTree con
        finalChildren =
          let lst = filter (/= dR 0) rebuiltTerms
          in if conTree /= dR 0 then lst ++ [conTree] else lst
    in case finalChildren of
         []  -> dR 0
         [x] -> x
         xs  -> TreeNode "f_add" xs

  ------------------------------------------------------------
  -- DEFAULT: recursively rebuild other nodes
  ------------------------------------------------------------
  _ -> TreeNode (name eq) children'


safePow :: Maybe Rational -> Maybe Rational -> Maybe Rational
safePow (Just base) (Just exp)
  -- 0^negative → division by zero
  | base == 0 && exp < 0 = Nothing

  -- fractional power of negative number → invalid
  | denominator exp /= 1 = Nothing

  -- safe integer power
  | otherwise = Just (base ^^ numerator exp)
safePow _ _ = Nothing


otherNode :: TreeNode -> TreeNode
otherNode eq
  | eq == noneNode = noneNode
  -- LOG
  | name eq == "f_log" =
      case children eq of
        [c] | c == dR 1 -> dR 0
            | c == s_e   -> dR 1
        _ -> TreeNode "f_log" (map otherNode (children eq))

  -- POWER
  | name eq == "f_pow" =
      case children eq of
        [a,b] ->
          let a' = otherNode a
              b' = otherNode b
              fa = frac a'
              fb = frac b'
              computedA = compute a'
              computedB = compute b'
          in case (fa, fb) of
               (Just 0, Just y) | y < 0 -> noneNode
               _ | b' == dR 0 -> dR 1
                 | b' == dR 1 -> a'
                 | a' == dR 1 -> dR 1
                 | a' == dR 0 && maybe False (>0) computedB -> dR 0
                 | otherwise ->
                     -- distribute exponent over multiplication
                     if name a' == "f_mul"
                     then TreeNode "f_mul" [TreeNode "f_pow" [c, b'] | c <- children a']
                     -- combine nested powers if base positive or absolute
                     else if maybe False (>0) computedA || name a' == "f_abs"
                          then case name a' of
                                 "f_pow" ->
                                   let innerExp = children a' !! 1
                                       newExp   = TreeNode "f_mul" [innerExp, b']
                                   in TreeNode "f_pow" [children a' !! 0, newExp]
                                 _ -> case safePow fa fb of
                                        Just r  -> fracToTree r
                                        Nothing -> TreeNode "f_pow" [a', b']
                     else if maybe False (\x -> x < 0 && x /= (-1)) fb
                     then
                         TreeNode "f_pow" [TreeNode "f_pow" [a', neg b'], dR (-1)]
                     else
                         TreeNode "f_pow" [a', b']
        _ -> TreeNode "f_pow" (map otherNode (children eq))

  -- DEFAULT
  | otherwise =
      case frac eq of
        Just r  -> fracToTree r       -- numeric constant: convert to TreeNode
        Nothing ->
          let children' = map otherNode (children eq)  -- recursively process children
          in if noneNode `elem` children'
               then noneNode                         -- if any child is noneNode, return noneNode
               else TreeNode (name eq) children'


-- Check if a TreeNode represents an even integer
evenPower :: TreeNode -> Bool
evenPower t = case t of
  TreeNode ('d':'_':rest) [] ->    -- Numeric literal like "d_2"
    case reads rest :: [(Integer, String)] of
      [(n,"")] -> even n
      _        -> False
  _ -> False

solve3 :: TreeNode -> TreeNode
solve3 eq = dowhile eq (\x -> flattenTree (otherNode (additionNode (multiplyNode x))))

-- Mapping for inequality flips when denominator is negative
flipIneq :: String -> String
flipIneq s = case s of
    "gt" -> "lt"
    "ge" -> "le"
    "lt" -> "gt"
    "le" -> "ge"
    "eq" -> "eq"
    _    -> s

simplify :: TreeNode -> TreeNode
simplify eq
    | eq == noneNode = noneNode
    | name eq `elem` ["f_and","f_or","f_not"] =
        TreeNode (name eq) (map simplify (children eq))
    | drop 2 (name eq) `elem` ["gt","ge","lt","le","eq"] =
        let denom0 = name eq /= "f_eq"
            lhs = simplify (children eq !! 0)
            rhs = simplify (children eq !! 1)
            diff = fSub lhs rhs                -- Subtract TreeNodes
            (tmp, denom) = clearDiv diff denom0  -- Pass two arguments separately
            tmp' = simplify tmp
            valueStr = drop 2 (name eq)
            valueStr' = if denom then valueStr else flipIneq valueStr
            outName = "f_" ++ valueStr'
        in TreeNode outName [tmp', dR 0]
    | otherwise =
        let eq' = flattenTree eq
            eqBasic = convertToBasic eq'
        in solve3 eqBasic