{-# LANGUAGE FlexibleInstances #-}

module Simplify where

import Data.Ratio
import Data.Maybe (fromMaybe, isJust, fromJust)
import Base 
import Data.Foldable (foldl')
import Data.List (isPrefixOf, partition)

import Debug.Trace (traceShow)

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

-- Multiply node recursively, merging powers and canceling automatically
multiplyNode :: TreeNode -> TreeNode
multiplyNode t@(TreeNode name children)
  | name == "f_mul" =
      let children' = map multiplyNode children
          -- separate constants from symbolic using frac
          (nums, syms) = partition (\c -> frac c /= Nothing) children'
          coef = product [r | Just r <- map frac nums]  -- multiply all numeric constants

          -- convert syms to (base, exponent) list
          basePowers = foldl insertPower [] syms

          insertPower :: [(TreeNode, TreeNode)] -> TreeNode -> [(TreeNode, TreeNode)]
          insertPower acc node =
            let (base, pow) = case node of
                                TreeNode "f_pow" [b,e] -> (b,e)
                                _ -> (node, dR 1)
            in case lookup base acc of
                 Just oldPow -> map (\(b',p) -> if b' == base then (b', TreeNode "f_add" [p,pow]) else (b',p)) acc
                 Nothing     -> (base,pow):acc

          -- rebuild nodes, skip powers = 0
          rebuilt = [ if p == dR 1 then b else TreeNode "f_pow" [b,p]
                    | (b,p) <- basePowers, p /= dR 0 ]

          -- add coefficient if not 1
          finalChildren = if coef /= 1 then rebuilt ++ [fracToTree coef] else rebuilt
      in case finalChildren of
           []  -> dR 1
           [x] -> x
           xs  -> TreeNode "f_mul" xs

  | name == "f_div" =  -- convert division to negative powers
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
      in if noneNode `elem` children'
            then noneNode
            else case name eq of

  ------------------------------------------------------------
  -- ADDITION
  ------------------------------------------------------------
  "f_add" ->
    let
        -- collect numeric constants
        (con, nonNums) =
          foldr
            (\c (acc, rest) ->
               case frac c of
                 Just r  -> (acc + r, rest)
                 Nothing -> (acc, c : rest))
            (0, [])
            children'

        -- split a term into (base, multiplier)
        splitTerm :: TreeNode -> (TreeNode, TreeNode)
        splitTerm t =
          case t of
            TreeNode "f_mul" cs ->
              let
                  (nums, others) =
                    foldr
                      (\x (ns, os) ->
                         case frac x of
                           Just _  -> (x:ns, os)
                           Nothing -> (ns, x:os))
                      ([], [])
                      cs

                  powerNode =
                    case filter (/= dR 0) nums of
                      []  -> dR 1
                      [x] -> x
                      xs  -> TreeNode "f_mul" xs

                  baseNode =
                    case filter (/= dR 1) others of
                      []  -> dR 1
                      [x] -> x
                      xs  -> TreeNode "f_mul" xs
              in
                ( baseNode
                , powerNode
                )

            _ -> (t, dR 1)

        -- merge like bases
        mergeTerms :: [(TreeNode, TreeNode)] -> (TreeNode, TreeNode)
                   -> [(TreeNode, TreeNode)]
        mergeTerms [] t = [t]
        mergeTerms ((b,m):xs) (b',m')
          | sortTree b == sortTree b'   = (b, TreeNode "f_add" [m, m']) : xs
          | otherwise = (b,m) : mergeTerms xs (b',m')

        baseTerms =
          foldl mergeTerms [] (map splitTerm nonNums)

        rebuilt =
          [ case multiplier of
              t | t == dR 1 -> base
              t | t == dR 0 -> dR 0
              t             -> TreeNode "f_mul" [base, t]
          | (base, multiplier) <- baseTerms
          , multiplier /= dR 0
          ]

        finalChildren =
          let conTree = fracToTree con
          in if conTree /= dR 0
                then rebuilt ++ [conTree]
                else rebuilt

    in case finalChildren of
         []  -> dR 0
         [x] -> x
         xs  -> TreeNode "f_add" xs

  ------------------------------------------------------------
  -- DEFAULT: rebuild node recursively
  ------------------------------------------------------------
  _ ->
    TreeNode (name eq) children'


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

  -- MULTIPLICATION
  | name eq == "f_mul" =
      let children' = filter (/= dR 1) (map otherNode (children eq))
      in multiplyNode (TreeNode "f_mul" children')

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
                     then multiplyNode $ TreeNode "f_mul" [TreeNode "f_pow" [c, b'] | c <- children a']
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
                     else if maybe False (<0) fb
                     then
                         TreeNode "f_pow" [TreeNode "f_pow" [a', neg b'], dR (-1)]
                     else
                         TreeNode "f_pow" [a', b']
        _ -> TreeNode "f_pow" (map otherNode (children eq))

  -- DEFAULT
  | otherwise =
      let children' = map otherNode (children eq)
      in if noneNode `elem` children' then noneNode else TreeNode (name eq) children'


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


------------------------------------------------------------
-- Check if a TreeNode is numeric (d_<number>)
------------------------------------------------------------
isNumber :: TreeNode -> Bool
isNumber t = case t of
  TreeNode ('d':'_':_) [] -> True
  _                        -> False

------------------------------------------------------------
-- Get the numeric value from a TreeNode ("d_<num>")
------------------------------------------------------------
getNumber :: TreeNode -> Rational
getNumber (TreeNode n [])
  | "d_" `isPrefixOf` n =
      let str = drop 2 n  -- remove "d_"
      in case reads str :: [(Integer,String)] of
           [(x,"")] -> x % 1
           _        -> error $ "Invalid numeric literal in getNumber: " ++ str
getNumber _ = error "getNumber called on non-numeric TreeNode"

------------------------------------------------------------
-- Flatten a multiplication node into a list of children
------------------------------------------------------------
flattenMul :: TreeNode -> [TreeNode]
flattenMul (TreeNode "f_mul" cs) = concatMap flattenMul cs
flattenMul x = [x]