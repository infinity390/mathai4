{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Base
import Data.Char

------------------------------------------------------------
-- TOKENS
------------------------------------------------------------

data Token
  = TNum Integer
  | TIdent String
  | TOp String
  | TLParen | TRParen
  | TLBrack | TRBrack
  | TComma
  | TEOF
  deriving (Eq, Show)

------------------------------------------------------------
-- LEXER
------------------------------------------------------------

lexer :: String -> [Token]
lexer [] = [TEOF]
lexer (c:cs)
  | isSpace c = lexer cs

  | isDigit c =
      let (ds, r) = span isDigit (c:cs)
      in TNum (read ds) : lexer r

  | isAlpha c =
      let (xs, r) = span isAlpha (c:cs)
      in TIdent xs : lexer r

  | c `elem` "+-*/^=<>&|!" =
      let (op, r) = span (`elem` "+-*/^=<>&|!") (c:cs)
      in TOp op : lexer r

  | c == '(' = TLParen : lexer cs
  | c == ')' = TRParen : lexer cs
  | c == '[' = TLBrack : lexer cs
  | c == ']' = TRBrack : lexer cs
  | c == ',' = TComma  : lexer cs

  | otherwise =
      error ("Unknown character: " ++ [c])

------------------------------------------------------------
-- PARSER STATE
------------------------------------------------------------

newtype Parser = Parser [Token]

peek :: Parser -> Token
peek (Parser (t:_)) = t
peek _ = TEOF

consume :: Parser -> (Token, Parser)
consume (Parser (t:ts)) = (t, Parser ts)
consume _ = (TEOF, Parser [])

------------------------------------------------------------
-- TOP LEVEL
------------------------------------------------------------

parse :: String -> TreeNode
parse s =
  let (e, p) = parseExpr (Parser (lexer s))
      tree = case peek p of
               TEOF -> e
               _    -> error "Extra input"
  in renameVars tree

-- Rename placeholders in the tree
renameVars :: TreeNode -> TreeNode
renameVars t =
  let newName
        | name t == "v_pi" = "s_pi"
        | name t == "v_i"  = "s_i"
        | name t == "v_e"  = "s_e"
        | otherwise        = name t
  in TreeNode newName (map renameVars (children t))


------------------------------------------------------------
-- GRAMMAR (mirrors Lark)
------------------------------------------------------------

-- expr → logic_equiv
parseExpr = parseEquiv

------------------------------------------------------------
-- logic
------------------------------------------------------------

parseEquiv p =
  let (a, p1) = parseImply p
  in case peek p1 of
       TOp "<->" ->
         let (_, p2) = consume p1
             (b, p3) = parseEquiv p2
         in (TreeNode "f_equiv" [a,b], p3)
       _ -> (a, p1)

parseImply p =
  let (a, p1) = parseOr p
  in case peek p1 of
       TOp "->" ->
         let (_, p2) = consume p1
             (b, p3) = parseImply p2
         in (TreeNode "f_imply" [a,b], p3)
       _ -> (a, p1)

parseOr p =
  let (a, p1) = parseAnd p
  in case peek p1 of
       TOp "|" ->
         let (_, p2) = consume p1
             (b, p3) = parseOr p2
         in (TreeNode "f_or" [a,b], p3)
       _ -> (a, p1)

parseAnd p =
  let (a, p1) = parseNot p
  in case peek p1 of
       TOp "&" ->
         let (_, p2) = consume p1
             (b, p3) = parseAnd p2
         in (TreeNode "f_and" [a,b], p3)
       _ -> (a, p1)

parseNot p =
  case peek p of
    TOp "!" ->
      let (_, p1) = consume p
          (a, p2) = parseNot p1
      in (TreeNode "f_not" [a], p2)
    _ -> parseCompare p

------------------------------------------------------------
-- comparison
------------------------------------------------------------

parseCompare p =
  let (a, p1) = parseAdd p
  in case peek p1 of
       TOp "="  -> bin "f_eq" a p1
       TOp "<"  -> bin "f_lt" a p1
       TOp ">"  -> bin "f_gt" a p1
       TOp "<=" -> bin "f_le" a p1
       TOp ">=" -> bin "f_ge" a p1
       _        -> (a, p1)
  where
    bin name a p =
      let (_, p1) = consume p
          (b, p2) = parseAdd p1
      in (TreeNode name [a,b], p2)

------------------------------------------------------------
-- arithmetic
------------------------------------------------------------

parseAdd p =
  let (a, p1) = parseMul p
  in case peek p1 of
       TOp "+" -> bin "f_add" a p1
       TOp "-" -> bin "f_sub" a p1
       _       -> (a, p1)
  where
    bin n a p =
      let (_, p1) = consume p
          (b, p2) = parseAdd p1
      in (TreeNode n [a,b], p2)

parseMul p =
  let (a, p1) = parsePow p
  in case peek p1 of
       TOp "*" -> bin "f_mul" a p1
       TOp "/" -> bin "f_div" a p1
       _       -> (a, p1)
  where
    bin n a p =
      let (_, p1) = consume p
          (b, p2) = parseMul p1
      in (TreeNode n [a,b], p2)

parsePow :: Parser -> (TreeNode, Parser)
parsePow p =
  let (a, p1) = parseFactor p
  in case peek p1 of
       TOp "^" ->
         let (_, p2) = consume p1
             (b, p3) = parseFactor p2   -- parseFactor now handles divisions
         in (TreeNode "f_pow" [a,b], p3)
       _ -> (a, p1)

parseFactor :: Parser -> (TreeNode, Parser)
parseFactor p =
  case peek p of
    TOp "-" ->
      let (_, p1) = consume p
          (a, p2) = parseFactor p1
      in (TreeNode "f_neg" [a], p2)

    TNum n ->
      let (_, p1) = consume p
      in (d n, p1)

    TIdent f ->
      let (_, p1) = consume p
      in case peek p1 of
           TLParen ->
             let (_, p2) = consume p1
                 (args, p3) = parseArgs p2 TRParen
             in (TreeNode ("f_" ++ f) args, p3)
           _ -> (v f, p1)

    TLParen ->
      let (_, p1) = consume p
          (e, p2) = parseExpr p1
      in case peek p2 of
           TRParen -> (e, snd (consume p2))
           _       -> error "Missing closing parenthesis"

    _ -> error ("Unexpected token in parseFactor: " ++ show (peek p))


------------------------------------------------------------
-- parseArgs now handles closing token dynamically (paren/brack)
------------------------------------------------------------

parseArgs :: Parser -> Token -> ([TreeNode], Parser)
parseArgs p closing =
  case peek p of
    t | t == closing -> ([], snd (consume p))
    _ ->
      let (a, p1) = parseExpr p
      in case peek p1 of
           TComma ->
             let (_, p2) = consume p1
                 (as, p3) = parseArgs p2 closing
             in (a:as, p3)
           t | t == closing -> ([a], snd (consume p1))
           _ -> error "Bad argument list"
