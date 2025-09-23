import copy
from lark import Lark, Tree
from base import *
import re

grammar = """
?start: expr

?expr: logic_equiv

?logic_equiv: logic_imply
            | logic_equiv "<->" logic_imply  -> equiv

?logic_imply: logic_or
            | logic_or "->" logic_imply      -> imply

?logic_or: logic_and
         | logic_or "|" logic_and            -> union
         | logic_or "||" logic_and           -> union

?logic_and: unary
          | logic_and "&" unary              -> intersection
          | logic_and "&&" unary             -> intersection

?unary: "~" unary     -> exclude
      | "!" unary     -> exclude
      | "-" unary     -> neg
      | comparison

?comparison: arithmetic
           | comparison "=" arithmetic  -> eq
           | comparison "<" arithmetic  -> lt
           | comparison ">" arithmetic  -> gt
           | comparison "<=" arithmetic -> le
           | comparison ">=" arithmetic -> ge

?arithmetic: term
           | arithmetic "+" term   -> add
           | arithmetic "-" term   -> sub

?term: factor
     | term "*" factor  -> mul
     | term "/" factor  -> div
     | term "." factor  -> dot

?factor: power

?power: base
      | power "^" base  -> pow
      | power "**" base  -> pow

?base: atom index*

?atom: NUMBER              -> number
     | capital_func
     | VARIABLE            -> variable
     | FUNC_NAME "(" [expr ("," expr)*] ")" -> func
     | "[" [expr ("," expr)*] "]" -> list
     | "(" expr ")"        -> paren
     | CNUMBER             -> cnumber
     | ESCAPED_STRING      -> string

?capital_func: CAPITAL_ID "(" [expr ("," expr)*] ")"   -> func
             | CAPITAL_ID                              -> matrix

?index: "[" expr "]"     -> index

FUNC_NAME: "midpoint" | "forall" | "imply" | "exist" | "len" | "sum" | "angle" | "line" | "sum2" | "charge" | "electricfield" | "perm" | "point" | "equationrhs" | "transpose" | "equationlhs" | "equation" | "error" | "covariance" | "variance" | "expect" | "mag" | "rad" | "laplace" | "diverge" | "pdif" | "gradient" | "curl" | "point1" | "point2" | "dot" | "point3" | "line1" | "line2" | "line3" | "sin" | "circumcenter" | "eqtri" | "linesegment" | "cos" | "tan" | "log" | "sqrt" | "integrate" | "dif" | "abs" | "cosec" | "sec" | "cot" | "arctan" | "arcsin" | "arccos" | "log10"

VARIABLE: /[a-z]/ | "nabla" | "pi" | "kc" | "hbar" | "em" | "ec" | "anot" | "null" | "all"

CAPITAL_ID: /[A-Z]/

CNUMBER: /c[0-9]+/

%import common.NUMBER
%import common.ESCAPED_STRING
%import common.WS_INLINE
%ignore WS_INLINE
"""

def parse(equation, funclist=None):
  global grammar
  equation = copy.copy(equation.replace(" ", ""))
  grammar2 = copy.deepcopy(grammar)
  if funclist is not None:
      output = grammar2.split("\n")
      for i in range(len(output)):
          if "FUNC_NAME:" in output[i]:
              output[i] = output[i].replace("FUNC_NAME: ", "FUNC_NAME: " + " | ".join(['"' + x + '"' for x in funclist]) + " | ")
      grammar2 = "\n".join(output)
  parser_main = Lark(grammar2, start='start', parser='lalr')
  parse_tree = parser_main.parse(equation)
  def convert_to_treenode(parse_tree):
      def tree_to_treenode(tree):
          if isinstance(tree, Tree):
              node = TreeNode(tree.data)
              node.children = [tree_to_treenode(child) for child in tree.children]
              return node
          else:
              return TreeNode(str(tree))
      return tree_to_treenode(parse_tree)
  def remove_past(equation):
      if equation.name in {"number", "paren", "func", "variable", "cnumber", "string", "matrix"}:
          if len(equation.children) == 1:
            for index, child in enumerate(equation.children):
              equation.children[index] = remove_past(child)
            return equation.children[0]
          else:
            for index, child in enumerate(equation.children):
              equation.children[index] = remove_past(child)
            return TreeNode(equation.children[0].name, equation.children[1:])
      coll = TreeNode(equation.name, [])
      for child in equation.children:
          coll.children.append(remove_past(child))
      return coll
  def prefixindex(equation):
      if equation.name == "base":
          return TreeNode("index", [equation.children[0]]+equation.children[1].children)
      return TreeNode(equation.name, [prefixindex(child) for child in equation.children])
  tree_node = convert_to_treenode(parse_tree)
  tree_node = remove_past(tree_node)
  
  tree_node = prefixindex(tree_node)
  
  def fxchange(tree_node):
    nonlocal funclist
    tmp3 = []
    if funclist is not None:
        tmp3 = funclist
    return TreeNode("f_"+tree_node.name if tree_node.name in tmp3+["imply", "B", "forall", "exist", "exclude", "union", "intersection", "len", "A", "index", "angle", "charge", "sum2", "electricfield", "line", "point", "sum", "V", "W", "transpose", "equationrhs", "equationlhs", "equation", "covariance", "variance", "expect", "error", "laplace", "dot", "curl", "pdif", "diverge", "gradient", "rad", "ge", "le", "gt", "lt", "F", "rad", "eqtri", "linesegment", "midpoint", "mag", "point1", "point2", "point3", "line1", "line2", "line3", "log10", "arcsin", "arccos", "arctan", "list", "cosec", "sec", "cot", "equiv", "or", "not", "and", "circumcenter", "transpose", "eq", "sub", "neg", "inv", "add", "sin", "cos", "tan", "mul", "integrate", "dif", "pow", "div", "log", "abs"]\
                    else "d_"+tree_node.name, [fxchange(child) for child in tree_node.children])
  tree_node = fxchange(tree_node)
  tree_node = replace(tree_node, tree_form("d_e"), tree_form("s_e"))
  tree_node = replace(tree_node, tree_form("d_pi"), tree_form("s_pi"))
  tree_node = replace(tree_node, tree_form("d_kc"), tree_form("s_kc"))
  tree_node = replace(tree_node, tree_form("d_em"), tree_form("s_em"))
  tree_node = replace(tree_node, tree_form("d_ec"), tree_form("s_ec"))
  tree_node = replace(tree_node, tree_form("d_anot"), tree_form("s_anot"))
  tree_node = replace(tree_node, tree_form("d_hbar"), tree_form("s_hbar"))
  tree_node = replace(tree_node, tree_form("d_null"), tree_form("s_null"))
  tree_node = replace(tree_node, tree_form("d_all"), tree_form("s_all"))
  tree_node = replace(tree_node, tree_form("d_i"), tree_form("s_i"))
  tree_node = replace(tree_node, tree_form("d_nabla"), tree_form("s_nabla"))

  def rfx(tree_node):
      if tree_node.name[:2] == "f_" and any(tree_node.children[i].name == "f_index" for i in range(len(tree_node.children))):
          index = 0
          for i in range(len(tree_node.children)):
              if tree_node.children[i].name == "f_index":
                  index = i
                  break
          if index != 0:
              return TreeNode("f_index", [TreeNode(tree_node.name, tree_node.children[:index])]+tree_node.children[index:])
      if tree_node.name == "f_line":
          return TreeNode("f_line", [tree_form("g_"+tree_node.children[0].name.replace('d_', '').replace('"', ''))])
      if tree_node.name == "f_angle":
          return TreeNode("f_angle", [tree_form("g_"+tree_node.children[0].name.replace('d_', '').replace('"', ''))])
      if tree_node.name[:2] == "d_" and tree_node.name[2:3] in ["A", "B", "C"]:
          out = "v_-"+str(ord(tree_node.name[2:3])-ord("A"))
          return TreeNode(out, tree_node.children)
      if tree_node.name[:2] == "d_" and tree_node.name[2:3] in ["a", "b", "c"]:
          out = "v_"+str(ord(tree_node.name[2:3])-ord("a"))
          return TreeNode(out, tree_node.children)
      if tree_node.name[:3] == "d_c":
          return tree_form("v_" + str(int(tree_node.name[3:])+100))
      return TreeNode(tree_node.name, [rfx(child) for child in tree_node.children])

  for i in range(26):
    alpha = ["x", "y", "z"]+[chr(x+ord("a")) for x in range(0,23)]
    beta = [chr(x+ord("A")) for x in range(0,26)]
    tree_node = replace(tree_node, tree_form("d_"+alpha[i]), tree_form("v_"+str(i)))
    tree_node = replace(tree_node, tree_form("d_"+beta[i]), tree_form("v_-"+str(i+1)))
    tree_node = replace(tree_node, tree_form("f_"+beta[i]), tree_form("v_-"+str(i+1)))
  tmp5 = rfx(tree_node)
  return tmp5
