from .diff import diff
from .expand import expand
from .simplify import simplify
from .base import *
import math

def enclose_const(eq):
    def req(eq, dic):
        for key in dic.keys():
            eq  = replace(eq, dic[key], key)
        return eq
    alloclst = []
    for i in range(0,26):
        if "v_"+str(i) not in vlist(eq):
            alloclst.append(tree_form("v_"+str(i)))
    dic = {}
    def helper(eq):
        nonlocal alloclst, dic
        if frac(eq) is not None:
            return eq
        
        if "v_" not in str_form(eq):
            if eq not in dic.keys():
                n = alloclst.pop(0)
                dic[eq] = n
            return dic[eq]
        else:
            if eq.name == "f_pow":
                return TreeNode(eq.name, [helper(eq.children[0]), eq.children[1]])
            return TreeNode(eq.name, [helper(child) for child in eq.children])
    eq= helper(eq)
    return eq, lambda x: req(x, dic)

def poly(eq, to_compute):
    def substitute_val(eq, val, var="v_0"):
        eq = replace(eq, tree_form(var), tree_form("d_"+str(val)))
        return eq
    def inv(eq):
        if eq.name =="f_eq":
            return False
        if eq.name[2:] in ["sin", "cos", "log"] and contain(eq.children[0], tree_form(to_compute)):
            return False
        if eq.name == "f_pow" and contain(eq.children[0], tree_form(to_compute)) and\
           (frac(eq.children[1]) is None or frac(eq.children[1]) < 0 or frac(eq.children[1]).denominator != 1):
            return False
        if eq.name == "f_abs":
            return False
        if any(not inv(child) for child in eq.children):
            return False
        return True
    if not inv(eq):
        return None
    out = []
    eq2 = eq
    for i in range(8):
        out.append(expand(simplify(eq2)))
        eq2 = diff(eq2, to_compute)
    for i in range(len(out)-1,-1,-1):
        if out[i] == tree_form("d_0"):
            out.pop(i)
        else:
            break
    final = []
    for index, item in enumerate(out):
        final.append(substitute_val(item, 0, to_compute)/tree_form("d_"+str(math.factorial(index))))
        
    return [expand(simplify(item)) for item in final][::-1]
