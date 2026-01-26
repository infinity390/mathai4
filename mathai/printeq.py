from .base import *
from .factor import merge_sqrt
from .simplify import simplify
import copy
from fractions import Fraction
def abstractexpr(eq):
    if eq.name == "f_pow" and frac(eq.children[1])==Fraction(1,2):
        eq = eq.children[0].fx("sqrt")
    if eq.name == "f_pow" and frac(eq.children[1])==Fraction(-1,2):
        eq = eq.children[0].fx("sqrt")**-1
    if eq.name in ["f_mul", "f_pow"]:
        
        lst = factor_generation(eq)
        deno = [item.children[0]**int(item.children[1].name[3:]) for item in lst if item.name == "f_pow" and item.children[1].name[:3] == "d_-"]
        if eq.name == "f_mul" and any(frac(item) is not None and frac(item) < 0 for item in lst):
            return simplify(-eq, False).fx("neg")
        if deno != []:
            
            num = [item for item in lst if item.name != "f_pow" or item.children[1].name[:3] != "d_-"]
            if num == []:
                num = [tree_form("d_1")]
            return TreeNode("f_div", [simplify(product(num), False), simplify(product(deno), False)])
    
    
    return TreeNode(eq.name, [abstractexpr(child) for child in eq.children])

def printeq_str(eq):
    if eq is None:
        return None
    eq = merge_sqrt(eq)
    return string_equation(str_form(dowhile(eq, abstractexpr)))
def printeq_obj(self):
    return printeq_str(self)

def printeq(eq):
    print(printeq_str(eq))
TreeNode.__repr__ = printeq_obj
