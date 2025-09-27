from diff import diff
from expand import expand
from simplify import simplify
from base import *
import math

def poly(eq, to_compute):
    def substitute_val(eq, val, var="v_0"):
        eq = replace(eq, tree_form(var), tree_form("d_"+str(val)))
        return eq
    def inv(eq):
        if eq.name == "f_pow" and eq.children[1] == tree_form("d_-1"):
            return False
        if any(not inv(child) for child in eq.children):
            return False
        return True
    if not inv(eq):
        return None
    out = []
    eq2 = eq
    for i in range(10):
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
