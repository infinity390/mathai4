from .simplify import simplify
from .base import *
from .parser import parse
from .formula_data import load_formula

def diff2_h(eq):
    if not eq.children:
        return eq
    if eq.name == "f_pdif":
        if contain(eq.children[0], eq.children[1]):
            eq2 = replace(copy.deepcopy(eq), eq.children[1], parse("x"))
            eq2 = simplify(eq2)
            out = load_formula["differentiation"](copy.deepcopy(eq2))
            if out is not None:
                out = replace(out, parse("x"), eq.children[1])
                if out != eq:
                    return out
        else:
            return tree_form("d_0")
    return eq
def diff2(eq):
    return dowhile(eq, lambda x: transform_dfs(simplify(x), diff2_h))
def diff(equation, var=None):
    var = var if var else "v_0"
    return diff2(TreeNode("f_pdif", [equation, tree_form(var)]))
