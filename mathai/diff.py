from .simplify import simplify
from .base import *
from .parser import parse
from .formula_list_compiler import formula_list_compiler
def diff_formula_init():
    formula_list = []
    for fx in ["pdif", "dif"]:
        tmp = [
            (f"{fx}(a^b,x)", f"b*(a^(b-1))*{fx}(a,x) + (a^b)*log(a)*{fx}(b,x)"),
            (f"{fx}(sin(a),x)", f"cos(a)*{fx}(a,x)"),
            (f"{fx}(cos(a),x)", f"-sin(a)*{fx}(a,x)"),
            (f"{fx}(arcsin(a),x)", f"(1/sqrt(1-a^2))*{fx}(a,x)"),
            (f"{fx}(arccos(a),x)", f"(-1/sqrt(1-a^2))*{fx}(a,x)"),
            (f"{fx}(arctan(a),x)", f"(1/(1+a^2))*{fx}(a,x)"),
            (f"{fx}(a+b,x)", f"{fx}(a,x)+{fx}(b,x)"),
            (f"{fx}(a*b,x)", f"{fx}(a,x)*b+a*{fx}(b,x)"),
            (f"{fx}(x,x)", "1"),
        ]
        formula_list += tmp
    formula_list = [[simplify(parse(x[0])), simplify(parse(x[1])), [], parse("v").name, [], {}, [], [], []] for x in formula_list]
    return formula_list_compiler(formula_list)
helper_fx = diff_formula_init()
print("differentiation formulas compiled")
def helper(eq):
    global helper_fx
    if eq.name in ["f_pdif"] and not contain(eq.children[0], eq.children[1]):
        return tree_form("d_0")
    if eq.name in ["f_dif"] and "v_" not in str_form(eq.children[0]):
        return tree_form("d_0")
    out = helper_fx(eq)
    if out is None:
        return eq
    return out
def diff2(eq):
    if eq is None:
        return None
    return dowhile(simplify(eq), lambda x: transform_dfs(x, lambda y: dowhile(y, helper)))
def diff(equation, var=None):
    var = var if var else "v_0"
    return diff2(TreeNode("f_pdif", [equation, tree_form(var)]))
