from .simplify import simplify
from .base import *
from .parser import parse
from .formula_list_compiler import formula_list_compiler
def diff_formula_init():
    formula_list = [
        (f"pdif(a^b,x)", f"b*(a^(b-1))*pdif(a,x) + (a^b)*log(a)*pdif(b,x)"),
        (f"pdif(sin(a),x)", f"cos(a)*pdif(a,x)"),
        (f"pdif(cos(a),x)", f"-sin(a)*pdif(a,x)"),
        (f"pdif(arcsin(a),x)", f"(1/sqrt(1-a^2))*pdif(a,x)"),
        (f"pdif(arccos(a),x)", f"(-1/sqrt(1-a^2))*pdif(a,x)"),
        (f"pdif(arctan(a),x)", f"(1/(1+a^2))*pdif(a,x)"),
        (f"pdif(x,x)", "1"),
        (f"pdif(k,x)", "0"),
        (f"pdif(c*d,x)", f"pdif(c,x)*d+c*pdif(d,x)"),
        (f"pdif(f+g,x)", f"pdif(f,x)+pdif(g,x)"),
        (f"pdif(k*a,x)", f"k*pdif(a,x)"),
    ]
    formula_list = [[simplify(parse(x[0])), simplify(parse(x[1])), ["v_0"], "v_0", [parse("k").name],\
                     {"v_5":1, "v_6":1, parse("f").name:0, parse("g").name:0}, [], [], []] for x in formula_list]
    return formula_list_compiler(formula_list)
helper_fx = diff_formula_init()
print("differentiation formulas compiled")
def diff2_h(eq):
    global helper_fx
    if not eq.children:
        return eq
    if eq.name == "f_pdif":
        if contain(eq.children[0], eq.children[1]):
            eq2 = replace(copy.deepcopy(eq), eq.children[1], parse("x"))
            eq2 = simplify(eq2)
            out = helper_fx(copy.deepcopy(eq2))
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
