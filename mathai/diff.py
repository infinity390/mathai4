from .simplify import simplify
from .base import *
from .parser import parse
from .structure import make_formula_function

def diff_formula_init():
    formula_list = []
    for fx in ["pdif", "dif"]:
        tmp = [
            (f"{fx}(a+b,x)", f"{fx}(a,x)+{fx}(b,x)"),
            (f"{fx}(a*b,x)", f"{fx}(a,x)*b+a*{fx}(b,x)"),
            (f"{fx}(x,x)", "1"),
            (f"{fx}(a^b,x)", f"b*(a^(b-1))*{fx}(a,x) + (a^b)*log(a)*{fx}(b,x)"),
            (f"{fx}(sin(a),x)", f"cos(a)*{fx}(a,x)"),
            (f"{fx}(cos(a),x)", f"-sin(a)*{fx}(a,x)"),
            (f"{fx}(arcsin(a),x)", f"(1/sqrt(1-a^2))*{fx}(a,x)"),
            (f"{fx}(arccos(a),x)", f"(-1/sqrt(1-a^2))*{fx}(a,x)"),
            (f"{fx}(arctan(a),x)", f"(1/(1+a^2))*{fx}(a,x)")
        ]
        formula_list += tmp
    formula_list = [[simplify(parse(x[0])), simplify(parse(x[1])), [], None, [], [], {}] for x in formula_list]
    return make_formula_function(formula_list)
helper_fx = diff_formula_init()
def helper(eq):
    global helper_fx
    if eq.name in ["f_dif", "f_pdif"] and not contain(eq.children[0], eq.children[1]):
        return tree_form("d_0")
    out = helper_fx(eq)
    if out is None:
        return eq
    return out
def diff2(eq):
    global helper
    if eq is None:
        return None
    return dowhile(eq, lambda x: transform_dfs(x, lambda y: dowhile(y, helper)))

def diff(equation, var="v_0"):
    def diffeq(eq):
        eq = simplify(eq)
        if "v_" not in str_form(eq):
            return tree_form("d_0")
        if eq.name == "f_add":
            add = tree_form("d_0")
            for child in eq.children:
                add += diffeq(child)
            return add
        elif eq.name == "f_abs":
            return diffeq(eq.children[0])*eq.children[0]/eq
        elif eq.name == "f_pow" and eq.children[0].name == "s_e":
            return diffeq(eq.children[1])*eq
        elif eq.name == "f_tan":
            return diffeq(eq.children[0])/(eq.children[0].fx("cos")*eq.children[0].fx("cos"))
        elif eq.name == "f_log":
            return diffeq(eq.children[0])*(tree_form("d_1")/eq.children[0])
        elif eq.name == "f_arcsin":
            return diffeq(eq.children[0])/(tree_form("d_1")-eq.children[0]*eq.children[0])**(tree_form("d_2")**-1)
        elif eq.name == "f_arccos":
            return tree_form("d_-1")*diffeq(eq.children[0])/(tree_form("d_1")-eq.children[0]*eq.children[0])**(tree_form("d_2")**-1)
        elif eq.name == "f_arctan":
            return diffeq(eq.children[0])/(tree_form("d_1")+eq.children[0]*eq.children[0])
        elif eq.name == "f_pow" and "v_" in str_form(eq.children[1]):
            a, b = eq.children
            return a**b * ((b/a) * diffeq(a) + a.fx("log") * diffeq(b))
        elif eq.name == "f_mul":
            add = tree_form("d_0")
            for i in range(len(eq.children)):
                tmp = eq.children.pop(i)
                if len(eq.children)==1:
                    eq2 = eq.children[0]
                else:
                    eq2 = eq
                add += diffeq(tmp)*eq2
                eq.children.insert(i, tmp)
            return add
        elif eq.name == "f_sin":
            eq.name = "f_cos"
            return diffeq(eq.children[0])*eq
        elif eq.name == "f_cos":
            eq.name = "f_sin"
            return tree_form("d_-1")*diffeq(eq.children[0])*eq
        elif eq.name[:2] == "v_":
            return TreeNode("f_dif", [eq])
        elif eq.name == "f_pow" and "v_" not in str_form(eq.children[1]):
            base, power = eq.children
            dbase = diffeq(base)
            b1 = power - tree_form("d_1")
            bab1 = TreeNode("f_pow", [base, b1])
            return power * bab1 * dbase
        return TreeNode("f_dif", [eq, tree_form(var)])
    def helper2(equation, var="v_0"):
        if equation.name == "f_dif":
            if equation.children[0].name == var:
                return tree_form("d_1")
            if not contain(equation.children[0], var):
                return tree_form("d_0")
            else:
                return equation
        return TreeNode(equation.name, [helper2(child, var) for child in equation.children])
    def calc(eq):
        if eq.name == "f_dif":
            return diffeq(eq.children[0])
        return TreeNode(eq.name, [calc(child) for child in eq.children])
    if var is None:
        return simplify(calc(equation))
    equation = diffeq(equation)
    equation = helper2(equation, var)
    return simplify(equation)
