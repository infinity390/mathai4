from .structure import structure
from .base import *
from .parser import parse
from .simplify import simplify
from .expand import expand
from .diff import diff
from .trig import trig0
from .fraction import fraction
def substitute_val(eq, val, var="v_0"):
    eq = replace(eq, tree_form(var), tree_form("d_"+str(val)))
    return eq
def subslimit(equation, var):
    equation2 = trig0(replace(equation, var, tree_form("d_0")))
    try:
        tmp = simplify(equation2)
        return simplify(expand(tmp))
    except:
        return None
def check(num, den, var):
    n, d = None, None
    n, d = (dowhile(replace(e, tree_form(var), tree_form("d_0")), lambda x: trig0(simplify(x))) for e in (num, den))
    if n is None or d is None:
        return False
    if n == 0 and d == 0: return True
    if d != 0:
        return simplify(n/d)
    return False
def lhospital(num, den, steps,var):
    out = check(num, den, var)
    if isinstance(out, TreeNode):
        return out
    for _ in range(steps):
        num2, den2 = map(lambda e: simplify(diff(e, var)), (num, den))
        out = check(num2, den2, var)
        if out is True:
            num, den = num2, den2
            continue
        if out is False:
            eq2 = simplify(fraction(simplify(num/den)))
            return eq2
        return out
def lhospital2(eq, var):
    eq=  simplify(eq)
    if eq is None:
        return None
    if not contain(eq, tree_form(var)):
        return eq
    num, dem = [simplify(item) for item in num_dem(eq)]
    if num is None or dem is None:
        return eq
    return lhospital(num, dem, 10,var)
def limit0(equation):
    if equation.name == "f_ref":
        return equation
    eq2 = equation
    g = ["f_limit", "f_limitpinf", "f_limitninf"]
    if eq2.name in g and contain(eq2.children[0], eq2.children[1]):
        equation = eq2.children[0]
        wrt = eq2.children[1]
        lst = factor_generation(equation)
        lst_const = [item for item in lst if not contain(item, wrt)]
        if lst_const != []:
            equation = product([item for item in lst if contain(item, wrt)]).copy_tree()
            const = product(lst_const)
            const = simplify(const)
            if not contain(const, tree_form("s_i")):
                return limit0(TreeNode(equation.name,[equation, wrt])) *const
        equation = eq2
    return TreeNode(equation.name, [limit0(child)  for child in equation.children])
def limit2(eq):
    g = ["f_limit", "f_limitpinf", "f_limitninf"]
    if eq.name in g and eq.children[0].name == "f_add":
        eq = summation([TreeNode(eq.name, [child, eq.children[1]]) for child in eq.children[0].children])
    return TreeNode(eq.name, [limit2(child) for child in eq.children])
def limit1(eq):
    if eq.name == "f_limit":
        a, b = limit(eq.children[0], eq.children[1].name)
        if b:
            return a
        else:
            return TreeNode(eq.name, [a, eq.children[1]])
    return TreeNode(eq.name, [limit1(child) for child in eq.children])
def fxinf(eq):
    if eq is None:
        return None
    if eq.name == "f_add":
        if tree_form("s_inf") in eq.children and -tree_form("s_inf") in eq.children:
            return None
        if tree_form("s_inf") in eq.children:
            return tree_form("s_inf")
        if -tree_form("s_inf") in eq.children:
            return -tree_form("s_inf")
    if eq.name == "f_mul":
        lst = factor_generation(eq)
        if tree_form("s_inf") in lst:
            eq = TreeNode(eq.name, [dowhile(child, fxinf) for child in eq.children])
            if None in eq.children:
                return None
            lst = factor_generation(eq)
            if tree_form("d_0") in lst:
                return tree_form("d_0")
            lst2 = [item for item in lst if "v_" in str_form(item)]
            sign = True
            if len([item for item in lst if "v_" not in str_form(item) and not contain(item, tree_form("s_inf")) and compute(item)<0]) % 2==1:
                sign = False
            if lst2 == []:
                if sign:
                    return tree_form("s_inf")
                else:
                    return -tree_form("s_inf")
    if eq.name == "f_pow":
        if "v_" not in str_form(eq.children[0]) and not contain(eq.children[0], tree_form("s_inf")) and compute(eq.children[0])>0:
            if eq.children[1] == -tree_form("s_inf"):
                return tree_form("d_0")
    eq = TreeNode(eq.name, [fxinf(child) for child in eq.children])
    if None in eq.children:
        return None
    return eq
def limit3(eq):
    if eq.name == "f_limitpinf":
        if not contain(eq, eq.children[1]):
            return eq.children[0]
        eq2 = replace(eq.children[0], eq.children[1], tree_form("s_inf"))
        eq2 = dowhile(eq2, fxinf)
        if not contain(eq2, tree_form("s_inf")) and not contain(eq2, eq.children[1]):
            return simplify(eq2)
    return TreeNode(eq.name, [limit3(child) for child in eq.children])
def limit(equation, var="v_0"):
    eq2 = dowhile(replace(equation, tree_form(var), tree_form("d_0")), lambda x: trig0(simplify(x)))
    if eq2 is not None and not contain(equation, tree_form(var)):
        return eq2, True
    equation =  lhospital2(equation, var)
    equation = simplify(expand(simplify(equation)))
    if not contain(equation, tree_form(var)):
        return equation, True
    return equation, False
