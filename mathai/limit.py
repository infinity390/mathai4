from .structure import structure
from .base import *
from .parser import parse
from .simplify import simplify
from .expand import expand
from .diff import diff
from .trig import trig0
from .fraction import fraction
from .tool import poly
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
def fxinf2(eq):
    if eq is None:
        return None
    if eq.name == "f_add":
        if tree_form("s_inf") in eq.children and -tree_form("s_inf") in eq.children:
            return None
        if tree_form("s_inf") in eq.children:
            return tree_form("s_inf")
        if -tree_form("s_inf") in eq.children:
            return -tree_form("s_inf")
    if eq.name == "f_pow":
        if "v_" not in str_form(eq.children[0]) and not contain(eq.children[0],tree_form("s_inf")) and simplify(eq.children[0]) != 1 and compute(eq.children[0]) > 1:
            if eq.children[1] == -tree_form("s_inf"):
                return tree_form("d_0")
    return eq
def fxinf3(eq):
    if eq is None:
        return None
    n, d = num_dem(eq)
    nlst = [item for item in factor_generation(n) if item != 1]
    dlst = [item for item in factor_generation(d) if item != 1]
    enter = True
    a = contain(n, tree_form("s_inf"))
    b = contain(d, tree_form("s_inf"))
    if a:
        if all(item == tree_form("s_inf") or not contain(item,tree_form("s_inf")) for item in nlst):
            pass
        else:
            enter = False
    if b:
        if all(item == tree_form("s_inf") or not contain(item,tree_form("s_inf")) for item in dlst):
            pass
        else:
            enter = False
    if enter:
        if d == 0:
            return None
        if n == 0:
            return tree_form("d_0")
        if not a and not b:
            return eq
        if not a and b:
            return tree_form("d_0")
        if not b and a:
            if compute(d) > 0:
                return n
            else:
                return -n
        if a and b:
            return None
    return eq
def fxinf(eq):
    if eq is None:
        return None
    lst = factor_generation(eq)
    sign = 1
    inf = (tree_form("s_inf") in lst)
    inf_inv = (tree_form("s_inf")**-1 in lst)
    for i in range(len(lst)-1,-1,-1):
        if lst[i] == 0:
            return tree_form("d_0")
        if not contain(lst[i], tree_form("s_inf")) and (inf or inf_inv):
            lst[i] = simplify(lst[i])
            if lst[i] == 0:
                return tree_form("d_0")
            if compute(lst[i])<0:
                sign *= -1
            lst.pop(i)
        elif lst[i] == tree_form("s_inf"):
            inf = True
            lst.pop(i)
        elif lst[i] == tree_form("s_inf")**-1:
            inf_inv = True
            lst.pop(i)
        elif lst[i] == 1:
            lst.pop(i)
    if sign == -1:
        lst.append(tree_form("d_-1"))
    if inf:
        lst.append(tree_form("s_inf"))
    if inf_inv:
        lst.append(tree_form("s_inf")**-1)
    return product(lst)
def fxinf5(eq, parent=None):
    eq = fxinf(eq)
    eq = fxinf2(eq)
    if parent != "f_mul":
        eq = fxinf3(eq)
    return TreeNode(eq.name, [fxinf5(child, eq.name) for child in eq.children])
def limit4(equation):
    if equation.name == "f_limitpinf":
        if not contain(equation, equation.children[1]):
            return equation.children[0]
        eq = equation.children[0]
        n, d = num_dem(eq)
        n, d = simplify(n), simplify(d)
        v2 = tree_form(vlist(eq)[0])
        p1 = poly(n, v2.name)
        p2 = poly(d, v2.name)
        if p1 is not None and p2 is not None and len(p1)<=len(p2) and len(p1)>1 and len(p2)>1:
            v = simplify(v2**(len(p2)-1))
            return TreeNode("f_limitpinf", [simplify(expand(n/v)/expand(d/v)), equation.children[1]])
    return equation
def limit5(eq):
    if eq.name == "f_limit" and len(eq.children) == 3:
        return TreeNode("f_limit", [replace(eq.children[0], eq.children[1], eq.children[1]+eq.children[2]), eq.children[1]])
    return TreeNode(eq.name, [limit5(child) for child in eq.children])
def limit3(eq):
    if eq.name == "f_limitpinf":
        if not contain(eq, eq.children[1]):
            return eq.children[0]
        eq2 = replace(eq.children[0], eq.children[1], tree_form("s_inf"))
        eq2 = dowhile(eq2, lambda x: fxinf5(x, x.name))
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
