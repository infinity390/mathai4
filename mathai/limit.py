from .structure import structure
from .base import *
from .parser import parse
from .simplify import simplify, solve
from .expand import expand
from .diff import diff
from .trig import trig0
from .fraction import fraction
from .printeq import printeq_str
tab=0
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
    
    n, d = (dowhile(replace(e, var, tree_form("d_0")), lambda x: trig0(simplify(x))) for e in (num, den))
    if n is None or d is None:
        return False
    if n == 0 and d == 0: return True
    if d != 0: return simplify(n/d)
    return False
def lhospital(num, den, steps,var):
    logs = []
    out = check(num, den, var)
    if isinstance(out, TreeNode):
        return out
    for _ in range(steps):
        num2, den2 = map(lambda e: simplify(diff(e, var.name)), (num, den))
        out = check(num2, den2, var)
        if out is True:
            num, den = num2, den2
            logs += [(0,"lim x->0 "+printeq_str(simplify(num/den)))]
            continue
        if out is False:
            eq2 = simplify(fraction(simplify(num/den)))
            return eq2,logs
        return out,logs
def lhospital2(eq, var):
    eq=  simplify(eq)
    if eq is None:
        return None
    if not contain(eq, var):
        return eq,[]
    num, dem = [simplify(item) for item in num_dem(eq)]
    if num is None or dem is None:
        return eq,[]
    
    return lhospital(num, dem, 10,var)
ls = [parse("sin(A)"), parse("A^B-1"),parse("log(1+A)"), parse("cos(A)")]
ls= [simplify(item) for item in ls]

def approx(eq, var):
    n, d= num_dem(eq)
    n, d = solve(n), solve(d)
    n, d = expand(n), expand(d)
    out = []
    for equation in [n, d]:
        for item in factor_generation(equation):
            tmp = structure(item, ls[0])
            if tmp is not None and contain(tmp["v_-1"], var):
                item2 = substitute_val(tmp["v_-1"], 0, var.name)
                if tree_form("d_0") == expand(simplify(item2)):
                    equation = equation/item
                    equation = equation*tmp["v_-1"]
                    break
                elif tree_form("d_0") == expand(simplify(tree_form("s_pi") - item2)):
                    equation = equation/item
                    equation = equation*(tree_form("s_pi") - tmp["v_-1"])
                    break
            tmp = structure(item, ls[1])
            if tmp is not None and contain(tmp["v_-1"], var) and not contain(tmp["v_-2"], var):
                item2 = substitute_val(tmp["v_-1"], 0, var.name)
                item2 = expand(solve(item2))
                if tree_form("d_0") == item2:
                    equation = equation/item
                    equation = solve(equation*tmp["v_-1"]*tmp["v_-2"].fx("log"))
                    break
            tmp = structure(item, ls[2])
            if tmp is not None and contain(tmp["v_-1"], var):
                                
                item2 = substitute_val(tmp["v_-1"], 0, var.name)
                item2 = expand(solve(item2))
                if tree_form("d_0") == item2:
                    equation = equation/item
                    equation = solve(equation*tmp["v_-1"])
                    break
            tmp = structure(item, ls[3])
            if tmp is not None and contain(tmp["v_-1"], var):
                item2 = substitute_val(item, 0, var.name)
                
                if tree_form("d_0") == expand(solve(item2)):
                    
                    equation = equation/item
                    equation = equation*(tree_form("d_1") - tmp["v_-1"]**tree_form("d_2"))
                    break
                
        equation = solve(equation)
        out.append(equation)
    return simplify(out[0]/out[1])
def approx_limit(equation, var):
    return dowhile(equation, lambda x: approx(x, var))

def limit(equation, var=tree_form("v_0")):
    logs = [(0,"lim x->0 "+printeq_str(simplify(equation)))]
    eq2 = dowhile(replace(equation, var, tree_form("d_0")), lambda x: trig0(simplify(x)))
    if eq2 is not None and not contain(equation, var):
        return eq2,logs
    equation, tmp = lhospital2(equation, var)
    equation = simplify(expand(simplify(equation)))
    if not contain(equation, var):
        return equation,logs+tmp
    '''
    if equation.name == "f_add":
        return simplify(summation([limit(child, var) for child in equation.children]))
    '''
    return equation,logs+tmp
