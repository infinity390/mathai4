from .inverse import inverse
from .integrate import integrate_full
from .ode import diffsolve as ode_solve
from .parser import parse
from .simplify import simplify, log0
from .base import *
from .diff import diff
from .trig import trig0, trig1
from .univariate_inequality import wavycurvy, prepare, absolute, handle_sqrt, eq2range
from .bivariate_inequality import solve_logically
from .fraction import fraction
from .expand import expand
from .logic import logic0, set_sub, truth_gen
from .factor import factor2, factor
from .limit import limit1, limit5
from .linear import linear_solve
def simple_wavycurvy(eq):
    fx = lambda x: dowhile(x, lambda y: logic0(fraction(simplify(y))))
    eq = fx(eq)
    eq = handle_sqrt(eq)
    eq = fx(eq)
    eq = factor2(eq)
    eq = wavycurvy(eq)
    if eq.name == "f_range":
        tmp = eq2range(eq).truth()
        if tmp == 1:
            return tree_form("s_true")
        if tmp == -1:
            return tree_form("s_false")
    return eq
def two_eq_handle(eq):
    eq = flatten_tree(eq)
    orig = eq
    if eq.name == "f_and" and all(item.name == "f_eq" for item in eq.children):
        out = vlist(eq)
        eq = simplify(eq)
        if out == []:
            pass
        elif all(all("v_" not in str_form(diff(item.children[0],v)) for v in out) for item in eq.children):
            eq = linear_solve(eq)
            return eq
        elif len(eq.children) == 2 and len(out) == 2:
            a, b = copy.deepcopy(eq.children)
            a_expr = a.children[0]
            b_expr = b.children[0]
            result = tree_form("s_false")
            for v1, v2 in [(out[0], out[1]), (out[1], out[0])]:
                inv = inverse(copy.deepcopy(a_expr), v1)  # x = 8 - y
                if inv is not None:
                    substituted = replace(copy.deepcopy(b_expr), tree_form(v1), inv)
                    pair = (
                        TreeNode("f_eq", [tree_form(v1), inv]) &
                        TreeNode("f_eq", [substituted, tree_form("d_0")])
                    )
                    result = result | pair
            print(result)
            return result
    return orig
def god(string):
    print(f"? {string}")
    print("thinking...")
    eq = None
    eq = parse(string)
    log = [eq]
    if "f_limit" in str_form(eq):
        eq = limit1(limit5(eq))
    elif all("f_"+item not in str_form(eq) for item in "dif add mul abs pow dif integrate arcsin sin cos log limit eq lt le ge gt".split(" ")) and\
       any("f_"+item in str_form(eq) for item in "and or not".split(" ")):
        eq = solve_logically(truth_gen(simplify(set_sub(eq))))
    elif any("f_"+item in str_form(eq) for item in "eq lt le ge gt".split(" ")) and all("f_"+item not in str_form(eq) for item in "limit dif integrate".split(" ")):
        lst = [simplify, log0, simplify, lambda x: dowhile(x, absolute), lambda x: dowhile(x, lambda y: simplify(fraction(y))), handle_sqrt,
               prepare, factor2, lambda x: simplify(x, True, True), logic0, wavycurvy, wavycurvy]
        lst2 = [simplify, trig0, lambda x: dowhile(x, lambda y: simplify(expand(simplify(fraction(y))))), trig1, simplify, expand, simplify, logic0]
        sel = lst.copy()
        if any("f_"+item in str_form(eq) for item in "sin cos tan cosec sec cot".split(" ")) or\
           len(vlist(eq)) > 1:
            sel = lst2
        for item in sel:
            eq = item(eq)
            if eq not in log:
                log.append(eq)
                print(eq)
    else:
        if "f_dif" in str_form(eq) and "f_integrate" not in str_form(eq):
            eq = simplify(ode_solve(simplify(eq)))
            log.append(eq)
        if "f_integrate" in str_form(eq):
            eq = integrate_full(eq)
    if any("f_"+item in str_form(eq) for item in "eq lt le ge gt".split(" ")):
        print(eq)
        eq = simple_wavycurvy(eq)
        eq = two_eq_handle(eq)
        eq = simple_wavycurvy(eq)
        eq = simple_wavycurvy(eq)
        if eq.name in ["f_and", "f_or"]:
            eq = TreeNode(eq.name, list(set(eq.children)))
            if len(eq.children) == 1:
                eq = eq.children[0]
    if isinstance(eq, TreeNode):
        eq = simplify(expand(simplify(fraction(simplify(eq)))))
    print(f"=> {eq}")
    print()
    return eq
