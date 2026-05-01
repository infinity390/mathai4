from .integrate import integrate_full
from .ode import diffsolve
from .parser import parse
from .simplify import simplify, log0
from .base import *
from .trig import trig0, trig1
from .univariate_inequality import wavycurvy, prepare, absolute, range2eq
from .bivariate_inequality import solve_logically
from .fraction import fraction
from .expand import expand
from .logic import logic0, set_sub, truth_gen
from .factor import factor2, factor
from .limit import limit1, limit5

def god(string):
    print(f"? {string}")
    print("thinking...")
    eq = parse(string)
    log = [eq]
    if "f_limit" in str_form(eq):
        eq = limit1(limit5(eq))
    elif all("f_"+item not in str_form(eq) for item in "add mul abs pow dif integrate arcsin sin cos log limit".split(" ")) and\
       any("f_"+item in str_form(eq) for item in "and or not".split(" ")):
        eq = solve_logically(truth_gen(simplify(set_sub(eq))))
    elif any("f_"+item in str_form(eq) for item in "eq lt le ge gt".split(" ")) and all("f_"+item not in str_form(eq) for item in "limit dif integrate".split(" ")):
        lst = [simplify, log0, simplify, lambda x: dowhile(x, absolute), lambda x: dowhile(x, lambda y: simplify(fraction(y))),\
               factor2, prepare, factor2, logic0, lambda x: wavycurvy(x, tree_form(vlist(x)[0])), wavycurvy]
        lst2 = [simplify, trig0, lambda x: dowhile(x, lambda y: simplify(expand(simplify(fraction(y))))), trig1, simplify, expand, simplify, logic0]
        sel = lst.copy()
        if any("f_"+item in str_form(eq) for item in "sin cos tan cosec sec cot".split(" ")) or\
           len(vlist(eq)) != 1:
            sel = lst2
        for item in sel:
            eq = item(eq)
            if eq not in log:
                log.append(eq)
                print(eq)
    else:
        if "f_dif" in str_form(eq) and "f_integrate" not in str_form(eq):
            eq = simplify(diffsolve(simplify(eq)))
            log.append(eq)
        if "f_integrate" in str_form(eq):
            eq = integrate_full(eq)
    if isinstance(eq, TreeNode):
        eq = simplify(expand(simplify(fraction(simplify(eq)))))
    print(f"=> {eq}")
    print()
    return eq
