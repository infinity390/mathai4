from .inverse import inverse
from .integrate import integrate_full
from .ode import diffsolve as ode_solve
from .parser import parse
from .simplify import simplify, log0
from .base import *
from .diff import diff
from .trig import trig0, trig1
from .univariate_inequality import wavycurvy, prepare, absolute, range2eq, handle_sqrt
from .bivariate_inequality import solve_logically
from .fraction import fraction
from .expand import expand
from .logic import logic0, set_sub, truth_gen
from .factor import factor2, factor
from .limit import limit1, limit5
from .linear import linear_solve
def god(string):
    print(f"? {string}")
    print("thinking...")
    eq = parse(string)
    log = [eq]
    if "f_limit" in str_form(eq):
        eq = limit1(limit5(eq))
    elif all("f_"+item not in str_form(eq) for item in "dif add mul abs pow dif integrate arcsin sin cos log limit".split(" ")) and\
       any("f_"+item in str_form(eq) for item in "and or not".split(" ")):
        eq = solve_logically(truth_gen(simplify(set_sub(eq))))
    elif any("f_"+item in str_form(eq) for item in "eq lt le ge gt".split(" ")) and all("f_"+item not in str_form(eq) for item in "limit dif integrate".split(" ")):
        lst = [simplify, log0, simplify, lambda x: dowhile(x, absolute), lambda x: dowhile(x, lambda y: simplify(fraction(y))), handle_sqrt,
               prepare, factor2, lambda x: simplify(x, True, True), logic0, wavycurvy, wavycurvy]
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
            eq = simplify(ode_solve(simplify(eq)))
            log.append(eq)
        if "f_integrate" in str_form(eq):
            eq = integrate_full(eq)
    if flatten_tree(eq).name == "f_and" and all(item.name == "f_eq" for item in flatten_tree(eq).children):
        out = vlist(eq.children[0])
        eq = simplify(eq)
        for item in eq.children:
            out = list(set(out) & set(vlist(item)))
        print(eq)
        if all(all("v_" not in str_form(diff(item.children[0],v)) for v in out) for item in eq.children):
            out = list(set(vlist(eq))-set(out))
            eq = linear_solve(eq, [tree_form(item) for item in out])
            eq = wavycurvy(eq)
            print(eq)
        elif len(eq.children) == 2 and len(vlist(eq)) == 2:
            a, b = copy.deepcopy(eq.children)
            a, b = a.children[0], b.children[0]
            var = None
            index = None
            tmp = None
            for v in vlist(eq):
                for i in range(2):
                    tmp = inverse(copy.deepcopy([a,b][i]), v)
                    if tmp is not None:
                        var = v
                        index = i
            if index is not None:
                tmp2 = TreeNode("f_eq", [[a,b][index], tree_form("d_0")])
                eq = TreeNode("f_eq", [replace([a,b][1-index], tree_form(var), tmp), tree_form("d_0")]) 
                eq = simplify(factor2(prepare(simplify(fraction(simplify(eq))))), True, True)
                eq2 = None
                if eq.name == "f_or":
                    eq2 = eq.children[0] & tmp2
                    for item in eq.children[1:]:
                        eq2 = eq2 | (item & tmp2)
                if eq2 is not None:
                    eq = eq2
                    print(eq)
                    eq = wavycurvy(eq)
                else:
                    print(eq)
    if isinstance(eq, TreeNode):
        eq = simplify(expand(simplify(fraction(simplify(eq)))))
    print(f"=> {eq}")
    print()
    return eq
