from .base import *
from .simplify import solve
from .expand import expand
def inverse(rhs,term, sign=None):
    term = tree_form(term)
    lhs = tree_form("d_0")
    count = 15
    
    while not rhs==term:
        if rhs.name == "f_add":
            if all(term in factor_generation(child) for child in rhs.children):
                newrhs = solve(expand(rhs*term**-1))
                if not contain(newrhs, term):
                    rhs = term * newrhs
            else:
                for i in range(len(rhs.children)-1,-1,-1):
                    if not contain(rhs.children[i], term):
                        lhs = lhs - rhs.children[i]
                        rhs.children.pop(i)
        elif rhs.name == "f_mul":
            for i in range(len(rhs.children)-1,-1,-1):
                if not contain(rhs.children[i], term):
                    lhs = lhs * rhs.children[i]**-1
                    if sign is not None:
                        if "v_" in str_form(rhs.children[i]):
                            return None
                        if compute(rhs.children[i]**-1) < 0:
                            sign = not sign
                    
                    rhs.children.pop(i)
        elif rhs.name == "f_pow" and contain(rhs.children[0], term):
            lhs = lhs ** (tree_form("d_1")/rhs.children[1])
            rhs = copy.deepcopy(rhs.children[0])
        elif rhs.name == "f_sin" and contain(rhs.children[0], term):
            lhs = lhs.fx("arcsin")
            rhs = copy.deepcopy(rhs.children[0])
        elif rhs.name == "f_arcsin" and contain(rhs.children[0], term):
            lhs = lhs.fx("sin")
            rhs = copy.deepcopy(rhs.children[0])
        elif rhs.name == "f_arccos" and contain(rhs.children[0], term):
            lhs = lhs.fx("cos")
            rhs = copy.deepcopy(rhs.children[0])
        elif rhs.name == "f_cos" and contain(rhs.children[0], term):
            lhs = lhs.fx("arccos")
            rhs = copy.deepcopy(rhs.children[0])
        elif rhs.name == "f_log" and contain(rhs.children[0], term):
            lhs = tree_form("s_e")**lhs
            rhs = copy.deepcopy(rhs.children[0])
        elif rhs.name == "f_pow" and rhs.children[0].name == "s_e" and contain(rhs.children[1], term):
            lhs = lhs.fx("log")
            rhs = copy.deepcopy(rhs.children[1].fx("log"))
        elif rhs.name == "f_tan" and contain(rhs.children[0], term):
            lhs = lhs.fx("arctan")
            rhs = copy.deepcopy(rhs.children[0])
        elif rhs.name == "f_arctan" and contain(rhs.children[0], term):
            lhs = lhs.fx("tan")
            rhs = copy.deepcopy(rhs.children[0])
        if len(rhs.children) == 1:
            rhs = rhs.children[0]
        count -= 1
        if count == 0:
            return None
    if sign is None:
        return solve(lhs)
    return solve(lhs), sign
