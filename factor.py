from parser import parse
from structure import transform_formula
from base import *
from simplify import simplify,solve
from expand import expand
import math
def _factorconst(eq):
    def hcf_list(numbers):
        if not numbers:
            return None  # empty list
        hcf = numbers[0]
        for num in numbers[1:]:
            hcf = math.gcd(hcf, num)
        return hcf
    def extractnum(eq):
        lst = factor_generation(eq)
        for item in lst:
            if item.name[:2] == "d_":
                return int(item.name[2:])
        return 1
    n = 1
    if eq.name == "f_add":
        n = hcf_list([extractnum(child) for child in eq.children])
        eq = TreeNode(eq.name, [child/tree_form("d_"+str(n)) for child in eq.children])
    if n != 1:
        return tree_form("d_"+str(n))*eq
    return TreeNode(eq.name, [factorconst(child) for child in eq.children])
def factorconst(eq):
    return simplify(_factorconst(eq))
def factor_quad_formula_init():
    var = "x"
    formula_list = [(f"(A*D^2+B*D+C)", f"A*(D-(-B+(B^2-4*A*C)^(1/2))/(2*A))*(D-(-B-(B^2-4*A*C)^(1/2))/(2*A))")]
    formula_list = [[simplify(parse(y)) for y in x] for x in formula_list]
    expr = [[parse("A"), parse("1")], [parse("B"), parse("0"), parse("1")], [parse("C"), parse("0")]]
    return [formula_list, var, expr]

def factor_cube_formula_init():
    var = "x"
    formula_list = [(f"D^3+E", f"(D+E^(1/3))*(D^2-D*E^(1/3)+E^(2/3))"), (f"D^3-E", f"(D-E^(1/3))*(D^2+D*E^(1/3)+E^(2/3))")]
    formula_list = [[simplify(parse(y)) for y in x] for x in formula_list]
    expr = [[parse("A")], [parse("B")]]
    return [formula_list, var, expr]
formula_gen2 = factor_quad_formula_init()
formula_gen3 = factor_cube_formula_init()
def factor_helper(equation, complexnum, power=2):
    global formula_gen2, formula_gen3
    maxnum = 1
    def high(eq):
        nonlocal maxnum
        if eq.name == "f_pow" and eq.children[1].name[:2] == "d_":
            n = int(eq.children[1].name[2:])
            if n>power and n % power == 0:
                 maxnum = max(maxnum, n)
        for child in eq.children:
            high(child)
    def helper(eq):
        nonlocal maxnum
        if eq.name == "f_pow" and eq.children[1].name[:2] == "d_":
            n = int(eq.children[1].name[2:])
            if n>power and n % power == 0 and maxnum==n:
                return (eq.children[0]**tree_form("d_"+str(int(n/power))))**power
        return TreeNode(eq.name, [helper(child) for child in eq.children])
    high(equation)
    out = None
    if power == 2:
        out = transform_formula(helper(equation), "v_0", formula_gen2[0], formula_gen2[1], formula_gen2[2])
    elif power == 3:
        out = transform_formula(helper(equation), "v_0", formula_gen3[0], formula_gen3[1], formula_gen3[2])
    if out is not None:
        out = simplify(solve(out))
    if out is not None and (complexnum or (not complexnum and not contain(out, tree_form("s_i")))):
        return out
    else:
        return TreeNode(equation.name, [factor_helper(child, complexnum, power) for child in equation.children])
def factor(equation, complexnum=False):
    return solve(factor_helper(simplify(solve(factor_helper(simplify(equation), complexnum, 2))), complexnum, 3))
