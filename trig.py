from simplify import solve, simplify
from base import *
from expand import expand
from structure import transform_formula
from parser import parse
trig_sin_table = {
    (0,1): parse("0"),
    (1,6): parse("1/2"),
    (1,4): parse("2^(1/2)/2"),   # π/4
    (1,3): parse("3^(1/2)/2"),   # π/3
    (1,2): parse("1"),           # π/2
    (2,3): parse("3^(1/2)/2"),   # 2π/3
    (3,4): parse("2^(1/2)/2"),   # 3π/4
    (5,6): parse("1/2"),         # 5π/6
    (1,1): parse("0")            # π
}
trig_cos_table = {
    (0,1): parse("1"),           # 0
    (1,6): parse("3^(1/2)/2"),   # π/6
    (1,4): parse("2^(1/2)/2"),   # π/4
    (1,3): parse("1/2"),         # π/3
    (1,2): parse("0"),           # π/2
    (2,3): parse("-1/2"),        # 2π/3
    (3,4): parse("-2^(1/2)/2"),  # 3π/4
    (5,6): parse("-1/2"),        # 5π/6
    (1,1): parse("-1")           # π
}

for key in trig_cos_table.keys():
    trig_cos_table[key] = simplify(trig_cos_table[key])
for key in trig_sin_table.keys():
    trig_sin_table[key] = simplify(trig_sin_table[key])
def trig0(eq):
    def isneg(eq):
        if eq.name[:2] != "d_":
            return False
        if int(eq.name[2:]) >= 0:
            return False
        return True
    def single_pi(lst):
        if tree_form("d_0") in lst:
            return 0, 1
        count = 0
        for item in lst:
            if item == tree_form("s_pi"):
                count += 1
        if count != 1:
            return None
        eq = solve(product(lst)/tree_form("s_pi"))
        out = frac(eq)
        if out is None or out < 0:
            return None
        a,b= out.numerator, out.denominator
        a %= 2*b
        if a > b:       
            a = 2*b - a
        return a, b
    if eq.name == "f_arctan":
        if eq.children[0].name == "d_0":
            return tree_form("d_0")
    if eq.name == "f_log":
        if eq.children[0].name == "d_1":
            return tree_form("d_0")
    if eq.name=="f_tan":
        return eq.children[0].fx("sin")/eq.children[0].fx("cos")
    if eq.name == "f_sec":
        return eq.children[0].fx("cos")**-1
    if eq.name == "f_cosec":
        return eq.children[0].fx("sin")**-1
    if eq.name == "f_cot":
        return eq.children[0].fx("cos")/eq.children[0].fx("sin")
    if eq.name == "f_sin":
        lst = factor_generation(eq.children[0])
        if any(isneg(item) for item in lst):
            return -(eq.children[0]*-1).fx("sin")
        out=single_pi(lst)
        if out is not None:
            return trig_sin_table[tuple(out)]
    if eq.name == "f_cos":
        lst = factor_generation(eq.children[0])
        if any(isneg(item) for item in lst):
            return (eq.children[0]*-1).fx("cos")
        out=single_pi(lst)
        if out is not None:
            if tuple(out) in trig_cos_table.keys():
                return trig_cos_table[tuple(out)]
    return TreeNode(eq.name, [trig0(child) for child in eq.children])

def product_to_sum(eq):
    lst = factor_generation(eq)
    if len(lst) == 1:
        return lst[0]
    if len(lst) == 2:
        a, b = lst
        if a.name == "f_sin" and b.name == "f_sin":
            return ((a.children[0] - b.children[0]).fx("cos") - (a.children[0] + b.children[0]).fx("cos")) / tree_form("d_2")
        elif a.name == "f_cos" and b.name == "f_cos":
            return ((a.children[0] - b.children[0]).fx("cos") + (a.children[0] + b.children[0]).fx("cos")) / tree_form("d_2")
        elif a.name == "f_sin" and b.name == "f_cos":
            return ((a.children[0] + b.children[0]).fx("sin") + (a.children[0] - b.children[0]).fx("sin")) / tree_form("d_2")
        elif a.name == "f_cos" and b.name == "f_sin":
            return ((a.children[0] + b.children[0]).fx("sin") - (a.children[0] - b.children[0]).fx("sin")) / tree_form("d_2")
    first, rest = lst[0], lst[1:]
    s = tree_form("d_0")
    eq = expand(simplify(first * product_to_sum(solve(TreeNode("f_mul", rest)))))
    if eq.name == "f_add":
        for child in eq.children:
            s += product_to_sum(child)
            s = simplify(s)
    else:
        s = eq
    return s
def trig_formula_init():
    var = "x"
    formula_list = [(f"A*sin(B)+C*sin(B)", f"(A^2+C^2)^(1/2)*sin(B+arctan(C/A))"),\
                    (f"sin(B+D)", f"sin(B)*cos(D)+cos(B)*sin(D)"),\
                    (f"cos(B+D)", f"cos(B)*cos(D)-sin(B)*sin(D)")]
    formula_list = [[parse(y) for y in x] for x in formula_list]
    expr = [[parse("A"), parse("1")], [parse("B")], [parse("C"), parse("1")], [parse("D")]]
    return [formula_list, var, expr]
formula_gen4 = trig_formula_init()
def trig3(eq):
    def iseven(eq):
        if eq.name[:2] != "d_":
            return False
        if int(eq.name[2:]) < 2 or int(eq.name[2:]) % 2 != 0:
            return False
        return True
    
    if eq.name == "f_sin":
        lst = factor_generation(eq.children[0])
        if any(iseven(item) for item in lst):
            eq= 2*(eq.children[0]/2).fx("sin")*(eq.children[0]/2).fx("cos")
    if eq.name == "f_cos":
        lst = factor_generation(eq.children[0])
        if any(iseven(item) for item in lst):
            eq = (eq.children[0]/2).fx("cos")**2-(eq.children[0]/2).fx("sin")**2
    eq = expand(simplify(eq))
    return TreeNode(eq.name, [trig3(child) for child in eq.children])
def trig1(equation):
    equation = product_to_sum(equation)
    return TreeNode(equation.name, [trig1(child) for child in equation.children])
def trig4(eq):
    out = transform_formula(eq, "v_0", formula_gen4[0], formula_gen4[1], formula_gen4[2])
    if out is not None:
        return out
    return TreeNode(eq.name, [trig4(child) for child in eq.children])
def trig2(eq):
    if eq.name == "f_add":
        for item in itertools.combinations(range(len(eq.children)), 2):
            if all(eq.children[item2].name == "f_sin" for item2 in item):
                a, b = eq.children[item[0]].children[0], eq.children[item[1]].children[0]
                rest = [item2 for index, item2 in enumerate(eq.children) if index not in item]
                if len(rest)==0:
                    rest = tree_form("d_0")
                else:
                    rest = summation(rest)
                two = tree_form("d_2")
                return rest + two*((a+b)/two).fx("sin")*((a-b)/two).fx("cos")
    return TreeNode(eq.name, [trig2(child) for child in eq.children])
