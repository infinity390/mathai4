import itertools
from .simplify import simplify
from .base import *
from .expand import expand
from .structure import transform_formula
from .parser import parse
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
    if eq is None:
        return None
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
        eq = simplify(product(lst)/tree_form("s_pi"))
        out = frac(eq)
        if out is None or out < 0:
            return None
        a,b= out.numerator, out.denominator
        a %= 2*b
        if a > b:       
            a = 2*b - a
        return a, b
    if eq.name == "f_arccosec":
        return (1/eq.children[0]).fx("arcsin")
    
    if eq.name == "f_arctan":
        if eq.children[0].name == "d_0":
            return tree_form("d_0")
    if eq.name == "f_log":
        if eq.children[0].name == "d_1":
            return tree_form("d_0")
    if eq.name=="f_tan":
        if eq.children[0].name == "f_arctan":
            return eq.children[0].children[0]
        return eq.children[0].fx("sin")/eq.children[0].fx("cos")
    if eq.name == "f_sec":
        return eq.children[0].fx("cos")**-1
    if eq.name == "f_cosec":
        return eq.children[0].fx("sin")**-1
    if eq.name == "f_cot":
        return eq.children[0].fx("cos")/eq.children[0].fx("sin")
    
    if eq.name == "f_sin":
        if eq.children[0].name == "f_arcsin":
            return eq.children[0].children[0]
        lst = factor_generation(eq.children[0])
        if any(isneg(item) for item in lst):
            return -(eq.children[0]*-1).fx("sin")
        out=single_pi(lst)
        if out is not None and tuple(out) in trig_sin_table.keys():
            return trig_sin_table[tuple(out)]
    
    if eq.name == "f_cos":
        if eq.children[0].name == "f_arccos":
            return eq.children[0].children[0]
        lst = factor_generation(eq.children[0])
        if any(isneg(item) for item in lst):
            return (eq.children[0]*-1).fx("cos")
        out=single_pi(lst)
        if out is not None:
            if tuple(out) in trig_cos_table.keys():
                return trig_cos_table[tuple(out)]
    return TreeNode(eq.name, [trig0(child) for child in eq.children])
def cog(expr):
    expr = TreeNode(expr.name, [product_to_sum(child) for child in expr.children])
    expr = trig0(simplify(expr))
    expr = expand(simplify(expr))
    return expr
def product_to_sum(expr):
    factors = factor_generation(expr)
    other = []
    lst = []
    for item in factors:
        if item.name in ["f_cos", "f_sin"]:
            lst.append(item)
        else:
            other.append(item)
    if len(lst) <= 1:

        return dowhile(expr, cog)
    if len(lst) == 2:
        a, b = lst
        out = None
        if a.name < b.name:
            a, b = b, a
        A, B = a.children[0], b.children[0]
        if a.name == "f_sin" and b.name == "f_sin":
            out =((A - B).fx("cos") - (A + B).fx("cos")) / tree_form("d_2")
        elif a.name == "f_cos" and b.name == "f_cos":
            out =((A - B).fx("cos") + (A + B).fx("cos")) / tree_form("d_2")
        elif a.name == "f_sin" and b.name == "f_cos":
            out =((A + B).fx("sin") + (A - B).fx("sin")) / tree_form("d_2")
            
        return out * product(other)

    rest = tree_form("d_1")
    if len(lst) % 2 == 1:
        rest = lst.pop(0)
    out = []
    for i in range(0, len(lst), 2):
        out.append(product_to_sum(product(lst[i:i+2])))
    expr = product(out)*rest*product(other)
    
    return dowhile(expr, cog)

def trig_formula_init():
    var = ""
    formula_list = [(f"A*sin(B)+C*sin(B)", f"(A^2+C^2)^(1/2)*sin(B+arctan(C/A))"),\
                    (f"sin(B+D)", f"sin(B)*cos(D)+cos(B)*sin(D)"),\
                    (f"cos(B+D)", f"cos(B)*cos(D)-sin(B)*sin(D)"),\
                    (f"cos(B)^2", f"1-sin(B)^2"),\
                    (f"1/cos(B)^2", f"1/(1-sin(B)^2)"),\
                    (f"cos(arcsin(B))", f"sqrt(1-B^2)"),\
                    (f"sin(arccos(B))", f"sqrt(1-B^2)"),\
                    (f"arccos(B)", f"pi/2-arcsin(B)"),\
                    (f"sin(arctan(B))", f"x/sqrt(1+x^2)"),\
                    (f"cos(arctan(B))", f"1/sqrt(1+x^2)")]
    formula_list = [[simplify(parse(y)) for y in x] for x in formula_list]
    expr = [[parse("A"), parse("1")], [parse("B")], [parse("C"), parse("1")], [parse("D")]]
    return [formula_list, var, expr]
#formula_gen4 = trig_formula_init()
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
def noneg_pow(eq):
    if eq.name == "f_pow" and frac(eq.children[1]) is not None and frac(eq.children[1])<0:
        return (eq.children[0]**(simplify(-eq.children[1])))**-1
    return TreeNode(eq.name, [noneg_pow(child) for child in eq.children])
    
def trig1(eq):
    eq = noneg_pow(eq)
    return product_to_sum(eq)

def trig4(eq):
    done = False
    def _trig4(eq, numer=True, chance="sin"):
        nonlocal done
        if eq.name == "f_sin":
            if eq.children[0].name == "f_add" and len(eq.children[0].children)>=2:
                r = len(eq.children[0].children)%2
                a, b = TreeNode("f_add", eq.children[0].children[:round((len(eq.children[0].children)-r)/2)]),\
                       TreeNode("f_add", eq.children[0].children[round((len(eq.children[0].children)-r)/2):])
                if len(a.children)==1:
                    a=a.children[0]
                if len(b.children)==1:
                    b=b.children[0]
                return a.fx("sin")*b.fx("cos") + a.fx("cos")*b.fx("sin")
            if eq.children[0].name == "f_arccos":
                a = eq.children[0].children[0]
                return (1-a**2)**(tree_form("d_2")**-1)
            if eq.children[0].name == "f_arctan":
                a = eq.children[0].children[0]
                return a/(1+a**2)**(tree_form("d_2")**-1)
        if eq.name == "f_pow" and numer:
            if eq.children[0].name == "f_cos" and chance == "cos":
                a = eq.children[0].children[0]
                if frac(eq.children[1]) == 2:
                    done = True
                    return 1 - a.fx("sin")**2
            if eq.children[0].name == "f_sin" and chance == "cos":
                a = eq.children[0].children[0]
                if frac(eq.children[1]) == 2:
                    done = True
                    return 1 - a.fx("cos")**2
        if eq.name == "f_cos":
            if eq.children[0].name == "f_add" and len(eq.children[0].children)>=2:
                r = len(eq.children[0].children)%2
                a, b = TreeNode("f_add", eq.children[0].children[:round((len(eq.children[0].children)-r)/2)]),\
                       TreeNode("f_add", eq.children[0].children[round((len(eq.children[0].children)-r)/2):])
                if len(a.children)==1:
                    a=a.children[0]
                if len(b.children)==1:
                    b=b.children[0]
                return a.fx("cos")*b.fx("cos") - a.fx("sin")*b.fx("sin")
            if eq.children[0].name == "f_arcsin":
                a = eq.children[0].children[0]
                return (1-a**2)**(tree_form("d_2")**-1)
            if eq.children[0].name == "f_arctan":
                a = eq.children[0].children[0]
                return tree_form("d_1")/(1+a**2)**(tree_form("d_2")**-1)
        
        return TreeNode(eq.name, [_trig4(child, False, chance) if eq.name != "f_add" and\
                                  (not numer or (eq.name == "f_pow" and frac(eq.children[1]) is not None and frac(eq.children[1]) < 0))\
                                   else _trig4(child, True, chance) for child in eq.children])
    eq= _trig4(eq)
    if not done:
        eq = _trig4(eq,"cos")
    return eq
def trig2(eq):
    # Base case: if not an addition, recurse into children
    if eq.name != "f_add":
        return TreeNode(eq.name, [trig2(child) for child in eq.children])

    # Try all pairs in the addition
    for i, j in itertools.combinations(range(len(eq.children)), 2):
        c1, c2 = eq.children[i], eq.children[j]

        # Combine only sin/sin or cos/cos
        if c1.name in ["f_sin", "f_cos"] and c2.name in ["f_sin", "f_cos"]:
            A, B = c1.children[0], c2.children[0]
            rest = [eq.children[k] for k in range(len(eq.children)) if k not in (i, j)]
            rest_tree = summation(rest) if rest else tree_form("d_0")

            two = tree_form("d_2")

            # sinA + sinB
            if c1.name == "f_sin" and c2.name == "f_sin":
                combined = two * ((A + B) / two).fx("sin") * ((A - B) / two).fx("cos")

            # cosA + cosB
            elif c1.name == "f_cos" and c2.name == "f_cos":
                combined = two * ((A + B) / two).fx("cos") * ((A - B) / two).fx("cos")

            # sinA + cosB (leave unchanged)
            else:
                continue

            new_expr = rest_tree + combined
            # Re-run trig2 in case there are more sin/cos sums to simplify
            return trig2(new_expr)

    # If no sin/cos pairs found, just recurse on children
    return TreeNode(eq.name, [trig2(child) for child in eq.children])
