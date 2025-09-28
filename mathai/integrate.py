from .parser import parse
import itertools
from .diff import diff
from .fraction import fraction
from .simplify import solve, simplify
from .expand import expand
from .base import *
from .printeq import printeq_str
from .structure import transform_formula
from .inverse import inverse
from .tool import poly
from fractions import Fraction
from .printeq import printeq
def integrate_summation(equation, wrt, tab, inf):
    logs= []
    orig = copy.deepcopy(equation)
    for i in range(2):
        
        if equation.name == "f_add":
            logs += [(tab, f"by integration over sums {', '.join([printeq_str(simplify(child)) for child in equation.children])}")]
            answer = []
            for child in equation.children:
                out = integrate(child, wrt, tab+1, inf)
                if out is None:
                    return None
                logs += out[1]
                answer.append(out[0])
            return summation(answer), logs
        if i == 0:
            
            tmp = expand(simplify(fraction(simplify(equation))))
            
            logs += [(tab, f"integrating {printeq_str(simplify(equation))} will be the same thing as integrating {printeq_str(simplify(tmp))}")]
            
            equation = tmp
            if equation.name != "f_add" and orig != equation:
                out = integrate(equation, wrt, tab+1, inf)
                if out is None:
                    return None
                return out[0], logs+out[1]
    return None

def subs_heuristic(eq, var):
    output = []
    def collect2(eq):
        if eq.name == "f_pow" and frac(eq.children[1]) is not None and abs(frac(eq.children[1])) == Fraction(1,2):
            output.append(str_form(eq.children[0].fx("sqrt")))
        if eq.name in ["f_pow", "f_sin", "f_cos", "f_arcsin"] and eq.children[0].name[:2] != "v_" and var in str_form(eq.children[0]):
            output.append(str_form(eq.children[0]))
        if eq.name == "f_pow" and eq.children[0].name == "s_e" and "v_" in str_form(eq):
            if eq.children[1].name[:2] != "v_":
                output.append(str_form(eq.children[1]))
            output.append(str_form(eq))
        
        for child in eq.children:
            collect2(child)
    def collect3(eq):
        if eq.name in ["f_sin", "f_cos"]:
            output.append(str_form(eq.children[0].fx("cos")))
        for child in eq.children:
            collect3(child)  
    collect2(eq)
    if output == []:
        collect3(eq)
    tmp = sorted(output, key=lambda x: len(x))
    tmp = [simplify(tree_form(x)) for x in tmp]
    
    return tmp

def integrate_subs(equation, term, v1, v2, tab, inf):
    
    origv2 = copy.deepcopy(v2)
    equation = solve(equation)
    eq = equation
    termeq = term
    t = inverse(copy.deepcopy(termeq), v1)
    g = inverse(termeq, v2)
    
    if g is None:
        return None
    if t is None:
        return None
    else:
        t = expand(t)
        eq = replace(eq, tree_form(v1), t)
               
        eq2 = replace(diff(g, v1), tree_form(v1), t)
        equation = eq/eq2
        equation = solve(equation)
        
    lst = [ equation]
    for eq in lst:
        if v1 in str_form(eq):
            continue
        
        eq = expand(simplify(eq))
        
        out = integrate(eq, origv2, tab+1, inf)
       
        if out is None:
            continue
        tmp, logs = out
        tmp = replace(tmp, tree_form(v2), g)
        return tmp, [(tab, f"substituted {str(tree_form(origv2))}={printeq_str(simplify(g))}, integrating {printeq_str(simplify(eq))} wrt {str(tree_form(origv2))}")]+logs+\
               [(tab, f"substituting back to {printeq_str(simplify(out[0]))} which is the result after integration")]
    return None

def integrate_subs_main(equation, wrt, tab, inf):
    v2 = "v_"+str(int(wrt[2:])+1)
    for item in subs_heuristic(equation, wrt):
        x = tree_form(v2)-item
        
        tmp3 = integrate_subs(equation, x, wrt, v2, tab, inf)
        
        if tmp3 is not None:
            return tmp3[0], tmp3[1]
    return None
def sqint(equation, var="v_0", depth=0, inf=0):
    
    logs = []
    def sgn(eq):
        if compute(eq) <0:
            return tree_form("d_-1"), tree_form("d_-1")*eq
        return tree_form("d_1"), eq
    one = tree_form("d_1")
    two = tree_form("d_2")
    four = tree_form("d_4")
    three = tree_form("d_3")
    root = tree_form("d_2")**-1
    zero = tree_form("d_0")
    
    n, d = num_dem(equation)
    n, d = simplify(n), simplify(d)
    term = [simplify(x) for x in factor_generation(d)]
    const = product([item for item in term if "v_" not in str_form(item)])
    term = [item for item in term if "v_" in str_form(item)]
    mode = False
    if all(item.name == "f_pow" and simplify(item.children[1]-root) == zero for item in term):
        d = simplify(expand(const**two*product([item.children[0] for item in term])))
    else:
        mode = True
        if any(item.name == "f_pow" and simplify(item.children[1]-root) == zero for item in term):
            return None
    v = vlist(equation)[0]
    x = tree_form(v)
    
    np = poly(n, v)
    
    dp = poly(d, v)
    
    if np is None or dp is None:
        return None
    
    if len(np) == 1 and len(dp) == 3:
        k, a, b, c = np+dp
        if a == zero:
            return None
        s1, s2 = sgn(a)
        const = (four*a*c - b**two)/(four*a)
        t1, t2 = sgn(const)
        la = s2**root
        lb = b*s2**root/(two*a)
        
        if mode:
            if s1 == one:
                if t1 == one:
                    return k*((la*x+lb)/t2**root).fx("arctan")/(la * t2**root), logs
                else:
                    return None
            else:
                if t1 == one:
                    return None
                else:
                    _, t2 = sgn(-const)
                    return -k*((la*x+lb)/t2**root).fx("arctan")/(la * t2**root), logs
        if s1 == one:
            if t1 == one:
                return simplify(k*(la*x + lb + ((la*x + lb)**two + t2)**root).fx("abs").fx("log")/la), logs
            else:
                return simplify(k*(la*x + lb + ((la*x + lb)**two - t2)**root).fx("abs").fx("log")/la), logs
                
        else:
            if t1 == one:
                return k*((la*x + lb)/t2**root).fx("arcsin")/la, logs
            else:
                return None
    if len(np) == 2 and len(dp) == 3:
        
        p, q, a, b, c = np+dp
        if a == zero:
            return None
        A = p/(two*a)
        B = q - A*b
        t = a*x**two + b*x + c
        
        if not mode:
            tmp = sqint(simplify(one/t**root), var, depth, inf)
            if tmp is None:
                tmp = integrate(simplify(one/t**root), var, depth, inf)
                if tmp is None:
                    return None
                log2 = tmp[1]
                tmp = tmp[0]
                
            else:
                log2 = tmp[1]
                tmp = tmp[0]
            return A*two*t**root + tmp*B, logs+log2
        else:
            tmp = sqint(simplify(one/t), var, depth, inf)
            if tmp is None:
                tmp = integrate(simplify(one/t), var, depth, inf)
                
                if tmp is None:
                    return None
                log2 = tmp[1]
                tmp = tmp[0]
                
            else:
                log2 = tmp[1]
                tmp = tmp[0]
            return A*t.fx("abs").fx("log") + tmp*B, logs+log2
    return None
typeint = "integrate"
def typeintegrate():
    global typeint
    typeint=  "integrate"
def typesqint():
    global typeint
    typeint=  "sqint"
def typebyparts():
    global typeint
    typeint=  "byparts"
def byparts(eq, wrt="v_0", tab=0, inf=0):
    
    lst = factor_generation(eq)
    if len(lst) == 1:
        lst += [tree_form("d_1")]
    if len(lst) == 2:
        for i in range(2):
            
            f, g = copy.deepcopy([lst[i], lst[1-i]])
            
            logs = [(tab, f"trying integration by parts, f = {printeq_str(simplify(f))} and g = {printeq_str(simplify(g))}")]
            typeintegrate()
            out1 = integrate(copy.deepcopy(g), wrt, tab+1, inf)
            typebyparts()
            
            if out1 is None:
                continue
            
            typeintegrate()
            out2 = integrate(simplify(diff(copy.deepcopy(f), wrt)*out1[0]), wrt, tab+1, inf)
            
            typebyparts()
            if out2 is None:
                continue
            
            return copy.deepcopy([simplify(copy.deepcopy(f) * out1[0] - out2[0]), logs+out1[1]+out2[1]])
    return None
def integration_formula_init():
    var = "x"
    formula_list = [(f"(A*{var}+B)^C", f"(A*{var}+B)^(C+1)/(A*(C+1))"),\
                    (f"sin(A*{var}+B)", f"-cos(A*{var}+B)/A"),\
                    (f"cos(A*{var}+B)", f"sin(A*{var}+B)/A"),\
                    (f"1/(A*{var}+B)", f"log(abs(A*{var}+B))/A"),\
                    (f"e^(A*{var}+B)", f"e^(A*{var}+B)/A"),\
                    (f"abs(A*{var}+B)", f"(A*{var}+B)*abs(A*{var}+B)/(2*A)")]
    formula_list = [[simplify(parse(y)) for y in x] for x in formula_list]
    expr = [[parse("A"), parse("1")], [parse("B"), parse("0")]]
    return [formula_list, var, expr]
formula_gen = integration_formula_init()


def integrate(equation, wrt="v_0", tab=0, inf=0):
    global formula_list, var, expr
    global typeint

    equation = simplify(equation)
    
    logs = []
    if tab == 0:
        logs += [(tab, f"the given question is to integrate {printeq_str(simplify(equation))} wrt to {str(tree_form(wrt))}")]
        
    if equation == tree_form(wrt):
        return equation**2/2,[]
    if not contain(equation,tree_form(wrt)):
        return tree_form(wrt)*equation,logs
    out = transform_formula(equation, wrt, formula_gen[0], formula_gen[1], formula_gen[2])
    if out is not None:
        return out, logs
    lst = factor_generation(equation)
    
    lst_const = [item for item in lst if not contain(item, tree_form(wrt))]
    if lst_const != []:
        equation = product([item for item in lst if contain(item, tree_form(wrt))])
        const = product(lst_const)
        const = simplify(const)
        if const != 1 and not contain(const, tree_form("s_i")):
            
            equation = simplify(equation)
            out = integrate(equation, wrt, tab+1, inf)
            
            if out is None:
                return None
            out = (out[0]*const, out[1])
            return out[0], logs+\
            [(tab, f"extracted the constant {printeq_str(simplify(const))}, now integrating the equation {printeq_str(simplify(equation))} only")]+out[1]+\
            [(tab, f"result is {printeq_str(simplify(out[0]))}")]
    
    out = integrate_summation(equation, wrt, tab, inf)
    if out is not None:
        return out[0], logs+out[1]+[(tab, f"result is {printeq_str(simplify(out[0]))}")]
    out = None
    if typeint in ["integrate", "sqint"]:
        out = sqint(equation, wrt, tab+1, inf)
        if out is not None:
            return out[0], logs+out[1]+[(tab, f"result is {printeq_str(simplify(out[0]))}")]
    if typeint == ["byparts", "integrate"]:
        if inf==0:
            out = integrate_subs_main(equation, wrt, tab, inf+1)
        if out is not None:
            return out[0], logs+out[1]+[(tab, f"result is {printeq_str(simplify(out[0]))}")]
    if typeint == "byparts":
        
        out = byparts(copy.deepcopy(equation), wrt, tab, inf)
        if out is not None:
            return out[0], logs+out[1]+[(tab, f"result is {printeq_str(simplify(out[0]))}")]
    return None
