from parser import parse
import itertools
from diff import diff
from fraction import fraction
from simplify import solve, simplify
from expand import expand
from base import *
from printeq import printeq_str
from structure import transform_formula
from inverse import inverse
def integrate_summation(equation, wrt, tab):
    logs= []
    for i in range(2):
        
        if equation.name == "f_add":
            logs += [(tab, f"by integration over sums {", ".join([printeq_str(simplify(child)) for child in equation.children])}")]
            answer = []
            for child in equation.children:
                out = integrate(child, wrt, tab+1, tab)
                if out is None:
                    return None
                logs += out[1]
                answer.append(out[0])
            return summation(answer), logs
        if i == 0:
            
            tmp = expand(equation)
            logs += [(tab, f"integrating  {printeq_str(simplify(equation))} will be the same thing as integrating {printeq_str(simplify(tmp))}")]
            equation = tmp
    return None
def subs_heuristic(eq, var):
    output = []
    def collect2(eq):
        if eq.name == "f_pow" and eq.children[0] == tree_form(var) and eq.children[1].name[:2]=="d_":
            if int(eq.children[1].name[2:])==6:
                output.append(str_form( tree_form(var)**tree_form("d_3") ))
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
    tmp = [solve(tree_form(x)) for x in tmp]
    return tmp

def integrate_subs(equation, term, v1, v2, tab):
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
        out = integrate(eq, origv2, tab+1)
       
        if out is None:
            continue
        tmp, logs = out
        tmp = replace(tmp, tree_form(v2), g)
        return tmp, [(tab, f"substituted {str(tree_form(origv2))}={printeq_str(simplify(g))}, integrating {printeq_str(simplify(eq))} wrt {str(tree_form(origv2))}")]+logs+\
               [(tab, f"substituting back to {printeq_str(simplify(out[0]))} which is the result after integration")]
    return None

def integrate_subs_main(equation, wrt, tab):
    v2 = "v_"+str(int(wrt[2:])+1)
    for item in subs_heuristic(equation, wrt):
        x = tree_form(v2)-item
        
        tmp3 = integrate_subs(equation, x, wrt, v2, tab)
        
        if tmp3 is not None:
            return tmp3[0], tmp3[1]
    return None
def integration_formula_init():
    var = "x"
    formula_list = [(f"(A*{var}+B)^C", f"(A*{var}+B)^(C+1)/(A*(C+1))"),\
                    (f"sin(A*{var}+B)", f"-cos(A*{var}+B)/A"),\
                    (f"cos(A*{var}+B)", f"sin(A*{var}+B)/A")]
    formula_list = [[parse(y) for y in x] for x in formula_list]
    expr = [[parse("A"), parse("1")], [parse("B"), parse("0")]]
    return [formula_list, var, expr]
formula_gen = integration_formula_init()
def integrate(equation, wrt="v_0", tab=0, inf=None):
    global formula_list, var, expr
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
        if solve(const) != 1:
            
            equation = solve(equation)
            out = integrate(equation, wrt, tab+1)
            
            if out is None:
                return None
            out = (out[0]*const, out[1])
            return out[0], logs+out[1]+[(tab, f"extracted the constant {printeq_str(simplify(const))}, now integrating the equation {printeq_str(simplify(equation))} only"),
                                   (tab, f"result is {printeq_str(simplify(out[0]))}")]
    
    out = integrate_summation(equation, wrt, tab)
    if out is not None:
        return out[0], logs+out[1]+[(tab, f"result is {printeq_str(simplify(out[0]))}")]
    out = None
    if tab==0 or (inf is not None and inf==0):
        out = integrate_subs_main(equation, wrt, tab)
    if out is not None:
        return out[0], logs+out[1]+[(tab, f"result is {printeq_str(simplify(out[0]))}")]
    return None
