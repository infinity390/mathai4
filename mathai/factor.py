import itertools
from .trig import trig0
from .parser import parse
from .structure import transform_formula
from .base import *
from .simplify import simplify, convert_to_basic
from .expand import expand
import math
from .tool import poly
from .fraction import fraction

from collections import Counter
def multiset_intersection(*lists):
    counters = list(map(Counter, lists))
    common = counters[0]
    for c in counters[1:]:
        common = common & c
    return list(common.elements())
def subtract_sublist(full_list, sublist):
    c_full = Counter(full_list)
    c_sub = Counter(sublist)
    result = c_full - c_sub
    tmp = list(result.elements())
    if tmp == []:
        return [tree_form("d_1")]
    return tmp
def term_common2(lst, take_neg_common=False):
    s = []
    arr = [factor_generation(child,True) for child in lst]
    if take_neg_common and not all(tree_form("d_-1") not in item for item in arr):
        arr = [item+[tree_form("d_-1"), tree_form("d_-1")] if tree_form("d_-1") not in item else item for item in arr]
    s = multiset_intersection(*arr)
    if s == []:
        return simplify(summation(lst))
    arr = [subtract_sublist(item, s) for item in arr]
    return simplify(product(s)*summation([product(item) for item in arr]))
def take_common(eq):
    score = []
    output = []
    def helper(eq, depth=0):
        nonlocal score
        nonlocal output
        key = tuple(sorted(eq,key=lambda x: str_form(x)))
        if key in output:
            return
        else:
            score.append(depth)
            output.append(key)
        for item3 in partitions(eq):
            for option in [False, True]:
                item = [term_common2(item2, option) for item2 in item3]  
                helper(item, depth+1)
    if eq.name == "f_add":
        helper(eq.children)
    else:
        return eq
    sc = {}
    for i in range(len(output)):
        output[i] = simplify(summation(list(output[i])))
        sc[output[i]] = score[i]
    output = list(set(output))
    return list(sorted(output, key=lambda x: -sc[x]))

def _factorconst(eq):
    def hcf_list(numbers):
        if not numbers:
            return None  # empty list
        n = 1
        if math.prod(numbers) < 0:
            n = -1
        hcf = numbers[0]
        for num in numbers[1:]:
            hcf = math.gcd(hcf, abs(num))
        return hcf*n
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

def _merge_sqrt(eq):
    lst= []
    eq2 = []
    for child in factor_generation(eq):
        if frac(child) is not None and frac(child).denominator==1:
            if frac(child)>0:
                eq2.append(child**2)
            elif frac(child)!=-1:
                eq2.append((-child)**2)
                lst.append(tree_form("d_-1"))
            else:
                lst.append(tree_form("d_-1"))
        elif child.name == "f_pow" and frac(child.children[1]) == Fraction(1,2):
            eq2.append(child.children[0])
        else:
            lst.append(child)
            
    if len(eq2)>1:
        if lst == []:
            lst= [tree_form("d_1")]
        return simplify(product(eq2)**(tree_form("d_2")**-1)*product(lst))
    return TreeNode(eq.name, [_merge_sqrt(child) for child in eq.children])
def sqrt_to_a_sqrt_b(n):
    if n == 0:
        return 0, 0
    sign = 1
    if n < 0:
        sign = -1
        m = -n
    else:
        m = n

    a = 1
    b = 1
    p = 2
    while p * p <= m:
        exp = 0
        while m % p == 0:
            m //= p
            exp += 1
        if exp:
            a *= p ** (exp // 2)
            if exp % 2 == 1:
                b *= p
        p += 1 if p == 2 else 2

    if m > 1:
        b *= m

    return sign * a, b
def merge_sqrt(eq):
    def helper(eq):
        if eq.name == "f_pow" and frac(eq.children[1]) == Fraction(1,2):
            if eq.children[0].name[:2] == "d_":
                n = int(eq.children[0].name[2:])
                a, b =sqrt_to_a_sqrt_b(n)
                return tree_form("d_"+str(b))**(tree_form("d_2")**-1)*tree_form("d_"+str(a))
        if eq.name == "f_pow" and frac(eq.children[1]) == Fraction(-1,2):
            if frac(eq.children[0]) is not None:
                out = frac(eq.children[0])
                b, a = [tree_form("d_"+str(x)) for x in [out.numerator, out.denominator]]
                return a**(tree_form("d_2")**-1)/b**(tree_form("d_2")**-1)
        return TreeNode(eq.name, [helper(child) for child in eq.children])
    return helper(_merge_sqrt(eq))
def rationalize_sqrt(eq):
    if eq.name== "f_pow" and frac(eq.children[1]) == Fraction(-1,2):
        eq = eq.children[0]**(tree_form("d_2")**-1)/eq.children[0]
    def term(eq):
        if eq.name == "f_add":
            output = []
            for child in eq.children:
                if any(child2.name == "f_pow" and frac(child2.children[1]) == Fraction(1,2) for child2 in factor_generation(child)):
                    output.append(simplify(-child))
                else:
                    output.append(child)
            return summation(output)
        return None
    n, d=num_dem(eq)
    n,d=simplify(n), simplify(d)
   
    if d != 1:
        t = term(d)
        if t is not None and t!=1:
            
            n,d=simplify(expand(simplify(n*t))),simplify(expand(simplify(d*t)))
            tmp= simplify(n/d)
            
            tmp = _merge_sqrt(tmp)
            
            return tmp
    return TreeNode(eq.name, [rationalize_sqrt(child) for child in eq.children])
def factorconst(eq):
    return simplify(_factorconst(eq))

def factor_helper(equation, complexnum, power=2):
    global formula_gen9
    if equation.name in ["f_or", "f_and", "f_not", "f_eq", "f_gt", "f_lt", "f_ge", "f_le"]:
        return TreeNode(equation.name, [factor_helper(child, complexnum, power) for child in equation.children])
    maxnum=1
    alloclst = []
    for i in range(0,26):
        if "v_"+str(i) not in vlist(equation):
            alloclst.append("v_"+str(i))
    r = alloclst.pop(0)
    fx = None
    curr = None
    def high(eq):
        nonlocal maxnum
        if eq.name == "f_pow" and eq.children[1].name[:2] == "d_":
            n = int(eq.children[1].name[2:])
            if abs(n)>power and abs(n) % power == 0:
                if abs(n)>abs(maxnum):
                    maxnum = n
        for child in eq.children:
            high(child)
    def helper(eq):
        nonlocal maxnum, fx, r
        if eq.name == "f_pow" and eq.children[1].name[:2] == "d_" and eq.children[0] == curr:
            n = int(eq.children[1].name[2:])
            if maxnum !=1 and n % maxnum == 0:
                fx = lambda x: replace(x, tree_form(r), curr**tree_form("d_"+str(maxnum)))
                out= tree_form(r)**tree_form("d_"+str(int(n/maxnum)))
                return out
        return TreeNode(eq.name, [helper(child) for child in eq.children])
    out = None
    
    for i in range(2,4):
        if power == i:
            
            for curr in vlist(equation):
                curr = tree_form(curr)
                fx = None
                maxnum = 1
                high(equation.copy_tree())
                
                if maxnum != 1:
                    maxnum= maxnum/power
                    maxnum = round(maxnum)
                eq2 = helper(equation.copy_tree())
                if not contain(eq2, tree_form(r)) or (contain(eq2, tree_form(r)) and not contain(eq2,curr)):
                    if not contain(eq2, tree_form(r)):
                        r = curr.name
                        fx = lambda x: x
                        
                    lst = poly(eq2.copy_tree(), r)
                    
                    if lst is not None and len(lst)==i+1:
                        
                        success = True
                        if i == 2:
                            
                            a, b, c = lst
                            x1 = (-b+(b**2 - 4*a*c)**(tree_form("d_2")**-1))/(2*a)
                            x2 = (-b-(b**2 - 4*a*c)**(tree_form("d_2")**-1))/(2*a)
                            x1 = fraction(simplify(x1))
                            x2 = fraction(simplify(x2))
                            eq2 = a*(tree_form(r)-x1)*(tree_form(r)-x2)
                            if not complexnum and (contain(x1, tree_form("s_i")) or contain(x2, tree_form("s_i"))):
                                success = False
                        else:
                            a, b, c, d = lst
                            B, C, D =  b/a, c/a, d/a
                            p = C-(B**2)/3
                            q = 2*B**3/27-B*C/3+D
                            t = q**2/4+ p**3/27

                            if compute(t) > 0:
                                u = (-q/2+t**(tree_form("d_2")**-1))**(tree_form("d_3")**-1)
                                v = (-q/2-t**(tree_form("d_2")**-1))**(tree_form("d_3")**-1)
                                y1 = u+v
                                three = 3**(tree_form("d_2")**-1)
                                y2 = -(u+v)/2+tree_form("s_i")*three*(u-v)/2
                                y3 = -(u+v)/2-tree_form("s_i")*three*(u-v)/2
                                
                            else:
                                ar = 2*(-p/3)**(tree_form("d_2")**-1)
                                phi = ((3*q/(2*p))*(-3/p)**(tree_form("d_2")**-1)).fx("arccos")
                                y1 = ar*(phi/3).fx("cos")
                                y2 = ar*((phi+2*tree_form("s_pi"))/3).fx("cos")
                                y3 = ar*((phi+4*tree_form("s_pi"))/3).fx("cos")
                                
                            x1,x2,x3 = y1-B/3 , y2-B/3, y3-B/3
                            x1 = simplify(trig0(simplify(x1)))
                            x2 = simplify(trig0(simplify(x2)))
                            x3 = simplify(trig0(simplify(x3)))
                            
                            out2 = None
                            if not complexnum:
                                for item in itertools.combinations([x1,x2,x3],2):
                                    if all(contain(item2,tree_form("s_i")) for item2 in list(item)):
                                        out2 = (tree_form(r)-item[0])*(tree_form(r)-item[1])
                                        break
                            if out2 is not None:
                                out2 = simplify(fraction(expand(simplify(out2))))
                                out3 = None
                                for item in [x1, x2, x3]:
                                    if not contain(item,tree_form("s_i")):
                                        out3 = item
                                        break
                                eq2 = a*(tree_form(r)-out3)*out2
                                
                            else:
                                eq2 = a*(tree_form(r)-x1)*(tree_form(r)-x2)*(tree_form(r)-x3)
                        if success:
                            equation = fx(eq2)
                            break

    
    if out is not None:
        out = simplify(out)
    if out is not None and (complexnum or (not complexnum and not contain(out, tree_form("s_i")))):
        return out
    
    return TreeNode(equation.name, [factor_helper(child, complexnum, power) for child in equation.children])
def factor(equation):
    if equation.name == "f_add": 
        return take_common(equation)[0]
    return TreeNode(equation.name, [factor(child) for child in equation.children])

def factor2(equation, complexnum=False):
    return simplify(factor_helper(simplify(equation), complexnum, 2))

def factor3(equation, complexnum=False):
    return simplify(factor_helper(simplify(factor_helper(simplify(equation), complexnum, 2)), complexnum, 3))
