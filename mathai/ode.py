from collections import Counter
from .diff import diff
from .factor import factor
from .expand import expand
from .base import *
from .fraction import fraction
from .simplify import simplify
import copy

def inversediff(lhs, rhs):
    count = 5
    while contain(rhs, tree_form("v_1")) or contain(lhs, tree_form("v_0")):
        success = False
        if rhs.name == "f_add":
            for i in range(len(rhs.children)-1,-1,-1):
                if not contain(rhs.children[i], tree_form("v_0")) or str_form(tree_form("v_1").fx("dif")) in [str_form(x) for x in factor_generation(rhs.children[i])]:
                    if contain(rhs.children[i], tree_form("v_0")) or contain(rhs.children[i], tree_form("v_1")):
                        success = True
                    lhs = lhs - rhs.children[i]
                    rhs.children.pop(i)
        elif rhs.name == "f_mul":
            for i in range(len(rhs.children)-1,-1,-1):
                if not contain(rhs.children[i], tree_form("v_0")):
                    if contain(rhs.children[i], tree_form("v_0")) or contain(rhs.children[i], tree_form("v_1")):
                        success = True
                    lhs = lhs / rhs.children[i]
                    rhs.children.pop(i)
        if len(rhs.children) == 1:
            rhs = rhs.children[0]
        rhs, lhs = copy.deepcopy([simplify(lhs), simplify(rhs)])
        if rhs.name == "f_add":
            for i in range(len(rhs.children)-1,-1,-1):
                if not contain(rhs.children[i], tree_form("v_1")) or str_form(tree_form("v_0").fx("dif")) in [str_form(x) for x in factor_generation(rhs.children[i])]:
                    if contain(rhs.children[i], tree_form("v_0")) or contain(rhs.children[i], tree_form("v_1")):
                        success = True
                    lhs = lhs - rhs.children[i]
                    rhs.children.pop(i)
        elif rhs.name == "f_mul":
            for i in range(len(rhs.children)-1,-1,-1):
                if not contain(rhs.children[i], tree_form("v_1")):
                    if contain(rhs.children[i], tree_form("v_0")) or contain(rhs.children[i], tree_form("v_1")):
                        success = True
                    lhs = lhs / rhs.children[i]
                    rhs.children.pop(i)
        rhs, lhs = copy.deepcopy([simplify(lhs), simplify(rhs)])
        if not success:
            lhs, rhs = factor(lhs),factor(rhs)
        count -= 1
        if count == 0:
            return simplify(e0(lhs-rhs))
    return simplify(e0(lhs-rhs))

def allocvar():
    return tree_form("v_101")

def epowersplit(eq):
    if eq.name == "f_pow" and eq.children[1].name == "f_add":
        return product([eq.children[0]**child for child in eq.children[1].children])
    return TreeNode(eq.name, [epowersplit(child) for child in eq.children])
def esolve(s):
    if s.name == "f_add" and "f_log" in str_form(s):
        return product([tree_form("s_e")**child for child in s.children]) - tree_form("d_1")
    return TreeNode(s.name, [esolve(child) for child in s.children])
def diffsolve_sep2(eq):
    global tab
    
    s = []
    eq = simplify(expand(eq))
    eq = e1(eq)
    
    def vlor1(eq):
        if contain(eq, tree_form("v_0")) and not contain(eq, tree_form("v_1")):
            return True
        if contain(eq, tree_form("v_1")) and not contain(eq, tree_form("v_0")):
            return True
        return False
    if eq.name == "f_add" and all(vlor1(child) and [str_form(x) for x in factor_generation(copy.deepcopy(child))].count(str_form(tree_form(vlist(child)[0]).fx("dif")))==1 for child in eq.children):
        for child in eq.children:
            v = vlist(child)[0]
            v2 = tree_form(v).fx("dif")
            child = replace(child, v2, tree_form("d_1"))
            child = simplify(child)
            
            
            tmp6 = TreeNode("f_integrate", [child, tree_form(v)])
            s.append(tmp6)
            
            if s[-1] is None:
                return None
        s.append(allocvar())
    else:
        return None
    s = summation(s)
    s = simplify(e0(s))
    
    return groupe(s)
def e0(eq):
    return TreeNode("f_eq", [eq, tree_form("d_0")])
def e1(eq):
    if eq.name == "f_eq":
        eq = eq.children[0]
    return eq
def groupe(eq):
    eq = esolve(eq)
    eq = simplify(eq)
    eq = fraction(eq)
    eq = simplify(eq)
    eq = epowersplit(eq)
    return eq

def diffsolve_sep(eq):
    eq = epowersplit(eq)

    eq = inversediff(tree_form("d_0"), eq.children[0].copy_tree())
    return eq

def diffsolve(eq):
    orig = eq.copy_tree()

    
    eq = diffsolve_sep2(diffsolve_sep(eq))
    if eq is None:
        for i in range(2):
            a = tree_form(f"v_{i}")
            b = tree_form(f"v_{1-i}")
            c = tree_form("v_2")
            eq2 = replace(orig, b,b*a)
            eq2 = expand(simplify(fraction(simplify(diff(eq2, None)))))
            eq2 = diffsolve_sep(eq2)
            eq2 = diffsolve_sep2(eq2)
            if eq2 is not None:
                return e0(TreeNode("f_subs", [replace(eq2.children[0],b,c), c,b/a]).fx("try"))
        eq = orig
    
    eq = fraction(eq)
    eq = simplify(eq)
    for i in range(2):
        out = linear_dif(eq, tree_form(f"v_{i}"), tree_form(f"v_{1-i}"))
        if out is not None:
            return out
    return eq
def clist(x):
    return list(x.elements())
def collect_term(eq, term_lst):
    
    lst = None
    if eq.name == "f_add":
        lst = eq.children
    else:
        lst = [eq]
        
    other = []
    dic = {}
    term_lst = sorted(term_lst, key=lambda x: -len(factor_generation(x)))
    for item2 in lst:
        done = True
        tmp2 = Counter(factor_generation(item2))
        for index, item in enumerate(term_lst):
            tmp = Counter(factor_generation(item))
            
            if (tmp2&tmp) == tmp and clist((tmp2 - tmp)&tmp)==[]:
                if item in dic.keys():
                    dic[item] += product(clist(tmp2-tmp))
                else:
                    dic[item] = product(clist(tmp2-tmp))
                done = False
                break
        if done:
            other.append(item2)
    other = summation(other)
    
    for key in dic.keys():
        dic[key] = simplify(dic[key])
    return [dic, simplify(other)]
def linear_dif(eq, a, b):
    eq = simplify(eq)
    out = collect_term(eq.children[0], [b.fx("dif"), b*a.fx("dif"), a.fx("dif")])
    
    if out[1] == tree_form("d_0"):
        tmp = out[0][b.fx("dif")]
        if tmp != tree_form("d_0"):
            
            for key in out[0].keys():
                out[0][key] = simplify(out[0][key]/tmp)
            p, q = out[0][b*a.fx("dif")], -out[0][a.fx("dif")]
            
            f = tree_form("s_e") ** TreeNode("f_integrate", [p, a])
            return simplify(TreeNode("f_eq", [b*f, TreeNode("f_integrate", [q*f, a])+allocvar()]))
    return None
