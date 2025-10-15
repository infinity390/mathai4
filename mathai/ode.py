from .factor import factor
from .expand import expand
from .base import *
from .fraction import fraction
from .simplify import simplify
import copy

def inversediff(lhs, rhs):
    count = 4
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

intconst = ["v_"+str(i) for i in range(101,150)]
def allocvar():
    global intconst
    return tree_form(intconst.pop(0))

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

    
    eq = diffsolve_sep2(eq)
    if eq is None:
        return orig
    return eq
