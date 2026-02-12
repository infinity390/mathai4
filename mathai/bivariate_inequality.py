import itertools
from .fraction import fraction
from .base import *
from .simplify import simplify
from .expand import expand
from .logic import logic0, logic4, truth_gen
from .univariate_inequality import absolute
from .factor import factorconst
def helper2(eq):
    if eq.name in ["f_forall", "f_exist"]:
        return False
    if eq.name in ["f_and", "f_not", "f_or", "f_equiv", "f_imply"]:
        return True
    return any(helper2(child) for child in eq.children)
def helper(eq):
    op_dic = {"le":"ge","ge":"le","gt":"lt","lt":"gt","eq":"eq"}
    out2 = []
    eq = simplify(eq)
    eq = dowhile(eq, absolute)
    logic00 = lambda x: dowhile(x,logic0)
    simp = lambda y: simplify(factorconst(dowhile(y, fraction)))
    simp2 = simp
    
    def canon(eq):
        if eq.name[2:] in "eq ge gt le lt".split(" "):
            eq1 = simplify(TreeNode("f_"+op_dic[eq.name[2:]], [simp2(-eq.children[0]), tree_form("d_0")]))
            eq2 = simplify(TreeNode(eq.name, [simp2(eq.children[0]), tree_form("d_0")]))
            if eq1 == eq2:
                return eq1
            elif str_form(eq1.children[0]) < str_form(eq2.children[0]):
                return eq2
            else:
                return eq1
        return TreeNode(eq.name, [canon(child) for child in eq.children])
    def prepare2(eq):
        nonlocal out2
        if eq.name[2:] in "eq ge gt le lt".split(" "):
            if eq not in out2:
                out2.append(eq)
                return
        if not helper2(eq):
            out2.append(eq)
            return
        for child in eq.children:
            prepare2(child)
    eq = simp(eq)
    
    eq = canon(eq)
    
    eq = logic00(eq)
    prepare2(eq)
    
    dic = {}
    v = ["v_"+str(i) for i in range(26) if "v_"+str(i) not in vlist(eq)]

    for key in out2:
      if key not in dic.keys():
          dic[key] = tree_form(v.pop(0))
      eq = replace(eq,key,dic[key])
    
    eq = truth_gen(eq)
    if eq.name in ["s_true", "s_false"]:
        return eq
    
    eq = logic4(eq)
    
    for key in dic.keys():
      eq = replace(eq,dic[key],key)
    return eq
def solve_logically2(eq):
    if eq.name in ["f_forall", "f_exist"]:
        s = str_form(eq.children[1])
        if "f_forall" not in s and "f_exist" not in s :
            return TreeNode(eq.name, [eq.children[0], helper(eq.children[1])])
        else:
            return TreeNode(eq.name, [eq.children[0], solve_logically2(helper(eq.children[1]))])
    return TreeNode(eq.name, [solve_logically2(child) for child in eq.children])
def solve_logically(eq):
    eq2 = helper(eq)
    return solve_logically2(eq2)
