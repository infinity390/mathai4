import itertools
from .fraction import fraction
from .base import *
from .simplify import simplify
from .expand import expand
from .logic import logic0, logic4, truth_gen
from .univariate_inequality import absolute
from .factor import factorconst

def solve_logically(eq):
    op_dic = {"le":"ge","ge":"le","gt":"lt","lt":"gt","eq":"eq"}
    out2 = []
    eq = simplify(eq)
    eq = dowhile(eq, absolute)
    logic00 = lambda x: dowhile(x,logic0)
    simp = lambda y: dowhile(y, lambda x: factorconst(fraction(simplify(expand(x)))))
    simp2 = lambda y: dowhile(y, lambda x: fraction(simplify(expand(x))))
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
        for child in eq.children:
            prepare2(child)
    eq = simp(eq)
    eq = canon(eq)
    eq = logic00(eq)
    prepare2(eq)
    
    dic = {}
    v = ["v_"+str(i) for i in range(26) if "v_"+str(i) not in vlist(eq)]
    pair = []
    for item in itertools.combinations(list(range(len(out2))), 2):
        if out2[item[0]].children[0] == out2[item[1]].children[0]:
            pair.append([tree_form(v[item[0]]),tree_form(v[item[1]])])
    for key in out2:
      dic[key] = tree_form(v.pop(0))
      eq = replace(eq,key,dic[key])
    
    eq = truth_gen(eq)
    if eq.name in ["s_true", "s_false"]:
        return eq
    eq2 = []
    for i in range(len(eq.children)):
        if eq.children[i].name != "f_and":
            eq2.append(eq.children[i])
            continue
        c = set(eq.children[i].children)
        c2 = False
        for item in pair:
            if item[0] in c and item[1] in c:
                c2 = True
                break
            if item[0].fx("not") in c and item[1] in c:
                c = c - {item[0].fx("not")}
            if item[1].fx("not") in c and item[0] in c:
                c = c - {item[1].fx("not")}
        if len(c) == 0 or c2:
            eq2.append(tree_form("s_false"))
        elif len(c) == 1:
            eq2.append(list(c)[0])
        else:
            eq2.append(and_all(list(c)))
    eq = or_all(eq2)
    eq = logic4(eq)    
    for key in dic.keys():
      eq = replace(eq,dic[key],key)
    return eq
