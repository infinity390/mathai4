import itertools
from collections import Counter
from .diff import diff, diff2
from .factor import factor, factor2, take_common, term_common2
from .expand import expand
from .base import *
from .fraction import fraction
from .simplify import simplify
import copy
from .inverse import inverse
from .parser import parse
def rev(eq):
    tmp = factor_generation(eq)
    if tree_form("v_0").fx("dif")**-1 in tmp or tree_form("v_1").fx("dif")**-1 in  tmp:
        return False
    for child in eq.children:
        if not rev(child):
            return False
    return True
def clr(eq):
    return simplify(product([item for item in factor_generation(eq) if "f_add" in str_form(item)]))
def tt(eq):
    if eq.name == "f_add":
        return term_common2(eq.children)
    return eq
def shift(lhs, rhs):
    if isinstance(lhs, list):
        lhs = simplify(summation(lhs))
    if isinstance(rhs, list):
        rhs = simplify(summation(rhs))
    rhs = simplify(-rhs)
    out2 = [factor_generation(lhs), factor_generation(rhs)]
    out = [[], []]
    for i in range(2):
        for j in range(len(out2[i])-1,-1,-1):
            if contain(out2[i][j], tree_form(f"v_{1-i}")) and not contain(out2[i][j], tree_form(f"v_{i}")):
                out[1-i].append(out2[i].pop(j))
    output = [simplify(product(out2[i])*product(out[i])**tree_form("d_-1")) for i in range(2)]
    if ("f_dif" in str_form(output[0]) or "f_dif" in str_form(output[1])) and tree_form("v_0").fx("dif") not in factor_generation(output[0]):
        output = [tree_form("d_1")/item for item in output]
    return output
def check(eq):
    for item in factor_generation(eq):
        if contain(item, tree_form("v_0")) and contain(item, tree_form("v_1")) :
            return False
    return True
def t(eq):
    eq2 = simplify(summation(eq))
    lst = take_common(eq2)
    for item in lst:
        if item.name == "f_add" and len(item.children) == 2:
           if all(check(child) for child in item.children):
               return item
        elif check(item):
            return item
    return eq2
def inversediff(lhs, rhs, old=None):
    if old is None:
        old = []
    if lhs is None or rhs is None:
        return None
    eq = simplify(lhs - rhs)
    eq = tt(eq)
    eq = clr(eq)
    eq = t(eq.children)
    xonly = []
    yonly = []
    mixed = []
    const = []
    lst = []
    if eq.name != "f_add":
        lst.append(eq)
    else:
        lst = eq.children
    for item in lst:
        con_x = contain(item, tree_form("v_0"))
        con_y = contain(item, tree_form("v_1"))
        if not con_x and not con_y:
            const.append(item)
        elif con_x and con_y:
            mixed.append(item)
        elif con_x:
            xonly.append(item)
        elif con_y:
            yonly.append(item)
    if len(const) > 0:
        if len(yonly) > 0:
            yonly += const
        else:
            xonly += const
    if eq in old:
        return None
    if len(mixed) == 0:
        tmp = shift(lhs, -rhs)
        return simplify(lhs-rhs)
    elif len(mixed) > 0:
        m = t(mixed)
        if m.name == "f_add":
            mixed = m.children
        else:
            mixed = [m]
        if len(yonly) > 0 and len(xonly) > 0:
            tmp = inversediff(*(shift(t(mixed + xonly), yonly)+[old+[eq]]))
            if tmp is None:
                tmp = inversediff(*(shift(xonly, t(mixed + yonly))+[old+[eq]]))
                if tmp is None:
                    return None
            return tmp
        elif len(yonly) > 0:
            if m.name == "f_add":
                return None
            return inversediff(*(shift(m, yonly)+[old+[eq]]))
        elif len(xonly) > 0:
            if m.name == "f_add":
                return None
            return inversediff(*(shift(xonly, m)+[old+[eq]]))
        else:
            if len(mixed) == 2:
                return inversediff(*(shift(*mixed)+[old+[eq]]))
    return None
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
    lst = None
    if eq is None:
        return None
    if eq.name == "f_add":
        lst = list(eq.children)
    else:
        lst = [eq]
    s = [allocvar()]
    for item in lst:
        item = simplify(item)
        tmp = factor_generation(item)
        tmp2 = product([k for k in tmp if k.name != "f_dif"])
        if tree_form("v_0").fx("dif") in tmp:
            s.append(TreeNode("f_integrate", [tmp2, tree_form("v_0")]))
        elif tree_form("v_1").fx("dif") in tmp:
            s.append(TreeNode("f_integrate", [tmp2, tree_form("v_1")]))
    return TreeNode("f_eq", [summation(s), tree_form("d_0")])
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
    eq = diff2(eq)
    eq = subs2(eq, order(eq))
    eq = fraction(simplify(fraction(eq)))
    if order(eq) == 2:
        for i in range(2):
            out = second_order_dif(eq, tree_form(f"v_{i}"), tree_form(f"v_{1-i}"))
            if out is not None:
                return out
        return orig
    eq = diffsolve_sep2(diffsolve_sep(eq))
    if eq is None:
        for i in range(2):
            a = tree_form(f"v_{i}")
            b = tree_form(f"v_{1-i}")
            c = tree_form("v_2")
            eq2 = orig
            eq2 = subs2(eq2, 1)
            eq2 = replace(eq2, b, b*a)
            eq2 = subs3(eq2)
            eq2 = simplify(fraction(simplify(eq2)))
            eq2 = diffsolve_sep(eq2)
            eq2 = diffsolve_sep2(eq2)
            if eq2 is not None:
                return e0(TreeNode("f_subs", [replace(eq2.children[0],b,c), c,b/a]).fx("try"))
        eq = orig
        eq = simplify(eq)
        eq = subs2(eq, 1)
        eq = fraction(eq)
        for i in range(2):
            out = linear_dif(eq, tree_form(f"v_{i}"), tree_form(f"v_{1-i}"))
            if out is not None:
                return out
        return orig
    else:
        return eq
def clist(x):
    return list(x.elements())
def collect_term(eq, term_lst):
    lst = None
    if eq.name == "f_add":
        lst = copy.deepcopy(eq.children)
    else:
        lst = [eq]
    other = []
    dic = {}
    term_lst = list(sorted(term_lst, key=lambda x: -len(factor_generation(x))))
    for item in term_lst:
        dic[item] = tree_form("d_0")
    for item2 in lst:
        done = True
        tmp2 = Counter(factor_generation(item2))
        for index, item in enumerate(term_lst):
            tmp = Counter(factor_generation(item))
            if (tmp2&tmp) == tmp:
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
def order(eq,m=0):
    best = m
    if eq.name in ["f_pdif", "f_dif"]:
        out = order(eq.children[0], m+1)
        best = max(out, best)
    else:
        for child in eq.children:
            out = order(child, m)
            best = max(out, best)
    return best
def subs2(eq, orde):
    if eq.name in ["f_dif", "f_pdif"] and len(eq.children) == 2:
        if orde == 1:
            return eq.children[0].fx("dif")/eq.children[1].fx("dif")
        else:
            return eq
    return TreeNode(eq.name, [subs2(child, orde) for child in eq.children])
def subs3(eq):
    if eq.name == "f_dif" and eq.children[0].name == "f_add":
        return summation([subs3(child.fx("dif")) for child in eq.children[0].children])
    if eq.name == "f_dif" and eq.children[0].name == "f_mul":
        return summation([product([subs3(child.fx("dif")) if index==index2 else child for index2, child in enumerate(eq.children[0].children)]) for index in range(len(eq.children[0].children))])
    return TreeNode(eq.name, [subs3(child) for child in eq.children])
def second_order_dif(eq, a, b):
    eq = simplify(eq)
    nn = [TreeNode("f_dif", [TreeNode("f_dif", [b,a]),a]), TreeNode("f_dif", [b,a]), b]
    out = collect_term(eq.children[0], nn)
    if out[1] == tree_form("d_0"):
        tmp = out[0][nn[0]]
        if tmp != tree_form("d_0"):
            for key in out[0].keys():
                out[0][key] = simplify(out[0][key]/tmp)
            B = out[0][nn[1]]
            C = out[0][nn[2]]
            if all(all(not contain(item, item2) for item2 in [a,b]) for item in [B, C]):
                r = parse("r")
                s = simplify(factor2(simplify(TreeNode("f_eq", [r**2 + B*r + C, tree_form("d_0")])), True))
                r1, r2 = [inverse(item, r.name) for item in s.children[0].children]
                out = None
                if contain(r1, tree_form("s_i")):
                    real = simplify(fraction((r1+r2)/tree_form("d_2")))
                    imagine = simplify((r1-real)/tree_form("s_i"))
                    out = tree_form("s_e")**(real*a)*(tree_form("v_101")*(imagine*a).fx("cos")+tree_form("v_102")*(imagine*a).fx("sin"))
                elif fraction(simplify(r1-r2)) == tree_form("d_0"):
                    out =(tree_form("v_101")+tree_form("v_102")*a)*tree_form("s_e")**(r1*a)
                else:
                    out = tree_form("v_101")*tree_form("s_e")**(r1*a) + tree_form("v_102")*tree_form("s_e")**(r2*a)
                return TreeNode("f_eq", [b, out])
    return None 
def linear_dif(eq, a, b):
    eq = simplify(eq)
    out = collect_term(eq.children[0], [b.fx("dif"), b*a.fx("dif"), a.fx("dif")])
    if out[1] == tree_form("d_0"):
        tmp = out[0][b.fx("dif")]
        if tmp != tree_form("d_0"):
            for key in out[0].keys():
                out[0][key] = simplify(out[0][key]/tmp)
            p, q = out[0][b*a.fx("dif")], -out[0][a.fx("dif")]
            if contain(p, b) or contain(q, b):
                return None
            f = tree_form("s_e") ** TreeNode("f_integrate", [p, a])
            return simplify(TreeNode("f_eq", [b*f, TreeNode("f_integrate", [q*f, a])+allocvar()]))
    return None
