from .expand import expand
from .ode import diffsolve, inversediff, order, groupe, epowersplit, subs2
from .base import *
from .simplify import simplify
from .diff import diff2
from .fraction import fraction
from .parser import parse
from .inverse import inverse
from .factor import factor

def capital2(eq):
    if eq.name == "f_pdif" and eq.children[0].name != "f_pdif":
        return eq.children[0]
    for child in eq.children:
        out = capital2(child)
        if out is not None:
            return out
    return None

def capital(eq):
    if eq.name[:2] == "f_" and eq.name != eq.name.lower():
        return eq
    for child in eq.children:
        out = capital(child)
        if out is not None:
            return out
    return None
def abs_const(eq):
    if eq.name == "f_abs":
        return tree_form("v_101")*eq.children[0]
    return TreeNode(eq.name, [abs_const(child) for child in eq.children])
def want(eq):
    if eq is None:
        return None
    if eq.name == "f_want":
        
        eq2 = eq.children[0]
        v = [tree_form(item) for item in vlist(eq.children[0])]
        lst = {}
        if eq.children[1].name == "f_and":
            for item in eq.children[1].children:
                item = abs_const(item)
                item = groupe(item)
                for item2 in v:
                    if contain(item, item2):
                        out = inverse(item.children[0], str_form(item2))
                        if out is not None:
                            lst[item2] = out
                            break
        for key in lst.keys():
            eq2 = replace(eq2, key, lst[key])
        if len(lst.keys()) == len(v):
            return fraction(groupe(simplify(eq2)))
    return TreeNode(eq.name, [want(child) for child in eq.children])
def absorb2(eq):
    if "v_103" in vlist(eq):
        v = vlist(eq)
        v.remove("v_103")
        if set(v)<set(["v_101", "v_102"]):
            return tree_form("v_103")
    if ["v_101"] == vlist(eq):
        return tree_form("v_101")
    if ["v_102"] == vlist(eq):
        return tree_form("v_102")
    if ["v_103"] == vlist(eq):
        return tree_form("v_103")
    return TreeNode(eq.name, [absorb2(child) for child in eq.children])
def absorb(eq):
    return dowhile(epowersplit(eq), absorb2)
def pde_sep(eq):
    if eq.name == "f_eq":
        eq = eq.children[0]
    r2 = parse("U(x,y)")
    eq = replace(eq, r2, parse("x").fx("X") * parse("y").fx("Y"))

    eq =  fraction(simplify(fraction(TreeNode("f_eq", [diff2(eq), tree_form("d_0")]))))
    out = inversediff(eq.children[0], tree_form("d_0"))
    if out is not None:
        out = list(out.children[0].children)
        if contain(out[0], tree_form("v_1")):
            out = out[::-1]
        out[0] = simplify(-out[0])
        
        lst = []
        for i in range(2):
            
            out[i] = TreeNode("f_eq", [out[i], tree_form("v_103")])
            out[i] = fraction(simplify(out[i]))
            r = capital(out[i])
            lst.append(r)
            out[i] = replace(out[i], r, tree_form(f"v_{1-i}"))
            out[i] = tree_form(str_form(out[i]).replace("f_pdif", "f_dif"))
            out[i] = diffsolve(out[i])
            
            out[i] = replace(out[i], tree_form(f"v_{1-i}"), r)
        out = TreeNode("f_eq", [r2, TreeNode("f_want", [product(lst), TreeNode("f_and", out)])])
        return replace(replace(out, lst[0], parse("a")), lst[1], parse("b"))
    return out
