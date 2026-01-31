from .ode import diffsolve
from .base import *
from .simplify import simplify
from .diff import diff
from .fraction import fraction
from .parser import parse
from .inverse import inverse
def capital2(eq):
    if eq.name == "f_pdif":
        return eq.children[0]
    for child in eq.children:
        out = capital2(child)
        if out is not None:
            return out
    return None
def subs(eq, r2):
    if eq.name == "f_dif":
        return TreeNode("f_pdif", eq.children)
    if eq == r2:
       return parse("x").fx("X") * parse("y").fx("Y")
    if eq.name == "f_pdif":
        return subs(diff(subs(eq.children[0], r2), str_form(eq.children[1])), r2)
    return TreeNode(eq.name, [subs(child, r2) for child in eq.children])
def inverse_pde(lhs, rhs, depth=3):
    if depth < 0:
        return None
    
    lhs = simplify(lhs.copy_tree())
    rhs = simplify(rhs.copy_tree())
    
    # separation check: lhs has no y, rhs has no x
    lhs_str = str_form(lhs)
    rhs_str = str_form(rhs)

    if "v_1" not in lhs_str and "v_0" not in rhs_str:
        return [lhs, rhs]

    sides = [lhs, rhs]

    for side in range(2):
        eq = sides[side]

        if eq.name not in ("f_add", "f_mul"):
            continue

        # iterate over a COPY — never mutate while iterating
        for i, child in enumerate(eq.children.copy()):
            # rebuild remaining expression safely
            rest_children = [
                c.copy_tree()
                for j, c in enumerate(eq.children)
                if j != i
            ]

            if not rest_children:
                continue

            if len(rest_children) == 1:
                rest = rest_children[0]
            else:
                rest = TreeNode(eq.name, rest_children)

            other = sides[1 - side].copy_tree()

            # move term across
            if eq.name == "f_add":
                moved = TreeNode("f_add", [other, -child.copy_tree()])
            else:  # f_mul
                moved = TreeNode("f_mul", [other, TreeNode("f_pow", [child.copy_tree(), tree_form("d_-1")])])

            moved = simplify(moved)
            rest = simplify(rest)

            if side == 0:
                out = inverse_pde(rest, moved, depth - 1)
            else:
                out = inverse_pde(moved, rest, depth - 1)

            if out is not None:
                return out

    return None
def subs2(eq):
    if eq.name == "f_pdif":
        return eq.children[0].fx("dif")/eq.children[1].fx("dif")
    return TreeNode(eq.name, [subs2(child) for child in eq.children])
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
    if eq.name == "f_want":
        eq2 = eq.children[0]
        v = [tree_form(item) for item in vlist(eq.children[0])]
        lst = {}
        if eq.children[1].name == "f_and":
            for item in eq.children[1].children:
                item = abs_const(item)
                for item2 in v:
                    if contain(item, item2):
                        out = inverse(item.children[0], str_form(item2))
                        if out is not None:
                            lst[item2] = out
                            break
        for key in lst.keys():
            eq2 = replace(eq2, key, lst[key])
        if len(lst.keys()) == len(v):
            return simplify(eq2)
    return TreeNode(eq.name, [want(child) for child in eq.children])
def pde_sep(eq):
    if eq.name == "f_eq":
        eq = eq.children[0]
    r2 = capital2(eq)
    
    eq =  simplify(fraction(subs(eq, r2)))
    out = inverse_pde(eq,tree_form("d_0"))
    if out is not None:
        out = list(out)
        lst = []
        for i in range(2):
            out[i] = subs2(out[i])
            out[i] = TreeNode("f_eq", [out[i], tree_form("v_102")])
            out[i] = fraction(simplify(out[i]))
            r = capital(out[i])
            lst.append(r)
            out[i] = replace(out[i], r, tree_form(f"v_{1-i}"))
            out[i] = diffsolve(out[i])
            out[i] = replace(out[i], tree_form(f"v_{1-i}"), r)
        out = TreeNode("f_eq", [r2, TreeNode("f_want", [product(lst), TreeNode("f_and", out)])])
        return replace(replace(out, lst[0], parse("a")), lst[1], parse("b"))
    return out
