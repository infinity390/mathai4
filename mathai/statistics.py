from .base import *
from .simplify import simplify
def expect(eq):
    if eq.name == "f_expect":
        if eq.children[0].name == "f_add":
            eq = summation([item.fx("expect") for item in eq.children[0].children])
    if eq.name == "f_expect":
        out = []
        keep = []
        for child in eq.children:
            if "v_-" in str_form(child) and child.name != "f_expect":
                keep.append(child)
            else:
                out.append(child)
        eq = simplify(product(out)*product(keep).fx("expect"))
    if eq.name == "f_variance":
        term = eq.children[0]
        eq = (term**2).fx("expect") + term.fx("expect")**2
    if eq.name == "f_covariance":
        x, y = eq.children
        eq = (x*y).fx("expect") - x.fx("expect")*y.fx("expect")
    return TreeNode(eq.name, [expect(child) for child in eq.children])
