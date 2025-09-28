from .simplify import solve
from .base import *

def diff(equation, var="v_0"):
    def diffeq(eq):
        eq = solve(eq)
        if "v_" not in str_form(eq):
            return tree_form("d_0")
        if eq.name == "f_add":
            add = tree_form("d_0")
            for child in eq.children:
                add += diffeq(child)
            return add
        elif eq.name == "f_abs":
            return diffeq(eq.children[0])*eq.children[0]/eq
        elif eq.name == "f_pow" and eq.children[0].name == "s_e":
            return diffeq(eq.children[1])*eq
        elif eq.name == "f_tan":
            return diffeq(eq.children[0])/(eq.children[0].fx("cos")*eq.children[0].fx("cos"))
        elif eq.name == "f_log":
            return diffeq(eq.children[0])*(tree_form("d_1")/eq.children[0])
        elif eq.name == "f_arcsin":
            return diffeq(eq.children[0])/(tree_form("d_1")-eq.children[0]*eq.children[0])**(tree_form("d_2")**-1)
        elif eq.name == "f_arccos":
            return tree_form("d_-1")*diffeq(eq.children[0])/(tree_form("d_1")-eq.children[0]*eq.children[0])**(tree_form("d_2")**-1)
        elif eq.name == "f_arctan":
            return diffeq(eq.children[0])/(tree_form("d_1")+eq.children[0]*eq.children[0])
        elif eq.name == "f_pow" and "v_" in str_form(eq.children[1]):
            a, b = eq.children
            return a**b * ((b/a) * diffeq(a) + a.fx("log") * diffeq(b))
        elif eq.name == "f_mul":
            add = tree_form("d_0")
            for i in range(len(eq.children)):
                tmp = eq.children.pop(i)
                if len(eq.children)==1:
                    eq2 = eq.children[0]
                else:
                    eq2 = eq
                add += diffeq(tmp)*eq2
                eq.children.insert(i, tmp)
            return add
        elif eq.name == "f_sin":
            eq.name = "f_cos"
            return diffeq(eq.children[0])*eq
        elif eq.name == "f_cos":
            eq.name = "f_sin"
            return tree_form("d_-1")*diffeq(eq.children[0])*eq
        elif eq.name[:2] == "v_":
            return TreeNode("f_dif", [eq])
        elif eq.name == "f_pow" and "v_" not in str_form(eq.children[1]):
            base, power = eq.children
            dbase = diffeq(base)
            b1 = power - tree_form("d_1")
            bab1 = TreeNode("f_pow", [base, b1])
            return power * bab1 * dbase
        return eq.fx("dif")
    def helper(equation, var="v_0"):
        if equation.name == "f_dif":
            if equation.children[0].name == var:
                return tree_form("d_1")
            return tree_form("d_0")
        return TreeNode(equation.name, [helper(child, var) for child in equation.children])
    equation = diffeq(equation)
    equation = helper(equation, var)
    return solve(equation)
