from .simplify import simplify
from .base import *
from .parser import parse
from .formula_data import load_formula

diff2 = lambda x: load_formula("differentiation")(x)
def diff(equation, var=None):
    var = var if var else "v_0"
    return diff2(TreeNode("f_pdif", [equation, tree_form(var)]))
