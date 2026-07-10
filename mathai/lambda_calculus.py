from .parser import parse
from .base import *
TRUE  = TreeNode("f_lambda", [TreeNode("v_0", []), TreeNode("f_lambda", [TreeNode("v_1", []), TreeNode("v_0", [])])])
FALSE = TreeNode("f_lambda", [TreeNode("v_0", []), TreeNode("f_lambda", [TreeNode("v_1", []), TreeNode("v_1", [])])])
AND   = TreeNode("f_lambda", [TreeNode("v_0", []), 
          TreeNode("f_lambda", [TreeNode("v_1", []), 
            TreeNode("f_apply", [TreeNode("f_apply", [TreeNode("v_0", []), TreeNode("v_1", [])]), TreeNode("v_0", [])])
          ])
        ])
OR    = TreeNode("f_lambda", [TreeNode("v_0", []), 
          TreeNode("f_lambda", [TreeNode("v_1", []), 
            TreeNode("f_apply", [TreeNode("f_apply", [TreeNode("v_0", []), TreeNode("v_0", [])]), TreeNode("v_1", [])])
          ])
        ])
NOT   = TreeNode("f_lambda", [TreeNode("v_0", []), 
          TreeNode("f_apply", [TreeNode("f_apply", [TreeNode("v_0", []), FALSE]), TRUE])
        ])
ZERO  = TreeNode("f_lambda", [TreeNode("v_0", []), TreeNode("f_lambda", [TreeNode("v_1", []), TreeNode("v_1", [])])])
ONE   = TreeNode("f_lambda", [TreeNode("v_0", []), TreeNode("f_lambda", [TreeNode("v_1", []), TreeNode("f_apply", [TreeNode("v_0", []), TreeNode("v_1", [])])])])
TWO   = TreeNode("f_lambda", [TreeNode("v_0", []), TreeNode("f_lambda", [TreeNode("v_1", []), TreeNode("f_apply", [TreeNode("v_0", []), TreeNode("f_apply", [TreeNode("v_0", []), TreeNode("v_1", [])])])])])
THREE = TreeNode("f_lambda", [TreeNode("v_0", []), TreeNode("f_lambda", [TreeNode("v_1", []), TreeNode("f_apply", [TreeNode("v_0", []), TreeNode("f_apply", [TreeNode("v_0", []), TreeNode("f_apply", [TreeNode("v_0", []), TreeNode("v_1", [])])])])])])
SUCC  = TreeNode("f_lambda", [TreeNode("v_0", []), 
          TreeNode("f_lambda", [TreeNode("v_1", []), 
            TreeNode("f_lambda", [TreeNode("v_2", []), 
              TreeNode("f_apply", [TreeNode("v_1", []), TreeNode("f_apply", [TreeNode("f_apply", [TreeNode("v_0", []), TreeNode("v_1", [])]), TreeNode("v_2", [])])])
            ])
          ])
        ])
PLUS  = TreeNode("f_lambda", [TreeNode("v_0", []), 
          TreeNode("f_lambda", [TreeNode("v_1", []), 
            TreeNode("f_apply", [TreeNode("f_apply", [TreeNode("v_0", []), SUCC]), TreeNode("v_1", [])])
          ])
        ])
MULT  = TreeNode("f_lambda", [TreeNode("v_0", []), 
          TreeNode("f_lambda", [TreeNode("v_1", []), 
            TreeNode("f_lambda", [TreeNode("v_2", []), 
              TreeNode("f_apply", [TreeNode("v_0", []), TreeNode("f_apply", [TreeNode("v_1", []), TreeNode("v_2", [])])])
            ])
          ])
        ])
church_def = {
    "s_true": TRUE, 
    "s_false": FALSE, 
    "f_and": AND,
    "f_or": OR,
    "f_not": NOT,
    "d_0": ZERO,
    "d_1": ONE,
    "d_2": TWO,
    "d_3": THREE,
    "f_successor": SUCC,
    "f_add": PLUS,
    "f_mul": MULT
}
class FreshGen:
    def __init__(self):
        self.counter = 0
    def fresh(self):
        self.counter += 1
        return f"v_fresh_{self.counter}"

def subst(node, var_name, replacement, fg):
    if node.name == "f_lambda":
        bound_var = node.children[0].name
        if bound_var == var_name:
            return node
        new_bound = fg.fresh()
        renamed_body = subst(node.children[1], bound_var, TreeNode(new_bound, []), fg)
        return TreeNode("f_lambda", [TreeNode(new_bound, []), subst(renamed_body, var_name, replacement, fg)])
    if node.name == "f_apply":
        return TreeNode("f_apply", [subst(node.children[0], var_name, replacement, fg), subst(node.children[1], var_name, replacement, fg)])
    if node.children == []:
        if node.name == var_name:
            return replacement
        return node
    return TreeNode(node.name, [subst(c, var_name, replacement, fg) for c in node.children])

def beta_reduce(eq, fg):
    if eq.name == "f_apply":
        func = beta_reduce(eq.children[0], fg)
        arg = beta_reduce(eq.children[1], fg)
        if func.name == "f_lambda":
            return beta_reduce(subst(func.children[1], func.children[0].name, arg, fg), fg)
        return TreeNode("f_apply", [func, arg])
    if eq.name == "f_lambda":
        return TreeNode("f_lambda", [eq.children[0], beta_reduce(eq.children[1], fg)])
    return eq

def normalize_structure(node, env=None, counter=None):
    if env is None: env = {}
    if counter is None: counter = [0]
    if node.name == "f_lambda":
        var_name = node.children[0].name
        new_var_name = f"var_{counter[0]}"
        counter[0] += 1
        old_val = env.get(var_name)
        env[var_name] = new_var_name
        norm_body = normalize_structure(node.children[1], env, counter)
        if old_val is not None: env[var_name] = old_val
        else: del env[var_name]
        return TreeNode("f_lambda", [TreeNode(new_var_name, []), norm_body])
    if node.name == "f_apply":
        return TreeNode("f_apply", [normalize_structure(node.children[0], env, counter), normalize_structure(node.children[1], env, counter)])
    if node.children == []:
        return TreeNode(env.get(node.name, node.name), [])
    return TreeNode(node.name, [normalize_structure(c, env, counter) for c in node.children])

def trees_match(n1, n2):
    if n1.name != n2.name or len(n1.children) != len(n2.children):
        return False
    return all(trees_match(c1, c2) for c1, c2 in zip(n1.children, n2.children))

def to_lambda(eq):
    for key, item in church_def.items():
        if eq.name == key:
            return item
    return TreeNode(eq.name, [to_lambda(child) for child in eq.children])

def from_lambda(eq):
    norm_eq = normalize_structure(eq)
    for key, item in church_def.items():
        if trees_match(norm_eq, normalize_structure(item)):
            return tree_form(key)
    return TreeNode(eq.name, [from_lambda(child) for child in eq.children])

def church(eq):
    fg = FreshGen()
    expanded = to_lambda(eq)
    reduced = dowhile(expanded, lambda e: beta_reduce(e, fg))
    return from_lambda(reduced)
