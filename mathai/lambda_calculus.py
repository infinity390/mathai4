from .parser import parse
from .base import *

# --- CHURCH DEFINITIONS ---
TRUE  = TreeNode("f_lambda", [TreeNode("v_0", []), TreeNode("f_lambda", [TreeNode("v_1", []), TreeNode("v_0", [])])])
FALSE = TreeNode("f_lambda", [TreeNode("v_0", []), TreeNode("f_lambda", [TreeNode("v_1", []), TreeNode("v_1", [])])])

AND   = TreeNode("f_lambda", [TreeNode("v_0", []), TreeNode("f_lambda", [TreeNode("v_1", []), TreeNode("f_apply", [TreeNode("f_apply", [TreeNode("v_0", []), TreeNode("v_1", [])]), TreeNode("v_0", [])])])])
OR    = TreeNode("f_lambda", [TreeNode("v_0", []), TreeNode("f_lambda", [TreeNode("v_1", []), TreeNode("f_apply", [TreeNode("f_apply", [TreeNode("v_0", []), TreeNode("v_0", [])]), TreeNode("v_1", [])])])])
NOT   = TreeNode("f_lambda", [TreeNode("v_0", []), TreeNode("f_apply", [TreeNode("f_apply", [TreeNode("v_0", []), FALSE]), TRUE])])

SUCC  = TreeNode("f_lambda", [TreeNode("v_0", []), TreeNode("f_lambda", [TreeNode("v_1", []), TreeNode("f_lambda", [TreeNode("v_2", []), TreeNode("f_apply", [TreeNode("v_1", []), TreeNode("f_apply", [TreeNode("f_apply", [TreeNode("v_0", []), TreeNode("v_1", [])]), TreeNode("v_2", [])])])])])])
PLUS  = TreeNode("f_lambda", [TreeNode("v_0", []), TreeNode("f_lambda", [TreeNode("v_1", []), TreeNode("f_apply", [TreeNode("f_apply", [TreeNode("v_0", []), SUCC]), TreeNode("v_1", [])])])])
MULT  = TreeNode("f_lambda", [TreeNode("v_0", []), TreeNode("f_lambda", [TreeNode("v_1", []), TreeNode("f_lambda", [TreeNode("v_2", []), TreeNode("f_apply", [TreeNode("v_0", []), TreeNode("f_apply", [TreeNode("v_1", []), TreeNode("v_2", [])])])])])])

# Pure Lambda Definition: λf. (λx. f (λv. x x v)) (λx. f (λv. x x v))
delay_wrapper = TreeNode("f_lambda", [
    TreeNode("v_2", []),
    TreeNode("f_apply", [
        TreeNode("f_apply", [TreeNode("v_1", []), TreeNode("v_1", [])]),
        TreeNode("v_2", [])
    ])
])

combinator_branch = TreeNode("f_lambda", [
    TreeNode("v_1", []),
    TreeNode("f_apply", [TreeNode("v_0", []), delay_wrapper])
])

Z_COMBINATOR = TreeNode("f_lambda", [
    TreeNode("v_0", []),
    TreeNode("f_apply", [combinator_branch, combinator_branch])
])

# PRED (Church Predecessor): λn.λf.λx. n (λg.λh. h (g f)) (λu.x) (λu.u)
PRED = TreeNode("f_lambda", [
    TreeNode("v_0", []), TreeNode("f_lambda", [
    TreeNode("v_1", []), TreeNode("f_lambda", [
    TreeNode("v_2", []), TreeNode("f_apply", [
        TreeNode("f_apply", [
            TreeNode("f_apply", [
                TreeNode("v_0", []), TreeNode("f_lambda", [
                TreeNode("v_3", []), TreeNode("f_lambda", [
                TreeNode("v_4", []), TreeNode("f_apply", [
                    TreeNode("v_4", []), TreeNode("f_apply", [TreeNode("v_3", []), TreeNode("v_1", [])])
                ])])])
            ]),
            TreeNode("f_lambda", [TreeNode("v_5", []), TreeNode("v_2", [])])
        ]),
        TreeNode("f_lambda", [TreeNode("v_5", []), TreeNode("v_5", [])])
    ])])])
])

church_def = {
    "s_true": TRUE, 
    "s_false": FALSE, 
    "f_and": AND,
    "f_or": OR,
    "f_not": NOT,
    "f_successor": SUCC,
    "f_add": PLUS,
    "f_mul": MULT
}

# v_0 = recurse function, v_1 = current number n
sum_blueprint = TreeNode("f_lambda", [
    TreeNode("v_0", []), 
    TreeNode("f_lambda", [
        TreeNode("v_1", []), 
        
        # Church Conditional Application: (Condition TrueBranch FalseBranch)
        TreeNode("f_apply", [
            TreeNode("f_apply", [
                # 1. Condition: is_zero(n)
                TreeNode("f_apply", [TreeNode("f_iszero", []), TreeNode("v_1", [])]),
                # 2. True Branch: return 0
                TreeNode("d_0", [])
            ]),
            # 3. False Branch: add(n, recurse(pred(n)))
            TreeNode("f_apply", [
                TreeNode("f_apply", [TreeNode("f_add", []), TreeNode("v_1", [])]),
                TreeNode("f_apply", [
                    TreeNode("v_0", []), 
                    TreeNode("f_apply", [TreeNode("f_pred", []), TreeNode("v_1", [])])
                ])
            ])
        ])
    ])
])

# ISZERO: λn. n (λx. FALSE) TRUE
ISZERO = TreeNode("f_lambda", [
    TreeNode("v_0", []),
    TreeNode("f_apply", [
        TreeNode("f_apply", [
            TreeNode("v_0", []),
            TreeNode("f_lambda", [TreeNode("v_1", []), church_def["s_false"]])
        ]),
        church_def["s_true"]
    ])
])

church_def["f_ycombinator"] = Z_COMBINATOR
church_def["f_iszero"] = ISZERO
church_def["f_pred"] = PRED
church_def["f_lsum"] = PRED

def make_church_numeral(n):
    body = TreeNode("v_1", [])
    for _ in range(n):
        body = TreeNode("f_apply", [TreeNode("v_0", []), body])
    inner_lambda = TreeNode("f_lambda", [TreeNode("v_1", []), body])
    outer_lambda = TreeNode("f_lambda", [TreeNode("v_0", []), inner_lambda])
    return outer_lambda

for i in range(10):
    church_def[f"d_{i}"] = make_church_numeral(i)
    
class FreshGen:
    def __init__(self):
        self.counter = 0
    def fresh(self):
        self.counter += 1
        return f"v_fresh_{self.counter}"

# --- SEMANTIC PATTERN RECOVERY DETECTORS ---

def detect_church_numeral(node):
    if node.name != "f_lambda": return None
    f_var = node.children[0].name
    body1 = node.children[1]
    if body1.name != "f_lambda": return None
    x_var = body1.children[0].name
    
    curr = body1.children[1]
    count = 0
    while curr.name == "f_apply":
        func, arg = curr.children[0], curr.children[1]
        if func.name != f_var or func.children: return None
        count += 1
        curr = arg
        
    if curr.name == x_var and not curr.children:
        return count
    return None

def detect_add_n(node):
    if node.name != "f_lambda": return None
    m_var = node.children[0].name
    body1 = node.children[1]
    if body1.name != "f_lambda": return None
    f_var = body1.children[0].name
    body2 = body1.children[1]
    if body2.name != "f_lambda": return None
    x_var = body2.children[0].name
    
    curr = body2.children[1]
    count = 0
    while curr.name == "f_apply":
        func, arg = curr.children[0], curr.children[1]
        if func.name == f_var and not func.children:
            count += 1
            curr = arg
        else:
            break
            
    if curr.name == "f_apply":
        func1, x_arg = curr.children[0], curr.children[1]
        if x_arg.name == x_var and not x_arg.children:
            if func1.name == "f_apply":
                m_arg, f_arg = func1.children[0], func1.children[1]
                if m_arg.name == m_var and not m_arg.children and f_arg.name == f_var and not f_arg.children:
                    return count
    return None

def detect_mul_n(node):
    if node.name != "f_lambda": return None
    m_var = node.children[0].name
    body1 = node.children[1]
    if body1.name != "f_lambda": return None
    x_var = body1.children[0].name
    body2 = body1.children[1]
    if body2.name != "f_lambda": return None
    z_var = body2.children[0].name
    
    curr = body2.children[1]
    count = 0
    while curr.name == "f_apply":
        func, arg = curr.children[0], curr.children[1]
        if func.name == "f_apply" and func.children[0].name == m_var and func.children[1].name == x_var:
            count += 1
            curr = arg
        else:
            break
    if curr.name == z_var and not curr.children:
        return count
    return None

# --- ENGINE LOGIC (ITERATIVE / STACK BASED) ---

def is_free(var_name, node):
    stack = [node]
    while stack:
        curr = stack.pop()
        if curr.name == "f_lambda":
            if curr.children[0].name != var_name:
                stack.append(curr.children[1])
        elif curr.name == "f_apply":
            stack.append(curr.children[1])
            stack.append(curr.children[0])
        elif not curr.children:
            if curr.name == var_name:
                return True
        else:
            for c in reversed(curr.children):
                stack.append(c)
    return False

def eta_reduce(initial_node):
    stack = [(initial_node, 0, None)]
    res_stack = []
    while stack:
        node, phase, extra = stack.pop()
        if phase == 0:
            if node.name == "f_lambda":
                stack.append((node, 1, None))
                stack.append((node.children[1], 0, None))
            elif node.name == "f_apply":
                stack.append((node, 2, None))
                stack.append((node.children[1], 0, None))
                stack.append((node.children[0], 0, None))
            else:
                res_stack.append(node)
        elif phase == 1:
            body = res_stack.pop()
            bound_var = node.children[0].name
            if body.name == "f_apply" and not body.children[1].children and body.children[1].name == bound_var:
                func = body.children[0]
                if not is_free(bound_var, func):
                    res_stack.append(func)
                    continue
            res_stack.append(TreeNode("f_lambda", [node.children[0], body]))
        elif phase == 2:
            arg = res_stack.pop()
            func = res_stack.pop()
            res_stack.append(TreeNode("f_apply", [func, arg]))
    return res_stack[0]

def to_lambda(eq):
    stack = [(eq, 0)]
    res_stack = []
    while stack:
        node, phase = stack.pop()
        if phase == 0:
            stack.append((node, 1))
            for child in reversed(node.children):
                stack.append((child, 0))
        else:
            n_child = len(node.children)
            children_res = [res_stack.pop() for _ in range(n_child)]
            children_res.reverse()
            
            if node.name in church_def:
                base = church_def[node.name]
                for c_res in children_res:
                    base = TreeNode("f_apply", [base, c_res])
                res_stack.append(base)
            else:
                res_stack.append(TreeNode(node.name, children_res))
    return res_stack[0]

def subst(initial_node, initial_var_name, initial_replacement, fg):
    stack = [(initial_node, initial_var_name, initial_replacement, 0, None)]
    res_stack = []
    while stack:
        node, var_name, replacement, phase, extra = stack.pop()
        if phase == 0:
            if node.name == "f_lambda":
                bound_var = node.children[0].name
                if bound_var == var_name:
                    res_stack.append(node)
                else:
                    new_bound = fg.fresh()
                    stack.append((node, var_name, replacement, 1, (new_bound, bound_var)))
                    stack.append((node.children[1], bound_var, TreeNode(new_bound, []), 0, None))
            elif node.name == "f_apply":
                stack.append((node, var_name, replacement, 3, 2))
                stack.append((node.children[1], var_name, replacement, 0, None))
                stack.append((node.children[0], var_name, replacement, 0, None))
            elif not node.children:
                if node.name == var_name:
                    res_stack.append(replacement)
                else:
                    res_stack.append(node)
            else:
                n_child = len(node.children)
                stack.append((node, var_name, replacement, 3, n_child))
                for child in reversed(node.children):
                    stack.append((child, var_name, replacement, 0, None))
        elif phase == 1:
            new_bound, bound_var = extra
            renamed_body = res_stack.pop()
            stack.append((node, var_name, replacement, 2, new_bound))
            stack.append((renamed_body, var_name, replacement, 0, None))
        elif phase == 2:
            new_bound = extra
            final_body = res_stack.pop()
            res_stack.append(TreeNode("f_lambda", [TreeNode(new_bound, []), final_body]))
        elif phase == 3:
            n_child = extra
            children_res = [res_stack.pop() for _ in range(n_child)]
            children_res.reverse()
            res_stack.append(TreeNode(node.name, children_res))
    return res_stack[0]

def beta_reduce(initial_eq, fg):
    stack = [(initial_eq, 0, None)]
    res_stack = []
    while stack:
        eq, phase, extra = stack.pop()
        if phase == 0:
            if eq.name == "f_apply":
                stack.append((eq, 1, None))
                stack.append((eq.children[1], 0, None))
                stack.append((eq.children[0], 0, None))
            elif eq.name == "f_lambda":
                stack.append((eq, 2, None))
                stack.append((eq.children[1], 0, None))
            else:
                res_stack.append(eq)
        elif phase == 1:
            arg = res_stack.pop()
            func = res_stack.pop()
            if func.name == "f_lambda":
                substituted = subst(func.children[1], func.children[0].name, arg, fg)
                stack.append((substituted, 0, None))
            else:
                res_stack.append(TreeNode("f_apply", [func, arg]))
        elif phase == 2:
            body = res_stack.pop()
            res_stack.append(TreeNode("f_lambda", [eq.children[0], body]))
    return res_stack[0]

def normalize_structure(initial_node, env=None, counter=None):
    if env is None: env = {}
    if counter is None: counter = [0]
    stack = [(initial_node, 0, None)]
    res_stack = []
    while stack:
        node, phase, extra = stack.pop()
        if phase == 0:
            if node.name == "f_lambda":
                var_name = node.children[0].name
                new_var_name = f"var_{counter[0]}"
                counter[0] += 1
                old_val = env.get(var_name)
                env[var_name] = new_var_name
                stack.append((node, 1, (new_var_name, var_name, old_val)))
                stack.append((node.children[1], 0, None))
            elif node.name == "f_apply":
                stack.append((node, 2, None))
                stack.append((node.children[1], 0, None))
                stack.append((node.children[0], 0, None))
            elif not node.children:
                res_stack.append(TreeNode(env.get(node.name, node.name), []))
            else:
                n_child = len(node.children)
                stack.append((node, 3, n_child))
                for child in reversed(node.children):
                    stack.append((child, 0, None))
        elif phase == 1:
            new_var_name, var_name, old_val = extra
            norm_body = res_stack.pop()
            if old_val is not None:
                env[var_name] = old_val
            else:
                if var_name in env: del env[var_name]
            res_stack.append(TreeNode("f_lambda", [TreeNode(new_var_name, []), norm_body]))
        elif phase == 2:
            arg = res_stack.pop()
            func = res_stack.pop()
            res_stack.append(TreeNode("f_apply", [func, arg]))
        elif phase == 3:
            n_child = extra
            children_res = [res_stack.pop() for _ in range(n_child)]
            children_res.reverse()
            res_stack.append(TreeNode(node.name, children_res))
    return res_stack[0]

def trees_match(n1, n2):
    stack = [(n1, n2)]
    while stack:
        p1, p2 = stack.pop()
        if p1.name != p2.name or len(p1.children) != len(p2.children):
            return False
        for c1, c2 in zip(reversed(p1.children), reversed(p2.children)):
            stack.append((c1, c2))
    return True

def from_lambda(initial_eq):
    stack = [(initial_eq, 0)]
    res_stack = []
    while stack:
        eq, phase = stack.pop()
        if phase == 0:
            reduced_eq = eta_reduce(eq)
            norm_eq = normalize_structure(reduced_eq)
            
            matched = False
            for key, item in church_def.items():
                if trees_match(norm_eq, normalize_structure(item)):
                    res_stack.append(TreeNode(key, []))
                    matched = True
                    break
            if matched: continue
            
            num = detect_church_numeral(norm_eq)
            if num is not None:
                res_stack.append(TreeNode(f"d_{num}", []))
                continue
                
            add_n = detect_add_n(norm_eq)
            if add_n is not None:
                res_stack.append(TreeNode("f_apply", [TreeNode("f_add", []), TreeNode(f"d_{add_n}", [])]))
                continue
                
            mul_n = detect_mul_n(norm_eq)
            if mul_n is not None:
                res_stack.append(TreeNode("f_apply", [TreeNode("f_mul", []), TreeNode(f"d_{mul_n}", [])]))
                continue
            
            stack.append((eq, 1))
            for child in reversed(eq.children):
                stack.append((child, 0))
        else:
            n_child = len(eq.children)
            children_res = [res_stack.pop() for _ in range(n_child)]
            children_res.reverse()
            res_stack.append(TreeNode(eq.name, children_res))
    return res_stack[0]

def church(eq):
    fg = FreshGen()
    expanded = to_lambda(eq)
    reduced = dowhile(expanded, lambda e: beta_reduce(e, fg))
    return from_lambda(reduced)
