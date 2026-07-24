import copy
import itertools
from .simplify import simplify
from .base import *
from .parser import parse

def structure(
    formula,
    formula_out,
    ignore_list=None,
    var_name=None,
    const_1=None,
    forbidden_value=None,
    const_var=None,
    positive=None,
    negative=None
):
    # Safe default argument initialization
    if ignore_list is None:
        ignore_list = []
    if const_1 is None:
        const_1 = []
    if forbidden_value is None:
        forbidden_value = {}
    if const_var is None:
        const_var = []
    if positive is None:
        positive = []
    if negative is None:
        negative = []
    equation = TreeNode("c_eq", [])

    def helper(equation, formula, varlist=None):
        nonlocal const_1, var_name, ignore_list, const_var

        # Shared dict passed down so bindings persist across sub-expressions
        if varlist is None:
            varlist = {}

        # 1. Variable / Leaf Node Matching
        if formula.name.startswith("v_"):
            conditions = []

            # Check if parameter must be constant with respect to the bound integration variable
            if (formula.name in const_1 or formula.name in const_var) and formula.name not in ignore_list:
                target_var = varlist.get(var_name, var_name)
                conditions.append(
                    TreeNode(
                        "f_if",
                        [
                            equation.c_contains_arg(target_var),
                            tree_form("s_false"),
                        ],
                    )
                )

            # Bind pattern variable ('v_0', 'v_3', etc.) to current AST node or assert equality
            if formula.name in varlist:
                match = TreeNode("f_==", [varlist[formula.name], equation])
            else:
                varlist[formula.name] = equation
                match = tree_form("s_true")

            if conditions:
                conditions.append(match.fx("else"))
                return TreeNode("f_condition", conditions)

            return match

        # 2. Operator / Compound Node Configuration
        variadic_ops = {"f_addw", "f_mulw", "f_waddw", "f_hadamardw", "f_wmul"}
        strip_w_ops = {"f_addw", "f_mulw", "f_waddw", "f_hadamardw"}

        more_children = formula.name in variadic_ops
        runtime_name = (
            formula.name[:-1] if formula.name in strip_w_ops else formula.name
        )

        eq_name_match = TreeNode(
            "f_==", [equation.c_name(), TreeNode(f"c_'{runtime_name}'", [])]
        )

        # Recursively check children
        if formula.children:
            body = TreeNode(
                "f_wand",
                [
                    helper(equation.c_child(i), child, varlist)
                    for i, child in enumerate(formula.children)
                ],
            )
        else:
            body = tree_form("s_true")

        # 3. Arity Validation & c_group Runtime Partitioning
        if more_children:
            inner = TreeNode(
                "f_if",
                [
                    TreeNode(
                        "f_>=",
                        [
                            equation.c_length(),
                            tree_form(f"d_{len(formula.children)}"),
                        ],
                    ),
                    TreeNode(
                        "f_wor",
                        [
                            TreeNode(
                                "f_any",
                                [
                                    TreeNode(
                                        "f_genexpr",
                                        [
                                            body,  # yielded match body
                                            equation.c_group(  # iterator
                                                len(formula.children),
                                                [
                                                    (i, child.name)
                                                    for i, child in enumerate(
                                                        formula.children
                                                    )
                                                    if child.name.startswith("d_")
                                                    or child.name.startswith("s_")
                                                    or (
                                                        child.name.startswith("v_")
                                                        and (
                                                            child.name in ignore_list
                                                        )
                                                    )
                                                ],
                                                [
                                                    i
                                                    for i, child in enumerate(
                                                        formula.children
                                                    )
                                                    if child.name.startswith("v_") and child.name in const_1
                                                ],
                                                const_var,
                                            ),
                                        ],
                                    )
                                ],
                            ),
                            equation.c_restore(),
                        ],
                    ),
                ],
            )
        else:
            inner = TreeNode(
                "f_if",
                [
                    TreeNode(
                        "f_==",
                        [
                            equation.c_length(),
                            tree_form(f"d_{len(formula.children)}"),
                        ],
                    ),
                    body,
                ],
            )

        # Construct nested conditional branch AST using f_condition structure
        return TreeNode(
            "f_condition",
            [
                TreeNode(
                    "f_if",
                    [
                        eq_name_match,
                        TreeNode(
                            "f_condition",
                            [inner, tree_form("s_false").fx("else")],
                        ),
                    ],
                ),
                tree_form("s_false").fx("else"),
            ],
        )
    def lst(formula):
        out = set()
        formula = conversion(formula)

        def helper_lst(node):
            if not node.children:
                return [node]
            child_groups = [tuple(node.children)]
            if node.name in ["f_waddw", "f_hadamardw", "f_addw", "f_mulw"]:
                child_groups = list(itertools.permutations(node.children))
            results = []
            for children in child_groups:
                child_perms = [helper_lst(child) for child in children]
                for combo in itertools.product(*child_perms):
                    results.append(TreeNode(node.name, list(combo)))
            return results

        for tree in helper_lst(formula):
            out.add(tree)
        return list(out)

    def conversion(node):
        new_name = node.name + "w" if node.name in ["f_wadd", "f_hadamard", "f_add", "f_mul"] else node.name
        return TreeNode(
            new_name, [conversion(child) for child in node.children]
        )

    def var_replace(eq, var):
        nonlocal forbidden_value
        out = None
        if eq == var:
            return []
        for child in eq.children:
            out = var_replace(child, var)
            if isinstance(out, list):
                if eq.name in ["f_add"]:
                    if (
                        var.name in forbidden_value
                        and forbidden_value[var.name] == 0
                    ):
                        return out
                    return out + [0]
                elif eq.name in ["f_mul"]:
                    if (
                        var.name in forbidden_value
                        and forbidden_value[var.name] == 1
                    ):
                        return out
                    return out + [1]
                else:
                    return out
        return out

    # FIX 3: Store as list of tuples to avoid dictionary key overwrite collisions
    formula_lst = []
    ll = []
    sorted_vars = sorted(vlist(formula))
    for item in sorted_vars:
        output = var_replace(formula, tree_form(item))
        # FIX 1: Changed const_2 to const_var
        if item not in const_1 + const_var:
            output = []
        if output is not None:
            ll.append([-100] + list(set(output)))
        else:
            ll.append([-100])

    for item in itertools.product(*ll):
        eq_try = copy.deepcopy(formula)
        eq_var = {}
        for index, item2 in enumerate(item):
            if item2 == -100:
                continue
            var_key = sorted_vars[index]
            eq_var[var_key] = tree_form(f"d_{item2}")
            eq_try = simplify(
                replace(
                    eq_try,
                    tree_form(var_key),
                    tree_form(f"d_{item2}"),
                )
            )
        formula_lst.append((eq_try, eq_var))

    formula_lst_2 = []
    for key, item in formula_lst:
        for item2 in lst(key):
            formula_lst_2.append((item2, item))

    final_output = {
        "f_add": [],
        "f_mul": [],
        "f_wadd": [],
        "f_hadamard": [],
        "f_wmul": [],
        "other": []
    }
    seen_rules = {k: set() for k in final_output}

    for item, update in formula_lst_2:
        varlist = copy.deepcopy(update)

        # Pass varlist so helper populates variable bindings (e.g., v_3 -> eq.children[0].children[0])
        out = helper(copy.deepcopy(equation), item, varlist)

        d = []
        for key, item2 in varlist.items():
            if key in forbidden_value:
                d.append(TreeNode("f_!=", [item2, tree_form(f"d_{forbidden_value[key]}")]))
            if key in positive:
                d.append(item2.c_is_positive())
            if key in negative:
                d.append(item2.c_is_negative())

        # Guard the evaluation of 'd' behind the successful structure match 'out'
        if len(d) == 0:
            pass
        elif len(d) == 1:
            out = TreeNode("f_condition", [
                TreeNode("f_if", [out, d[0]]),
                tree_form("s_false").fx("else")
            ])
        else:
            out = TreeNode("f_condition", [
                TreeNode("f_if", [out, TreeNode("f_wand", d)]),
                tree_form("s_false").fx("else")
            ])

        # Substitute matched eq children access paths into output formula
        local_formula_out = copy.deepcopy(formula_out)
        for key, item2 in varlist.items():
            local_formula_out = replace(
                local_formula_out, tree_form(key), item2
            )

        s = "if " + print_code(out) + ":\n"
        u = "\t\n"
        t = "\treturn " + print_code2(local_formula_out) + "\n"

        # FIX 2: Map runtime operator names (e.g., 'f_addw') back to final_output dictionary keys
        strip_w_ops = {"f_addw", "f_mulw", "f_waddw", "f_hadamardw"}
        base_name = item.name[:-1] if item.name in strip_w_ops else item.name
        target_key = base_name if base_name in final_output else "other"

        # Deduplicate identical if/return blocks generated by symmetric permutations
        if (s, u, t) not in seen_rules[target_key]:
            seen_rules[target_key].add((s, u, t))
            final_output[target_key].extend([s, u, t])

    return final_output

def print_condition(eq):
    assert eq.name == "f_condition"

    def emit(i):
        branch = eq.children[i]

        if branch.name == "f_else":
            return print_code_h(branch.children[0])

        cond = print_code_h(branch.children[0])
        value = print_code_h(branch.children[1])
        rest = emit(i + 1)

        return f"({value} if {cond} else \\\n" f" {rest})"

    return emit(0)


def print_code_h(eq):
    if eq.name == "s_true":
        return "True"

    if eq.name == "s_false":
        return "False"

    if eq.name == "f_not":
        return f"(not {print_code_h(eq.children[0])})"

    binary = {
        "f_==": "==",
        "f_!=": "!=",
        "f_wor": "or",
        "f_wand": "and",
        "f_>": ">",
        "f_>=": ">=",
    }

    if eq.name in binary:
        op = f" {binary[eq.name]} "
        return "(" + op.join(print_code_h(c) for c in eq.children) + ")"
    if eq.name == "f_index":
        return f"{print_code_h(eq.children[0])}[{eq.children[1]}]"
    if eq.name == "f_any":
        return f"any({print_code_h(eq.children[0])})"

    if eq.name == "f_list":
        return "[" + ", ".join(print_code_h(c) for c in eq.children) + "]"

    if eq.name == "f_genexpr":
        return f"({print_code_h(eq.children[0])} for _ in {print_code_h(eq.children[1])})"

    if eq.name == "f_condition":
        return print_condition(eq)
    
    if not eq.children:
        return eq.name[2:]

    return (
        f"{eq.name}("
        + ", ".join(print_code_h(c) for c in eq.children)
        + ")"
    )

def print_code2(eq):
    if (
        eq.name.startswith("d_")
        or eq.name.startswith("v_")
        or eq.name.startswith("s_")
    ):
        return f"tree_form('{eq.name}')"
    if eq.name == "s_true":
        return "True"
    if eq.name == "s_false":
        return "False"
    # FIX 2: Resolved the NameError context crash for square root transformations
    if eq.name == "f_sqrt":
        child = print_code2(eq.children[0])
        return f"{child}.fx('sqrt')"
    if eq.name == "f_not":
        child = print_code2(eq.children[0])
        return f"~{child}"
    if eq.name in ["f_pdif", "f_pow", "f_log", "f_dif", "f_wadd", "f_hadamard", "f_wmul"]:
        return f"TreeNode('{eq.name}', [{','.join(print_code2(c) for c in eq.children)}])"
    binary = {
        "f_==": "==",
        "f_!=": "!=",
        "f_>": ">",
        "f_>=": ">=",
        "f_wor": "or",
        "f_pow": "**",
        "f_wand": "and",
        "f_mul": "*",
        "f_add": "+"
    }
    if eq.name in binary:
        op = f" {binary[eq.name]} "
        return "(" + op.join(print_code2(c) for c in eq.children) + ")"
    if eq.name == "f_condition":
        return print_condition(eq)
    if not eq.children:
        return eq.name[2:]
    return (
        f"TreeNode('{eq.name}',["
        + ", ".join(print_code2(c) for c in eq.children)
        + "])"
    )


def de_w_addmul_h(eq):
    if eq.name in ["f_addw", "f_mulw", "f_waddw", "f_hadamardw"]:
        return TreeNode(eq.name[:-1], eq.children)
    return eq


def de_w_addmul(eq):
    return transform_dfs(eq, de_w_addmul_h)


def print_code(eq):
    out = print_code_h(de_w_addmul(eq))
    for item in ["f_addw", "f_mulw", "f_waddw", "f_hadamardw"]:
        out = out.replace(item, item[:-1])
    return out

def process_children(parent_node, validation_func, commutative=True):
    all_pairs = None
    if commutative:
        all_pairs = list(itertools.combinations(parent_node.children, 2))
    else:
        all_pairs = [list(pair) for pair in zip(parent_node.children, parent_node.children[1:])]
    nodes_to_remove = set()
    new_nodes_to_add = []
    
    for c1, c2 in all_pairs:
        if c1 in nodes_to_remove or c2 in nodes_to_remove:
            continue
        new_node = validation_func(TreeNode(parent_node.name, [c1, c2]))
        if new_node is not None:
            nodes_to_remove.add(c1)
            nodes_to_remove.add(c2)
            new_nodes_to_add.append(new_node)
            
    parent_node.children = [
        c for c in parent_node.children if c not in nodes_to_remove
    ]
    parent_node.children.extend(new_nodes_to_add)
    
    # FIX: If the node is left with a single child, strip the redundant parent shell
    if len(parent_node.children) == 1:
        return parent_node.children[0]
        
    return parent_node


def formula_compiler(lst_formula):
    s = ""
    for item in ["f_add", "f_mul", "f_wadd", "f_hadamard", "f_wmul"]:
        s += f"def transform_{item[2:]}(eq):\n"
        if len(lst_formula[item]) == 0:
            s += "\tpass\n"
        else:
            for index, item2 in enumerate(lst_formula[item]):
                s += "\t" + item2
    s += f"def transform_other(eq):\n"
    if len(lst_formula["other"]) > 0:
        for index, item in enumerate(lst_formula["other"]):
            s += "\t" + item
    else:
        s += "\tpass\n"
    s += "def transform(eq):\n"
    s += "\tres = None\n"
    s += f"\tif eq.name == 'f_add':\n"
    s += f"\t\tres = process_children(eq, transform_add)\n"
    for item in ["f_mul", "f_wadd", "f_hadamard"]:
        s += f"\telif eq.name == '{item}':\n"
        s += f"\t\tres = process_children(eq, transform_{item[2:]})\n"
    for item in ["f_wmul"]:
        s += f"\telif eq.name == '{item}':\n"
        s += f"\t\tres = process_children(eq, transform_{item[2:]}, False)\n"
    s += "\telse:\n"
    s += "\t\tres = transform_other(eq)\n"
    s += "\tif res is None:\n"
    s += "\t\treturn eq\n"
    s += "\treturn res\n"
    env = {
        "tree_form": tree_form,
        "process_children": process_children,
        "TreeNode": TreeNode,
    }
    exec(s, env)
    return env["transform"]

def formula_list_compiler(lst):
    dic = {"f_add": [], "f_mul": [], "f_wadd": [], "f_hadamard": [], "f_wmul":[], "other": []}
    for item in lst:
        out = structure(*item)
        for key, item2 in out.items():
            dic[key] += item2
    return formula_compiler(dic)
