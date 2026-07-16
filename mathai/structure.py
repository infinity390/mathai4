import copy
import itertools
from .simplify import simplify
from .base import *
from .parser import parse


def structure(
    formula,
    formula_out,
    ignore_list=[],
    var_name=None,
    const_1=[],
    const_2=[],
    forbidden_value={},
):
    varlist = {}
    equation = TreeNode("c_eq", [])
    def helper(equation, formula):
        nonlocal varlist, const_1, const_2, var_name, ignore_list
        
        # --- Compile-time Check for Variadic (more_children) Matching ---
        more_children = False
        if (
            formula.name in ["f_addw", "f_mulw"] 
            and formula.children 
            and formula.children[-1].name.startswith("v_") 
            and formula.children[-1].name not in ignore_list
        ):
            dic = {"const1": [], "const2": [], "other": []}
            for item in formula.children:
                if item.name.startswith("v_") and item.name not in ignore_list:
                    if item.name in const_1:
                        dic["const1"].append(item)
                    elif item.name in const_2:
                        dic["const2"].append(item)
                    else:
                        dic["other"].append(item)
                        
            if (
                (not dic["other"] and not dic["const1"] and formula.children[-1].name in const_2) or
                (not dic["other"] and dic["const1"] and formula.children[-1].name in const_1) or
                (dic["other"] and formula.children[-1].name not in const_1 + const_2)
            ):
                more_children = True

        # 1. Handle Variable Leaf Nodes (v_*)
        if formula.name.startswith("v_") and formula.name not in ignore_list:
            condition = []

            if var_name is not None and formula.name in const_1:
                condition.append(
                    TreeNode(
                        "f_if",
                        [equation.c_contains_arg(var_name), tree_form("s_false")],
                    )
                )

            if formula.name in const_2:
                condition.append(
                    TreeNode(
                        "f_if", [equation.c_contains_var(), tree_form("s_false")]
                    )
                )

            if formula.name in varlist.keys():
                match_node = TreeNode("f_==", [varlist[formula.name], equation])
            else:
                varlist[formula.name] = equation
                match_node = tree_form("s_true")

            if not condition:
                return match_node
            else:
                condition.append(match_node.fx("else"))
                return TreeNode("f_condition", condition)

        # 2. Setup Condition Expressions for Operators
        runtime_name = formula.name[:-1] if formula.name in ["f_addw", "f_mulw"] else formula.name
        eq_name_match = TreeNode("f_==", [equation.c_name(), TreeNode(f"c_'{runtime_name}'", [])])
        eq_len_match = TreeNode("f_==", [equation.c_length(), tree_form(f"d_{len(formula.children)}")])
        eq_len_greater = TreeNode("f_>", [equation.c_length(), tree_form(f"d_{len(formula.children)}")])

        # FIX 1: Snapshot varlist state before branch execution to prevent cross-contamination
        varlist_snapshot = varlist.copy()

        # --- Scenario A: Exact Length Match Recursion ---
        exact_lst = [
            helper(equation.c_child(i), formula.children[i])
            for i in range(len(formula.children))
        ]
        exact_match_action = tree_form("s_true") if not exact_lst else TreeNode("f_wand", exact_lst)

        # --- Scenario B: Variadic Loop Match Recursion (More Children) ---
        inner_conditions = [TreeNode("f_if", [eq_len_match, exact_match_action])]

        if more_children:
            # FIX 1 (cont.): Restore clean variable state for the alternative Variadic code path
            varlist = varlist_snapshot.copy()
            
            variadic_lst = []
            # Recursively process elements 0 through N-2 normally
            for i in range(len(formula.children) - 1):
                variadic_lst.append(helper(equation.c_child(i), formula.children[i]))
            
            N_minus_1 = len(formula.children) - 1
            eq_path = str(equation)
            
            # FIX 2: Generate a clean runtime slice representation string wrapped in a literal literal node wrapper
            packed_node = TreeNode(f"c_TreeNode('{runtime_name}', {eq_path}.children[{N_minus_1}:])", [])
            
            variadic_lst.append(helper(packed_node, formula.children[-1]))
            variadic_action = TreeNode("f_wand", variadic_lst)
            
            inner_conditions.append(TreeNode("f_if", [eq_len_greater, variadic_action]))

        # Inner fallthrough: Length mismatch fallback -> False
        inner_conditions.append(tree_form("s_false").fx("else"))
        inner_block = TreeNode("f_condition", inner_conditions)

        # Outer block execution wrapper
        outer_conditions = [
            TreeNode("f_if", [eq_name_match, inner_block]),
            tree_form("s_false").fx("else")
        ]
        
        return TreeNode("f_condition", outer_conditions)

    def lst(formula):
        out = set()
        formula = conversion(formula)

        def helper(node):
            if not node.children:
                return [node]
            child_groups = [tuple(node.children)]
            if node.name in ["f_addw", "f_mulw"]:
                child_groups = list(itertools.permutations(node.children))
            results = []
            for children in child_groups:
                child_perms = [helper(child) for child in children]
                for combo in itertools.product(*child_perms):
                    results.append(TreeNode(node.name, list(combo)))
            return results

        for tree in helper(formula):
            out.add(tree)
        return list(out)

    def conversion(node):
        if node.name == "f_add":
            node.name = "f_addw"
        elif node.name == "f_mul":
            node.name = "f_mulw"
        return TreeNode(
            node.name, [conversion(child) for child in node.children]
        )

    def var_replace(eq, var):
        nonlocal forbidden_value
        out = None
        if eq == var:
            return []
        for child in eq.children:
            out = var_replace(child, var)
            if isinstance(out, list):
                if eq.name == "f_add":
                    if (
                        var.name in forbidden_value
                        and forbidden_value[var.name] == 0
                    ):
                        return out
                    return out + [0]
                elif eq.name == "f_mul":
                    if (
                        var.name in forbidden_value
                        and forbidden_value[var.name] == 1
                    ):
                        return out
                    return out + [1]
                else:
                    return out
        return out

    formula_lst = {}
    ll = []
    for item in sorted(vlist(formula)):
        output = var_replace(formula, tree_form(item))
        if item not in const_1 + const_2:
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
            eq_var[list(sorted(vlist(formula)))[index]] = tree_form(
                f"d_{item2}"
            )
            eq_try = simplify(
                replace(
                    eq_try,
                    tree_form(list(sorted(vlist(formula)))[index]),
                    tree_form(f"d_{item2}"),
                )
            )
        formula_lst[eq_try] = eq_var
    formula_lst_2 = []
    for key, item in formula_lst.items():
        for item2 in lst(key):
            formula_lst_2.append((item2, item))
    final_output = {"f_add": [], "f_mul": [], "other": []}

    for item, update in formula_lst_2:
        varlist = copy.deepcopy(update)
        out = helper(copy.deepcopy(equation), item)
        d = []
        for key, item2 in varlist.items():
            if key in forbidden_value.keys():
                d.append(TreeNode("f_!=", [item2, tree_form(f"d_{forbidden_value[key]}")]))
        
        # FIX: Guard the evaluation of 'd' behind the successful structure match 'out'
        if len(d) == 0:
            pass 
        elif len(d) == 1:
            # Code output: (d[0] if out else False)
            out = TreeNode("f_condition", [
                TreeNode("f_if", [out, d[0]]), 
                tree_form("s_false").fx("else")
            ])
        else:
            # Code output: ((d[0] and d[1] and ...) if out else False)
            out = TreeNode("f_condition", [
                TreeNode("f_if", [out, TreeNode("f_wand", d)]), 
                tree_form("s_false").fx("else")
            ])
        
        # FIX 1: Safeguard rule generations by working on an isolated local copy
        local_formula_out = copy.deepcopy(formula_out)
        for key, item2 in varlist.items():
            local_formula_out = replace(
                local_formula_out, tree_form(key), item2
            )

        s = "if " + print_code(out) + ":\n"
        t = "\treturn " + print_code2(local_formula_out) + "\n"
        
        if item.name == "f_add":
            final_output["f_add"] += [s, t]
        elif item.name == "f_mul":
            final_output["f_mul"] += [s, t]
        else:
            final_output["other"] += [s, t]

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
        child = print_code_h(eq.children[0])
        return f"(not {child})"
    binary = {
        "f_==": "==",
        "f_!=": "!=",
        "f_wor": "or",
        "f_wand": "and",
        "f_>":">",
    }
    if eq.name in binary:
        op = f" {binary[eq.name]} "
        return "(" + op.join(print_code_h(c) for c in eq.children) + ")"
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
    if eq.name in ["f_pdif", "f_pow", "f_log", "f_dif"]:
        return f"TreeNode('{eq.name}', [{','.join(print_code2(c) for c in eq.children)}])"
    binary = {
        "f_==": "==",
        "f_!=": "!=",
        "f_>": ">",
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
    if eq.name == "f_addw":
        return TreeNode("f_add", eq.children)
    if eq.name == "f_mulw":
        return TreeNode("f_mul", eq.children)
    return eq


def de_w_addmul(eq):
    return transform_dfs(eq, de_w_addmul_h)


def print_code(eq):
    return (
        print_code_h(de_w_addmul(eq))
        .replace("f_addw", "f_add")
        .replace("f_mulw", "f_mul")
    )


def process_children(parent_node, validation_func):
    all_pairs = list(itertools.combinations(parent_node.children, 2))
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


def formula_complier(lst_formula):
    s = ""
    for item in ["f_add", "f_mul"]:
        s += f"def transform_{item[2:]}(eq):\n"
        if len(lst_formula[item]) == 0:
            s += "\tpass\n"
        else:
            for item2 in lst_formula[item]:
                s += "\t" + item2
    s += f"def transform_other(eq):\n"
    if len(lst_formula["other"]) > 0:
        for item in lst_formula["other"]:
            s += "\t" + item
    else:
        s += "\tpass\n"
    s += "def transform(eq):\n"
    s += "\tif eq.name == 'f_add':\n"
    s += "\t\tres = process_children(eq, transform_add)\n"
    s += "\telif eq.name == 'f_mul':\n"
    s += "\t\tres = process_children(eq, transform_mul)\n"
    s += "\telse:\n"
    s += "\t\tres = transform_other(eq)\n"
    s += "\tif res is not None:\n"
    s += "\t\treturn res\n"
    s += "\treturn res\n"
    env = {
        "tree_form": tree_form,
        "process_children": process_children,
        "TreeNode": TreeNode,
    }
    exec(s, env)
    return env["transform"]

def make_formula_function(lst):
    dic = {"f_add":[], "f_mul":[], "other":[]}
    for item in lst:
        out = structure(*item)
        for key, item2 in out.items():
            dic[key] += item2
    return formula_complier(dic)
