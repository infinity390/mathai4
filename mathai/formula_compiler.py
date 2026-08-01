import copy
import itertools
from .simplify import simplify
from .base import *
from .parser import parse
from fractions import Fraction
def structure(
    formula,
    formula_out,
    ignore_list=None,
    var_name=None,
    const_1=None,
    const_var=None,
    forbidden_value=None,
    positive=None,
    negative=None,
    arity=6
):
    if ignore_list is None:
        ignore_list = []
    if not isinstance(ignore_list, list):
        ignore_list = [ignore_list]

    if var_name is None:
        var_name = []
    if not isinstance(var_name, list):
        var_name = [var_name]
     
    if const_1 is None:
        const_1 = []
    if not isinstance(const_1, list):
        const_1 = [const_1]

    if const_var is None:
        const_var = []
    if not isinstance(const_var, list):
        const_var = [const_var]
    
    if forbidden_value is None:
        forbidden_value = []
    if len(forbidden_value)>0 and not isinstance(forbidden_value[0],list):
        forbidden_value = [forbidden_value]
    
    if positive is None:
        positive = []
    if not isinstance(positive, list):
        positive = [positive]
        
    if negative is None:
        negative = []
    if not isinstance(negative, list):
        negative = [negative]

    var_name = [item.name for item in var_name]
    const_var = [item.name for item in const_var]
    const_1 = [item.name for item in const_1]
    ignore_list = [item.name for item in ignore_list]
    
    equation = TreeNode("eq", [])
    def strip_w(s):
        if s in ["f_addw", "f_mulw", "f_hadamardw", "f_waddw"]:
            return s[:-1]
        return s
    def make_treenode_string(name, children):
        return f"TreeNode('{name}',[{','.join(children)}])"
    def helper2(equation, formula):
        nonlocal ignore_list, varlist, associativity, ignore
        if formula.name.startswith("v_") and formula.name in const_1:
            pass
        elif formula.name.startswith("v_") and formula.name in ignore_list:
            if formula.name.startswith("v_") and formula.name in varlist.keys():
                pass
            elif formula.name.startswith("v_") and formula.name not in varlist.keys():
                varlist[formula.name] = equation
                ignore.append(equation.name)
        elif formula.name.startswith("v_"):
            pass
        else:
            s = 0
            for key in formula.children:
                if key.name in associativity.keys():
                    s += associativity[key.name]
                else:
                    s += 1
            children = [f"{equation.name}.children[{i}]" for i in range(s)]
            new_children = []
            for key in formula.children:
                if key.name in associativity.keys():
                    n = associativity[key.name]
                    if n == 1:
                        new_children.append(children.pop(0))
                    else:
                        new_children.append(make_treenode_string(strip_w(formula.name), children[:n]))
                        children = children[n:]
                else:
                    new_children.append(children.pop(0))
            for i in range(len(formula.children)):
                helper2(TreeNode(new_children[i],[]),formula.children[i])
    def helper(equation, formula):
        nonlocal const_1, var_name, ignore_list, const_var, varlist, associativity, ignore
        if formula.name.startswith("v_") and formula.name in const_1:
            cond_1 = TreeNode("f_condition", [])
            if const_var == []:
                cond_2 = TreeNode("f_if", [TreeNode(f"any(contain({equation.name},item) for item in [{','.join(ignore)}]) or any(contain({equation.name},tree_form(item)) for item in {var_name})"),TreeNode("False")])
            else:
                cond_2 = TreeNode("f_if", [TreeNode(f"any(contain({equation.name},item) for item in [{','.join(ignore)}]) or any(contain({equation.name},tree_form(item)) for item in {var_name})"),TreeNode("False")])
            if formula.name.startswith("v_") and formula.name in varlist.keys():
                cond_3 = TreeNode(f"{varlist[formula.name].name} == {equation.name}")
                cond_1.children += [cond_2, cond_3.fx("else")]
                return cond_1
            elif formula.name.startswith("v_") and formula.name not in varlist.keys():
                varlist[formula.name] = equation
                cond_3 = TreeNode("True")
                cond_1.children += [cond_2, cond_3.fx("else")]
                return cond_1
        elif formula.name.startswith("v_") and formula.name in ignore_list:
            cond_1 = TreeNode("f_condition", [])
            cond_2 = None
            if const_var == []:
                cond_2 = TreeNode("f_if", [TreeNode(f"not {equation.name}.name.startswith('v_')"),TreeNode("False")])
            else:
                cond_2 = TreeNode("f_if", [TreeNode(f"not {equation.name}.name.startswith('v_')"),TreeNode("False")])
            if formula.name.startswith("v_") and formula.name in varlist.keys():
                cond_3 = TreeNode(f"{varlist[formula.name].name} == {equation.name}")
                cond_1.children += [cond_2, cond_3.fx("else")]
                return cond_1
            elif formula.name.startswith("v_") and formula.name not in varlist.keys():
                varlist[formula.name] = equation
                if equation.name not in ignore:
                    ignore.append(equation.name)
                cond_3 = TreeNode("True")
                cond_1.children += [cond_2, cond_3.fx("else")]
                return cond_1
        elif formula.name.startswith("v_"):
            if formula.name.startswith("v_") and formula.name in varlist.keys():
                return TreeNode(f"{varlist[formula.name].name} == {equation.name}")
            elif formula.name.startswith("v_") and formula.name not in varlist.keys():
                varlist[formula.name] = equation
                return TreeNode("True")
        else:
            s = 0
            for key in formula.children:
                if key.name in associativity.keys():
                    s += associativity[key.name]
                else:
                    s += 1
            children = [f"{equation.name}.children[{i}]" for i in range(s)]
            new_children = []
            for key in formula.children:
                if key.name in associativity.keys():
                    n = associativity[key.name]
                    if n == 1:
                        new_children.append(children.pop(0))
                    else:
                        new_children.append(make_treenode_string(strip_w(formula.name), children[:n]))
                        children = children[n:]
                else:
                    new_children.append(children.pop(0))
            cond_1 = TreeNode("f_condition", [])
            cond_2 = TreeNode("f_if", [TreeNode(f"{equation.name}.name != '{strip_w(formula.name)}' or len({equation.name}.children) != {s}"),\
                                       TreeNode("False")])
            lst = []
            for i in range(len(formula.children)):
                lst.append(helper(TreeNode(new_children[i],[]),formula.children[i]))
            if len(lst) == 0:
                lst = TreeNode("True")
            elif len(lst) == 1:
                lst = lst[0]
            else:
                lst = TreeNode("f_wand", lst)
            cond_3 = lst
            cond_1.children += [cond_2, cond_3.fx("else")]
            return cond_1
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
    def gen_ac(eq, formula_list, ignore_list):
        lst = []
        def merge_unique(dicts):
            merged = {}
            for d in dicts:
                for key, value in d.items():
                    if key in merged:
                        if merged[key] != value:
                            return None
                    else:
                        merged[key] = value
            return merged
        def make_eq(f):
            nonlocal formula_list, lst, ignore_list
            if f.name in ["f_addw", "f_mulw", "f_hadamardw", "f_waddw", "f_wmul"]:
                lst2 = []
                for i in range(len(f.children),len(f.children)+arity):
                    for item in groupings(list(range(i)), len(f.children),\
                                          [index for index, child in enumerate(f.children)\
                                           if child.name not in formula_list or child.name in ignore_list]):
                        dic  ={}
                        for j in range(len(item)):
                            if f.children[j].name in formula_list:
                                dic[f.children[j].name] = len(item[j])
                        lst2.append(dic)
                lst.append(lst2)
            for child in f.children:
                make_eq(child)
        make_eq(eq)
        output = []
        for item in itertools.product(*lst):
            out = merge_unique(item)
            if out is not None:
                output.append(out)
        return output
    def var_replace(eq, var):
        nonlocal forbidden_value
        out = None
        if eq == var:
            return []
        for child in eq.children:
            out = var_replace(child, var)
            if isinstance(out, list):
                if eq.name in ["f_add"]:
                    if any(
                        var.name == item[0].name
                        and item[1] == 0
                        for item in forbidden_value
                    ):
                        return out
                    return out + [0]
                elif eq.name in ["f_mul"]:
                    if any(
                        var.name == item[0].name
                        and item[1] == 1
                        for item in forbidden_value
                    ):
                        return out
                    return out + [1]
                else:
                    return out
        return out
    formula_lst = []
    ll = []
    sorted_vars = list(sorted(set(vlist(formula))))
    for item in sorted_vars:
        if forbidden_value == "forbid" or item in ignore_list:
            ll.append([-100])
            continue
        output = var_replace(formula, tree_form(item))
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
    final_output = ""
    for item, upd in formula_lst_2:
        hh = gen_ac(item, vlist(item), ignore_list)
        for associativity in hh:
            varlist = {}
            ignore = []
            helper2(copy.deepcopy(equation), item)
            out = helper(copy.deepcopy(equation), item)
            varlist.update(upd)
            d = []
            for key, item2 in varlist.items():
                for val in [str(h[1]) for h in forbidden_value if h[0].name == key]:
                    if item2.children == [] and item2.name[:2] in ["v_", "d_", "s_"]:
                        d.append(TreeNode("f_!=", [tree_form(f"'{item2.name}'"), tree_form(f"'d_{val}'")]))
                    elif item2.children == []:
                        d.append(TreeNode("f_!=", [tree_form(f"{item2.name}"), tree_form(f"'d_{val}'")]))
                    else:
                        d.append(TreeNode("f_!=", [tree_form(f"{item2}"), tree_form(f"'d_{val}'")]))
            for item2 in positive:
                local_pos = copy.deepcopy(item2)
                for key, item3 in varlist.items():
                    local_pos = replace(local_pos, tree_form(key), item3)
                s = print_code2(local_pos)
                s = f"simplify({s})==0 or (compute({s}) is not None and compute({s})>0)"
                d.append(TreeNode(s))
            for item2 in negative:
                local_neg = copy.deepcopy(item2)
                for key, item3 in varlist.items():
                    local_neg = replace(local_neg, tree_form(key), item3)
                s = print_code2(local_neg)
                s = f"simplify({s})==0 or (compute({s}) is not None and compute({s})<0)"
                d.append(TreeNode(s))
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
            local_formula_out = copy.deepcopy(formula_out)
            for key, item2 in varlist.items():
                local_formula_out = replace(
                    local_formula_out, tree_form(key), item2
                )
            s = "\tif " + print_code(out) + ":\n"
            t = "\t\treturn " + print_code2(local_formula_out) + "\n"
            final_output += s + t
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
        return f"any({print_code_h(eq.children[0])} {print_code_h(eq.children[1])})"
    if eq.name == "f_list":
        return "[" + ", ".join(print_code_h(c) for c in eq.children) + "]"
    if eq.name == "f_condition":
        return print_condition(eq)
    if not eq.children:
        return eq.name
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
    if eq.name == "f_sqrt":
        child = print_code2(eq.children[0])
        return f"{child}.fx('sqrt')"
    if eq.name == "f_not":
        child = print_code2(eq.children[0])
        return f"~{child}"
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
        return eq.name
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
def remove_all(s, items):
    if s.name in items:
        return True
    if s.name.startswith("v_"):
        return False
    if s.children == []:
        return True
    return all(remove_all(child, items) for child in s.children)
def formula_compiler(lst_formula):
    s = "def transform(eq_orig):\n"
    s += "\teq = copy.deepcopy(eq_orig)\n"
    s += lst_formula
    s += f"\treturn eq_orig"
    
    env = {
        "tree_form": tree_form,
        "TreeNode": TreeNode,
        "contain": contain,
        "str_form": str_form,
        "copy":copy,
        "simplify":simplify,
        "compute":compute,
        "remove_all":remove_all,
    }
    exec(s, env)
    return env["transform"]
def formula_list_compiler(lst):
    dic = ""
    for item in lst:
        out = structure(*item)
        for item2 in out:
            dic += item2
    return formula_compiler(dic)
