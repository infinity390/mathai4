import copy
import more_itertools
import itertools
from .base import *
from .parser import parse
from fractions import Fraction
from .simplify import simplify
import marshal
import os
def simplify0_h(eq):
    if eq.name == "f_add":
        lst = [item for item in eq.children if item.name != "d_0"]
        return operation("f_add", lst)
    if eq.name == "f_mul":
        if tree_form("d_0") in eq.children:
            return tree_form("d_0")
        lst = [item for item in eq.children if item.name != "d_1"]
        return operation("f_mul", lst)
    return eq
def simplify0(eq):
    return dowhile(eq, lambda x: transform_dfs(x, simplify0_h))
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
    
    def make_treenode_string(name, children):
        return f"TreeNode('{name}',[{','.join(children)}])"
    def helper(equation, formula):
        nonlocal ignore_list, varlist, associativity, ignore
        if formula.name.startswith("v_") and formula.name in ignore_list:
            cond_1 = TreeNode("f_condition", [])
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
            if formula.name in ["f_add", "f_mul"]:
                target_lengths = []
                for key in formula.children:
                    n = 1
                    if key.name in associativity.keys():
                        n = associativity[key.name]
                    target_lengths.append(n)
                unique_partitions = {tuple(tuple(sorted(group)) for group in more_itertools.split_into(p, target_lengths)) for p in itertools.permutations(children)}
                result = [[list(group) for group in partition] for partition in unique_partitions]
                if not hasattr(formula, "count"):
                    formula.count = len(result)-1
                else:
                    formula.count = formula.count-1
                result = result[formula.count]
                new_children = [item[0] if len(item) == 1 else make_treenode_string(formula.name, item) for item in result]
            else:
                for key in formula.children:
                    if key.name in associativity.keys():
                        n = associativity[key.name]
                        if n == 1:
                            new_children.append(children.pop(0))
                        else:
                            new_children.append(make_treenode_string(formula.name, children[:n]))
                            children = children[n:]
                    else:
                        new_children.append(children.pop(0))
            cond_1 = TreeNode("f_condition", [])
            ls = [f"{equation.name}.name != '{formula.name}'",f"len({equation.name}.children) != {s}"]
            ls = [TreeNode(h) for h in ls]
            cond_2 = TreeNode("f_if", [operation("f_wor", ls),\
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
        def make_eq(f, arity):
            nonlocal formula_list, lst, ignore_list
            valid_ops = {"f_add", "f_mul", "f_hadamard", "f_wadd", "f_wmul"}
            if f.name in valid_ops:
                lst2 = []
                label = [child.name for child in f.children if child.name.startswith("v_")]
                lst3 = [[1] if child.name in ignore_list else list(range(1,arity+1)) for child in f.children if child.name.startswith("v_")]
                for item in itertools.product(*lst3):
                    dic = {}
                    for index, item2 in enumerate(item):
                        dic[label[index]] = item2
                    lst2.append(dic)
                lst.append(lst2)
            for child in f.children:
                make_eq(child, arity)
        make_eq(eq, arity)
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
                    ) or var in negative:
                        return out
                    return out + [1]
                else:
                    return out
        return out
    def any_attr(eq):
        if hasattr(eq, "count"):
            return True
        if eq.children == []:
            return False
        return any(any_attr(child) for child in eq.children)
    def tree_count_all_zero(eq):
        if hasattr(eq, "count") and eq.count == 0:
            return False
        if eq.children == []:
            return True
        return all(tree_count_all_zero(child) for child in eq.children)
    def del_count(eq):
        if hasattr(eq, "count"):
            del eq.count
        for child in eq.children:
            del_count(child)
    formula_lst = []
    ll = []
    sorted_vars = list(sorted(set(vlist(formula))))
    fv = {}
    for key, item in forbidden_value:
        if key not in fv.keys():
            fv[key] = []
        fv[key] += [item]
    for item in sorted_vars:
        if item in ignore_list:
            ll.append([-100])
            continue
        output = [0,1] if item in var_name else []
        output = [x for x in output if (tree_form(item) not in fv.keys() or x not in fv[tree_form(item)]) and (item not in negative or x!=1)]
        if output == []:
            output = None
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
            eq_try = simplify0(
                replace(
                    eq_try,
                    tree_form(var_key),
                    tree_form(f"d_{item2}"),
                )
            )
        formula_lst.append((eq_try, eq_var))
    final_output = ""
    for item, upd in formula_lst:
        hh = gen_ac(item, vlist(item), ignore_list)
        for associativity in hh:
            while tree_count_all_zero(item):
                ignore = []
                varlist = copy.deepcopy(upd)
                out = helper(copy.deepcopy(equation), item)
                d = []
                for key, item2 in varlist.items():
                    if key in const_1:
                        if item2.name.startswith("d_"):
                            d.append(TreeNode(f"all(not contain(TreeNode('{item2}'),item) for item in [{','.join(ignore)}])"))
                        else:
                            d.append(TreeNode(f"all(not contain({item2},item) for item in [{','.join(ignore)}])"))
                for key, item2 in varlist.items():
                    for val in [h[1] for h in forbidden_value if h[0].name == key]:
                        if item2.name[:2] in ["d_"]:
                            d.append(TreeNode(f"{item2} != {val}"))
                        elif item2.name[:2] in ["f_","v_","s_"]:
                            pass
                        else:
                            d.append(TreeNode(f"{item2.name} != {val}"))
                for item2 in positive:
                    local_pos = copy.deepcopy(item2)
                    for key, item3 in varlist.items():
                        local_pos = replace(local_pos, tree_form(key), item3)
                    s = print_code2(local_pos)
                    s = f"frac({s}) is not None and frac({s})>=0"
                    d.append(TreeNode(s))
                for item2 in negative:
                    local_neg = copy.deepcopy(item2)
                    for key, item3 in varlist.items():
                        local_neg = replace(local_neg, tree_form(key), item3)
                    s = print_code2(local_neg)
                    s = f"frac({s}) is not None and frac({s})<=0"
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
                if not any_attr(item):
                    break
            del_count(item)
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
        return f"({value} if {cond} else {rest})"
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

def formula_compiler(lst_formula, save_to_file=None):
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
        "frac":frac
    }
    if save_to_file is not None:
        code = compile(s, "<string>", "exec")
        folder = os.path.join(os.path.dirname(__file__), "formula")
        os.makedirs(folder, exist_ok=True)
        with open(os.path.join(folder, f"{save_to_file}.marshal"), "wb") as f:
            marshal.dump(code, f)
        return None
    exec(s, env)
    return env["transform"]
def formula_list_compiler(lst, save_to_file=None):
    dic = ""
    for item in lst:
        out = structure(*item)
        for item2 in out:
            dic += item2
    return formula_compiler(dic, save_to_file)
