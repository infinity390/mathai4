import copy
import itertools
from .base import *
from .parser import parse
from fractions import Fraction
from .simplify import simplify
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
    equation,
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
    
    def strip_w(s):
        if s in ["f_addw", "f_mulw", "f_hadamardw", "f_waddw"]:
            return s[:-1]
        return s
    def helper(equation, formula):
        nonlocal const_1, var_name, ignore_list, const_var, varlist, associativity, ignore
        if formula.name.startswith("v_") and formula.name in const_1:
            if any(contain(equation,item) for item in ignore) or any(contain(equation,tree_form(item)) for item in var_name):
                return False
            else:                
                if formula.name.startswith("v_") and formula.name in varlist.keys():
                    return varlist[formula.name] == equation
                elif formula.name.startswith("v_") and formula.name not in varlist.keys():
                    varlist[formula.name] = equation
                    return True
        elif formula.name.startswith("v_") and formula.name in ignore_list:
            if not equation.name.startswith("v_"):
                return False
            else:
                if formula.name.startswith("v_") and formula.name in varlist.keys():
                    return varlist[formula.name] == equation
                elif formula.name.startswith("v_") and formula.name not in varlist.keys():
                    varlist[formula.name] = equation
                    if equation not in ignore:
                        ignore.append(equation)
                    return True
        elif formula.name.startswith("v_"):
            if formula.name.startswith("v_") and formula.name in varlist.keys():
                return varlist[formula.name] == equation
            elif formula.name.startswith("v_") and formula.name not in varlist.keys():
                varlist[formula.name] = equation
                return True
        else:
            s = 0
            for key in formula.children:
                if key.name in associativity.keys():
                    s += associativity[key.name]
                else:
                    s += 1
            if equation.name != strip_w(formula.name) or len(equation.children) != s:
                return False
            else:
                children = copy.deepcopy(equation.children)
                new_children = []
                for key in formula.children:
                    if key.name in associativity.keys():
                        n = associativity[key.name]
                        if n == 1:
                            new_children.append(children.pop(0))
                        else:
                            new_children.append(TreeNode(strip_w(formula.name), children[:n]))
                            children = children[n:]
                    else:
                        new_children.append(children.pop(0))
                lst = []
                for i in range(len(formula.children)):
                    lst.append(helper(new_children[i],formula.children[i]))
                if len(lst) == 0:
                    lst = True
                elif len(lst) == 1:
                    lst = lst[0]
                else:
                    lst = all(lst)
                return lst
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
            eq_try = simplify0(
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
            out2 = helper(copy.deepcopy(equation), copy.deepcopy(item))
            varlist = {}
            out = helper(copy.deepcopy(equation), item)
            varlist.update(upd)
            d = []
            for key, item2 in varlist.items():
                for val in [str(h[1]) for h in forbidden_value if h[0].name == key]:
                    d.append(item2 != val)
            for item2 in positive:
                local_pos = copy.deepcopy(item2)
                for key, item3 in varlist.items():
                    local_pos = replace(local_pos, tree_form(key), item3)
                d.append(simplify(local_pos)==0 or (compute(local_pos) is not None and compute(local_pos)>0))
            for item2 in negative:
                local_neg = copy.deepcopy(item2)
                for key, item3 in varlist.items():
                    local_neg = replace(local_neg, tree_form(key), item3)
                d.append(simplify(local_pos)==0 or (compute(local_neg) is not None and compute(local_neg)<0))
            if len(d) == 0:
                pass
            elif len(d) == 1:
                if out:
                    out = d[0]
                else:
                    out = False
            else:
                if out:
                    out = all(d)
                else:
                    out = False
            local_formula_out = copy.deepcopy(formula_out)
            for key, item2 in varlist.items():
                local_formula_out = replace(
                    local_formula_out, tree_form(key), item2
                )
            if out and out2:
                return local_formula_out
    return None
def convert_lst(eq):
    if eq.name == "f_list":
        return [int(child.name[2:]) if child.name.startswith("d_") else convert_lst(child) for child in eq.children]
    return TreeNode(eq.name, [convert_lst(child) for child in eq.children])
def convert_string(s):
    if s == "_":
        return None
    return convert_lst(parse(s))
def formula_interpret_helper(s):
    lst = []
    for item in s.split("\n"):
        item = item.split(" ")
        lst.append([simplify(parse(item[0])), simplify(parse(item[1]))] + [convert_string(item2) for item2 in item[2:-1]] + [int(item[-1])])
    return lst
def formula_interpret(equation, s):
    lst = formula_interpret_helper(s)
    return dowhile(equation, lambda x: transform_dfs(x, lambda y: helper(y, lst)))
def helper(equation, lst):
    for item in lst:
        out = structure(copy.deepcopy(equation), *item)
        if out is not None:
            return out
    return equation
