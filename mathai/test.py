from .simplify import simplify
from .expand import expand
from .fraction import fraction
from .base import *
from .logic import logic0
import random
import copy
def generate_random_tree(max_depth=5, max_branching_factor=4, branch_prob=0.7):
    UNARY_POOL = ["f_neg"]
    BINARY_POOL = ["f_div", "f_pow"]
    MANY_POOL = ["f_add", "f_mul"]
    LEAF_POOL = ["v_0", "v_1", "d_0", "d_1", "d_2", "d_-1", "d_-2"]
    unary_branches = []
    binary_branches = []
    many_branches = []
    leaf_nodes = []
    root = TreeNode(name="")
    queue = [(root, 0)]
    while queue:
        current_node, current_depth = queue.pop(0)
        should_branch = (
            current_depth < max_depth and 
            (current_depth == 0 or random.random() < branch_prob)
        )
        if should_branch:
            num_children = random.randint(1, max_branching_factor)
            if num_children == 1:
                current_node.name = random.choice(UNARY_POOL)
                unary_branches.append(current_node)
            elif num_children == 2:
                current_node.name = random.choice(BINARY_POOL)
                binary_branches.append(current_node)
            else:
                current_node.name = random.choice(MANY_POOL)
                many_branches.append(current_node)
            for _ in range(num_children):
                child = TreeNode(name="")
                current_node.children.append(child)
                queue.append((child, current_depth + 1))
        else:
            current_node.name = random.choice(LEAF_POOL)
            leaf_nodes.append(current_node)
    return root
def group_expressions(expressions, key_func):
    if not expressions:
        return [], []
    groups = []
    index_groups = []
    pair_cache = {}
    work_stack = list(reversed(list(enumerate(expressions))))
    while work_stack:
        idx, current_expr = work_stack.pop()
        matched = False
        for g_idx, group in enumerate(groups):
            rep = group[0]
            rep_idx = index_groups[g_idx][0]
            cache_key = (idx, rep_idx) if idx <= rep_idx else (rep_idx, idx)
            if cache_key not in pair_cache:
                pair_cache[cache_key] = key_func(current_expr, rep)
            if pair_cache[cache_key]:
                group.append(current_expr)
                index_groups[g_idx].append(idx)
                matched = True
                break
        if not matched:
            groups.append([current_expr])
            index_groups.append([idx])
    return groups, index_groups
def is_equivalent(x, y):
    if x is None or y is None:
        return x is None and y is None
    if x == y:
        return True
    diff = x - y
    res = dowhilelist(diff, [logic0, simplify, expand, fraction])
    return res == 0 or getattr(res, 'name', None) in (0, "0", "d_0")
def test(number_of_problem=3):
    lst = [
        generate_random_tree(max_depth=2, max_branching_factor=3)
        for _ in range(number_of_problem)
    ]
    print("problems generated")
    for i, item in enumerate(lst):
        print(f"{i+1}. {item}")
    print()
    processed_exprs = []
    for item in copy.deepcopy(lst):
        simp = simplify(item)
        if simp is None:
            processed_exprs.append(None)
        else:
            processed_exprs.append(dowhilelist(item, [simplify, expand, fraction]))
    group, h = group_expressions(processed_exprs, is_equivalent)
    print("answer")
    for a, b in zip(h, group):
        print("(" + ",".join([str(item + 1) for item in a]) + ")")
        for item in b:
            print(item)
        print()
