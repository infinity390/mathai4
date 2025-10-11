import itertools
from .base import *
from .simplify import solve, simplify

def expand(eq):
    if eq is None:
        return None

    stack = [(eq, 0)]  # (node, stage)
    result_map = {}     # id(node) -> expanded TreeNode

    while stack:
        node, stage = stack.pop()
        node_id = id(node)

        # Leaf node
        if not node.children and stage == 0:
            result_map[node_id] = TreeNode(node.name, [])
            continue

        if stage == 0:
            # Stage 0: push node back for stage 1 after children
            stack.append((node, 1))
            # Push children to stack
            for child in reversed(node.children):
                if id(child) not in result_map:
                    stack.append((child, 0))
        else:
            # Stage 1: all children processed
            children_expanded = [result_map[id(child)] for child in node.children]

            # Only f_mul or f_pow need special expansion
            if node.name in ["f_mul", "f_pow"]:
                current_eq = TreeNode(node.name, children_expanded)

                if node.name == "f_pow":
                    current_eq = TreeNode("f_pow", [current_eq])

                ac = []
                addchild = []

                for child in current_eq.children:
                    tmp5 = [solve(x) for x in factor_generation(child)]
                    ac += tmp5

                tmp3 = []
                for child in ac:
                    tmp2 = []
                    if child.name == "f_add":
                        if child.children != []:
                            tmp2.extend(child.children)
                        else:
                            tmp2 = [child]
                    else:
                        tmp3.append(child)
                    if tmp2 != []:
                        addchild.append(tmp2)

                tmp4 = 1
                for item in tmp3:
                    tmp4 = tmp4 * item
                addchild.append([tmp4])

                def flatten(lst):
                    flat_list = []
                    for item in lst:
                        if isinstance(item, list) and item == []:
                            continue
                        if isinstance(item, list):
                            flat_list.extend(flatten(item))
                        else:
                            flat_list.append(item)
                    return flat_list

                if len(flatten(addchild)) > 0:
                    add = 0
                    for prod_items in itertools.product(*addchild):
                        mul = 1
                        for item2 in prod_items:
                            mul = mul * item2
                            mul = simplify(mul)
                        add = add + mul
                        add = simplify(add)
                    current_eq = simplify(add)
                else:
                    current_eq = simplify(current_eq)

                # Store expanded result
                result_map[node_id] = current_eq
            else:
                # Default: reconstruct node with children
                result_map[node_id] = TreeNode(node.name, children_expanded)

    # Return final expanded eq
    return result_map[id(eq)]

