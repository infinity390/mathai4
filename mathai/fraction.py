from .base import *
from .simplify import simplify
from .expand import expand

def fraction(eq):
    stack = [(eq, None)]  # (current_node, parent_processed_children)
    result_map = {}  # Map original nodes to their processed TreeNode

    while stack:
        node, parent_info = stack.pop()

        # If node already processed, continue
        if node in result_map:
            continue

        # Base case: leaf node
        if not node.children:
            result_map[node] = TreeNode(node.name, [])
            continue

        # Check if all children are processed
        all_children_done = all(child in result_map for child in node.children)
        if not all_children_done:
            # Push current node back to stack after children
            stack.append((node, parent_info))
            for child in reversed(node.children):
                if child not in result_map:
                    stack.append((child, (node, node.children)))
            continue

        # Now all children are processed, handle this node
        if node.name == "f_eq":
            left = result_map[node.children[0]]
            right = result_map[node.children[1]]
            result_map[node] = TreeNode("f_eq", [left, right])
            continue

        elif node.name == "f_add":
            con = []
            for child in node.children:
                child_processed = result_map[child]
                if child_processed.name == "f_pow" and child_processed.children[1].name[:2] == "d_" and int(child_processed.children[1].name[2:]) < 0:
                    den = []
                    n = int(child_processed.children[1].name[2:])
                    if n == -1:
                        den.append(child_processed.children[0])
                    else:
                        den.append(TreeNode("f_pow", [child_processed.children[0], tree_form("d_" + str(-n))]))
                    con.append([[], den])
                elif child_processed.name == "f_mul":
                    num = []
                    den = []
                    for child2 in child_processed.children:
                        if child2.name == "f_pow" and child2.children[1].name[:2] == "d_" and int(child2.children[1].name[2:]) < 0:
                            n = int(child2.children[1].name[2:])
                            if n == -1:
                                den.append(child2.children[0])
                            else:
                                den.append(TreeNode("f_pow", [child2.children[0], tree_form("d_" + str(-n))]))
                        else:
                            num.append(child2)
                    con.append([num, den])
                else:
                    con.append([[child_processed], []])

            if len(con) > 1 and any(x[1] != [] for x in con):
                # Construct numerator
                a_children = []
                for i in range(len(con)):
                    b_children = con[i][0].copy()
                    for j in range(len(con)):
                        if i == j:
                            continue
                        b_children += con[j][1]
                    if len(b_children) == 0:
                        b_children = [tree_form("d_1")]
                    elif len(b_children) == 1:
                        b_children = b_children
                    else:
                        b_children = [TreeNode("f_mul", b_children)]
                    a_children += b_children if isinstance(b_children, list) else [b_children]

                a = TreeNode("f_add", a_children)

                # Construct denominator
                c_children = []
                for i in range(len(con)):
                    c_children += con[i][1]
                if len(c_children) == 1:
                    c = c_children[0]
                else:
                    c = TreeNode("f_mul", c_children)
                c = TreeNode("f_pow", [c, tree_form("d_-1")])

                result_map[node] = TreeNode("f_mul", [simplify(expand(simplify(a))), c])
                continue

        # Default: just reconstruct node
        children_processed = [result_map[child] for child in node.children]
        result_map[node] = TreeNode(node.name, children_processed)

    # Final return
    return simplify(result_map[eq])
