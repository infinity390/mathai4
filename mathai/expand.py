import itertools
from .base import *
from .simplify import simplify
'''
def _expand(equation):
    eq = equation
    eq.children = [_expand(flatten_tree(child)) for child in eq.children]
    if eq.name == "f_pow":
        n = frac(eq.children[1])
        if n is not None and n.denominator == 1 and n.numerator > 1:
            power_children = []
            for i in range(n.numerator):
                power_children.append(eq.children[0])
            return _expand(flatten_tree(TreeNode("f_mul", power_children)))
    if eq.name == "f_mul":
        lone_children = tree_form("d_1")
        bracket_children = []
        for i in range(len(eq.children)-1,-1,-1):
            if eq.children[i].name == "f_add":
                bracket_children.append(eq.children[i])
            elif eq.children[i].name == "f_pow" and eq.children[i].children[0].name == "f_add":
                n = frac(eq.children[i].children[1])
                if n is not None and n.denominator == 1 and n.numerator > 1:
                    for j in range(n.numerator):
                        bracket_children.append(eq.children[i].children[0])
                else:
                    lone_children = lone_children * eq.children[i]
            else:
                lone_children = lone_children * eq.children[i]
        lone_children = simplify(lone_children)
        while bracket_children != []:
            tmp = tree_form("d_0")
            for i in range(len(bracket_children[0].children)):
                if lone_children.name == "f_add":
                    for j in range(len(lone_children.children)):
                        tmp = tmp + bracket_children[0].children[i] * lone_children.children[j]
                else:
                    tmp = tmp + lone_children * bracket_children[0].children[i]
            lone_children = flatten_tree(simplify(tmp))
            bracket_children.pop(0)
        return lone_children
    return eq
'''
def _expand(equation):
    """Iterative version of _expand without recursion."""
    # Stack: (node, child_index, partially_processed_children)
    stack = [(equation, 0, [])]

    while stack:
        node, child_index, processed_children = stack.pop()

        # If all children are processed
        if child_index >= len(node.children):
            # Replace children with processed versions
            node.children = processed_children

            # === Handle f_pow ===
            if node.name == "f_pow":
                n = frac(node.children[1])
                if n is not None and n.denominator == 1 and n.numerator > 1:
                    # Convert power to repeated multiplication
                    power_children = [node.children[0] for _ in range(n.numerator)]
                    new_node = TreeNode("f_mul", power_children)
                    # Flatten tree
                    node = flatten_tree(new_node)
                    # Push it back for further processing
                    stack.append((node, 0, []))
                    continue

            # === Handle f_mul ===
            elif node.name == "f_mul":
                # Separate lone children and bracket children
                lone_children = tree_form("d_1")
                bracket_children = []

                # Iterate in reverse (like original)
                for child in reversed(node.children):
                    if child.name == "f_add":
                        bracket_children.append(child)
                    elif child.name == "f_pow" and child.children[0].name == "f_add":
                        n = frac(child.children[1])
                        if n is not None and n.denominator == 1 and n.numerator > 1:
                            for _ in range(n.numerator):
                                bracket_children.append(child.children[0])
                        else:
                            lone_children = lone_children * child
                    else:
                        lone_children = lone_children * child

                lone_children = simplify(lone_children)

                # Distribute bracket children over lone children iteratively
                while bracket_children:
                    tmp = tree_form("d_0")
                    bracket = bracket_children.pop(0)
                    for bc in bracket.children:
                        if lone_children.name == "f_add":
                            for lc in lone_children.children:
                                tmp = tmp + bc * lc
                        else:
                            tmp = tmp + bc * lone_children
                    # Simplify after each distribution
                    lone_children = flatten_tree(simplify(tmp))

                node = lone_children

            # === Return node to parent ===
            if stack:
                parent, idx, parent_children = stack.pop()
                parent_children.append(node)
                stack.append((parent, idx + 1, parent_children))
            else:
                # Root node fully expanded
                return node

        else:
            # Push current node back for next child
            stack.append((node, child_index, processed_children))
            # Push the child to process next
            child = flatten_tree(node.children[child_index])
            stack.append((child, 0, []))

def expand(eq):
    return _expand(eq)
