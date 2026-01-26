from .base import *
from .simplify import simplify
import itertools

def eliminate_powers(node):
    if not node.children:
        return node

    node.children = [eliminate_powers(c) for c in node.children]

    if node.name == "f_pow":
        base, exp = node.children
        n = frac(exp)

        # Only expand positive integer powers
        if not (n and n.denominator == 1 and n.numerator > 1):
            return node

        n = n.numerator

        # ---- Multinomial expansion ----
        if base.name == "f_add":
            terms = []
            for combo in itertools.product(base.children, repeat=n):
                prod = combo[0]
                for c in combo[1:]:
                    prod = prod * c
                terms.append(prod)
            return simplify(TreeNode("f_add", terms))

        # ---- Fallback: simple power ----
        return TreeNode("f_mul", [base] * n)

    return node



# =====================================================
# Phase 2: Single distributive rewrite (DEEPEST FIRST)
# =====================================================

def expand_once(node):
    """
    Performs exactly ONE distributive expansion.
    Deepest-first (post-order).
    """

    # ---- recurse FIRST (this is the fix) ----
    for i, c in enumerate(node.children):
        new, changed = expand_once(c)
        if changed:
            node.children[i] = new
            return node, True

    # ---- now try expanding at this node ----
    if node.name == "f_mul":
        for i, child in enumerate(node.children):
            if child.name == "f_add":
                left = node.children[:i]
                right = node.children[i+1:]

                terms = []
                for t in child.children:
                    prod = t
                    for r in right:
                        prod = prod * r
                    for l in reversed(left):
                        prod = l * prod
                    terms.append(prod)

                return TreeNode("f_add", terms), True

    return node, False

def _expand2(equation):
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

# =====================================================
# Phase 3: Global fixed-point driver
# =====================================================

def expand(eq):
    orig = TreeNode.matmul
    if TreeNode.matmul is None:
        return _expand2(eq)
    eq = simplify(eq)
    if TreeNode.matmul is not None:
        TreeNode.matmul = True
        eq = tree_form(str_form(eq).replace("f_wmul", "f_mul"))
        eq = flatten_tree(eq)
    eq = eliminate_powers(eq)
    while True:        
        eq = flatten_tree(eq)        
        eq, changed = expand_once(eq)
        if not changed:
            break
    eq =simplify(eq)
    TreeNode.matmul = orig
    return eq
