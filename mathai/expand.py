from .base import *
from .simplify import simplify
import itertools

def expand_nc(expr, label="f_mul"):
    """
    Expand expression where:
      - f_add is commutative
      - label (@) is NON-commutative
    """
    # --- base cases ---
    if expr.name not in {"f_add", label, "f_pow"}:
        return expr

    # --- expand children first ---
    expr.children = [expand_nc(c, label) for c in expr.children]

    # ==========================================================
    # POWER: (A + B)^n  only if n is positive integer
    # ==========================================================
    if expr.name == "f_pow":
        base, exp = expr.children
        n = frac(exp)
        if n and n.denominator == 1 and n.numerator > 1:
            factors = [base] * n.numerator
            return expand_nc(TreeNode(label, factors), label)
        return expr

    # ==========================================================
    # ADDITION (commutative)
    # ==========================================================
    if expr.name == "f_add":
        out = []
        for c in expr.children:
            if c.name == "f_add":
                out.extend(c.children)
            else:
                out.append(c)
        return TreeNode("f_add", out)

    # ==========================================================
    # NON-COMMUTATIVE MULTIPLICATION (@)
    # ==========================================================
    if expr.name == label:
        factors = []

        # flatten only (NO reordering)
        for c in expr.children:
            if c.name == label:
                factors.extend(c.children)
            else:
                factors.append(c)

        # find first additive factor
        for i, f in enumerate(factors):
            if f.name == "f_add":
                left  = factors[:i]
                right = factors[i+1:]

                terms = []
                for term in f.children:
                    new_factors = left + [term] + right
                    terms.append(
                        expand_nc(TreeNode(label, new_factors), label)
                    )

                return TreeNode("f_add", terms)

        # no addition inside → return as-is
        return TreeNode(label, factors)


def expand2(eq, over="*"):
    over = {"@": "f_wmul", ".":"f_dot", "*":"f_mul"}[over]
    return expand_nc(eq, over)
def expand(eq, over="*"):
    eq = expand2(eq, over)
    return TreeNode(eq.name, [expand(child, over) for child in eq.children])
