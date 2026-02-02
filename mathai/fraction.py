from .base import *
from .simplify import simplify
from .expand import expand

def fraction(expr):
    if expr is None:
        return None

    expr = simplify(expr)

    if expr.children == []:
        return expr

    children = [fraction(c) for c in expr.children]

    if expr.name == "f_add":
        terms = []

        for c in children:

            if c.name == "f_mul":
                num = []
                den = []
                for f in c.children:
                    if (
                        f.name == "f_pow"
                        and f.children[1].name.startswith("d_")
                        and int(f.children[1].name[2:]) < 0
                    ):
                        n = int(f.children[1].name[2:])
                        den.append(
                            f.children[0]
                            if n == -1
                            else TreeNode("f_pow", [f.children[0], tree_form(f"d_{-n}")])
                        )
                    else:
                        num.append(f)
                terms.append((num, den))

            elif (
                c.name == "f_pow"
                and c.children[1].name.startswith("d_")
                and int(c.children[1].name[2:]) < 0
            ):
                n = int(c.children[1].name[2:])
                terms.append(([], [
                    c.children[0]
                    if n == -1
                    else TreeNode("f_pow", [c.children[0], tree_form(f"d_{-n}")])
                ]))

            else:
                terms.append(([c], []))

        if not any(den for _, den in terms):
            return TreeNode("f_add", children)

        num_terms = []
        for i, (num_i, _) in enumerate(terms):
            acc = list(num_i)
            for j, (_, den_j) in enumerate(terms):
                if i != j:
                    acc += den_j
            if not acc:
                acc = [tree_form("d_1")]
            num_terms.append(
                acc[0] if len(acc) == 1 else TreeNode("f_mul", acc)
            )

        numerator = TreeNode("f_add", num_terms)

        den_all = []
        for _, den in terms:
            den_all += den

        denom = den_all[0] if len(den_all) == 1 else TreeNode("f_mul", den_all)
        denom = TreeNode("f_pow", [denom, tree_form("d_-1")])

        return simplify(
            TreeNode(
                "f_mul",
                [simplify(expand(numerator)), denom],
            )
        )

    return TreeNode(expr.name, children)

