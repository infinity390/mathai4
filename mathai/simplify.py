import math
from .base import *
from fractions import Fraction
from collections import Counter
def convert_to_basic(node):
    if not node.name.startswith("f_"):
        return node
    node.children = [convert_to_basic(c) for c in node.children]
    if node.name == "f_sub":
        node = node.children[0]-node.children[1]
    if node.name == "f_div":
        node = node.children[0]/node.children[1]
    if node.name == "f_sqrt":
        node = node.children[0]**(tree_form("d_2")**tree_form("d_-1"))
    return node

def clear_div(eq, denom):

    lst = factor_generation(eq)

    if tree_form("d_0") in lst:
        return tree_form("d_0"), True

    lst3 = []
    for item in lst:
        if "v_" not in str_form(item) and compute(item) < 0:
            lst3.append(item)
    sign = denom
    if len(lst3) % 2 == 1:
        sign = False
    if denom:
        eq2 = []
        eq3 = []
        for item in lst:
            if frac(item) is not None:
                eq2.append(item)
            else:
                eq3.append(item)

        if eq3 == []:
            return product(eq2), True
        return product(eq3), sign
    lst4 = []

    for item in lst:
        if item.name == "f_pow":
            tmp = frac(item.children[1])
            if tmp is None or tmp != -1:
                lst4.append(item)
        else:
            lst4.append(item)

    lst2 = []
    for item in lst4:
        if frac(item) is None:
            lst2.append(item)

    if lst2 == []:
        return product(lst4), sign
    return product(lst2), sign

def multiply_node(equation):
    
    if equation is None:
        return None
    stack = [(equation, 0, [])]

    while stack:
        node, child_index, processed_children = stack.pop()

        if child_index >= len(node.children):
            node.children = processed_children

            if node.name == "f_mul":

                con = 1
                new_children = []
                for child in reversed(node.children):
                    val = frac(child)
                    if val is not None:
                        con *= val
                    else:
                        new_children.append(child)

                if con == 0:
                    node = tree_form("d_0")

                    if stack:
                        parent, idx, parent_children = stack.pop()
                        parent_children.append(node)
                        stack.append((parent, idx + 1, parent_children))
                        continue
                    else:
                        return node

                node.children = new_children

                base_powers = []
                for child in node.children:
                    if child.name == "f_pow":
                        base, power = child.children
                    else:
                        base = child
                        power = tree_form("d_1")

                    found = False
                    for i, (b, p) in enumerate(base_powers):
                        if b == base:
                            base_powers[i] = (b, p + power)
                            found = True
                            break
                    if not found:
                        base_powers.append((base, power))

                new_mul = TreeNode("f_mul", [])
                for base, power in base_powers:
                    if power == tree_form("d_1"):
                        new_mul.children.append(base)
                    elif power == tree_form("d_0"):
                        continue
                    else:
                        new_mul.children.append(TreeNode("f_pow", [base, power]))

                con_tree = frac_to_tree(con)
                if con_tree != tree_form("d_1"):
                    new_mul.children.append(con_tree)

                if not new_mul.children:
                    node = tree_form("d_1")
                elif len(new_mul.children) == 1:
                    node = new_mul.children[0]
                else:
                    node = new_mul

            if stack:
                parent, idx, parent_children = stack.pop()
                parent_children.append(node)
                stack.append((parent, idx + 1, parent_children))
            else:
                return node  

        else:

            stack.append((node, child_index, processed_children))

            child = node.children[child_index]
            stack.append((child, 0, []))

def addition_node(equation):

    if equation is None:
        return None
    stack = [(equation, 0, [])]

    while stack:
        node, child_index, processed_children = stack.pop()

        if child_index >= len(node.children):
            node.children = processed_children

            if node.name == "f_add":

                con = 0
                new_children = []
                for child in reversed(node.children):
                    val = frac(child)
                    if val is not None:
                        con += val
                    else:
                        new_children.append(child)

                node.children = new_children

                base_terms = []
                for child in node.children:

                    power_node = TreeNode("f_mul", [])
                    base_node = None
                    mul_node = TreeNode("f_mul", [])
                    multiplier_node = None

                    if child.name == "f_mul":
                        for c in child.children:
                            if frac(c) is not None: 
                                if c != tree_form("d_0"):
                                    power_node.children.append(c)
                            else:  
                                if c != tree_form("d_1"):
                                    mul_node.children.append(c)
                        if len(mul_node.children) == 0:
                            base_node = tree_form("d_1")
                        elif len(mul_node.children) == 1:
                            base_node = mul_node.children[0]
                        else:
                            base_node = mul_node
                    else:
                        base_node = child

                    if not power_node.children:
                        multiplier_node = tree_form("d_1")
                    elif len(power_node.children) == 1:
                        multiplier_node = power_node.children[0]
                    else:
                        multiplier_node = power_node

                    found = False
                    for i, (b, m) in enumerate(base_terms):
                        if b == base_node:
                            base_terms[i] = (b, m + multiplier_node)
                            found = True
                            break
                    if not found:
                        base_terms.append((base_node, multiplier_node))

                new_add = TreeNode("f_add", [])
                for base, multiplier in base_terms:
                    if multiplier == tree_form("d_1"):
                        new_add.children.append(base)
                    elif multiplier == tree_form("d_0"):
                        continue
                    else:
                        new_add.children.append(base * multiplier)

                con_tree = frac_to_tree(con)
                if con_tree != tree_form("d_0"):
                    new_add.children.append(con_tree)

                if not new_add.children:
                    node = tree_form("d_0")
                elif len(new_add.children) == 1:
                    node = new_add.children[0]
                else:
                    node = new_add

            if stack:
                parent, idx, parent_children = stack.pop()
                parent_children.append(node)
                stack.append((parent, idx + 1, parent_children))
            else:

                return node

        else:

            stack.append((node, child_index, processed_children))

            child = node.children[child_index]
            stack.append((child, 0, []))

def other_node(eq):
    if eq is None:
        return None
    if eq.name == "f_log":
        if len(eq.children) == 1:
            if eq.children[0].name == "d_1":
                return tree_form("d_0")
            if eq.children[0].name == "s_e":
                return tree_form("d_1")
    if eq.name == "f_mul":
        if tree_form("d_1") in eq.children:
            return product([remove_extra(child) for child in eq.children if child != tree_form("d_1")])
    if eq.name == "f_pow" and len(eq.children) == 2:
        a, b = frac(eq.children[0]), frac(eq.children[1])
        if a is not None and b is not None and a == 0 and b < 0:
            return None
        if eq.children[1].name == "d_0":
            return tree_form("d_1")
        if eq.children[1].name == "d_1":
            return eq.children[0]
        if eq.children[0].name == "d_1":
            return tree_form("d_1")
        if eq.children[0].name == "f_abs" and eq.children[1].name.startswith("d_")\
           and int(eq.children[1].name[2:]) % 2 == 0:
            return eq.children[0].children[0] ** eq.children[1]

        if eq.children[0].name == "f_mul":
            n = frac(eq.children[1])
            if n is not None and n < 0 and n.numerator % 2 == 1 and n.denominator == 1:
                n2 = frac_to_tree(-n)
                if n2 == tree_form("d_1"):
                    return product([child**-1 for child in eq.children[0].children])
                return product([child**-1 for child in eq.children[0].children]) ** n2
        if frac(eq.children[1]) == Fraction(1,2):
            d = frac(eq.children[0])
            if d is not None and d < 0:
                return tree_form("s_i")*(frac_to_tree(-d)**eq.children[1])
        if eq.children[0].name == "f_pow":
            b = eq.children[0].children[1]
            c = eq.children[1]
            out = frac(b*c)
            if out is not None:
                out2 = frac(b)
                if out.numerator % 2 == 0 or (out2 is not None and out2.numerator % 2 != 0):
                    return eq.children[0].children[0] ** (b*c)
                else:
                    return eq.children[0].children[0].fx("abs") ** (b*c)
            else:
                tmp = compute(eq.children[0].children[0])
                if (tmp is not None and tmp > 0) or eq.children[0].children[0].name == "f_abs":
                    return eq.children[0].children[0] ** (b*c)
    c = frac(eq)
    if c is not None:
        c = frac_to_tree(c)
        if c != eq:
            return c
    if eq.name == "f_pow" and eq.children[0] == tree_form("s_i") and eq.children[1].name.startswith("d_"):
        n = int(eq.children[1].name[2:])
        if n % 4 == 0:
            return tree_form("d_1")
        if n % 4 == 1:
            return tree_form("s_i")
        if n % 4 == 2:
            return tree_form("d_-1")
        if n % 4 == 3:
            return -tree_form("s_i")
    if eq.name == "f_pow" and eq.children[0].name == "s_e":
        if eq.children[1].name == "f_log":
            return eq.children[1].children[0]
        if eq.children[1].name == "f_mul":
            lst = factor_generation(eq.children[1])
            log = None
            for i in range(len(lst)-1,-1,-1):
                if lst[i].name == "f_log":
                    log = lst[i]
                    lst.pop(i)
                    break
            if log is not None:
                return log.children[0] ** product(lst)
    for index, child in enumerate(eq.children):
        out = other_node(child)
        if out is None:
            return None
        eq.children[index] = out
    return TreeNode(eq.name, eq.children)
def cancel(eq):
    n, d = num_dem(eq)
    d = simplify(d)
    if d != tree_form("d_1"):
        n = simplify(n)
        a = Counter(factor_generation(n))
        b = Counter(factor_generation(d))
        c = a & b
        a = simplify(product(list(a-c)))
        b = simplify(product(list(b-c)))
        if b == tree_form("d_1"):
            return a
        if a == tree_form("d_1"):
            return b ** -1
        return a/b
    return TreeNode(eq.name, [cancel(child) for child in eq.children])
def solve3(eq):
    a = lambda x: multiply_node(x)
    b = lambda x: addition_node(x)
    c = lambda x: other_node(x)
    return dowhile(eq, lambda x: flatten_tree(c(b(a(x)))))

def simplify(eq, basic=True):
    if eq is None:
        return None
    if eq.name == "f_and" or eq.name == "f_not" or eq.name == "f_or":
        new_children = []
        for child in eq.children:
            new_children.append(simplify(child))
        return TreeNode(eq.name, new_children)
    if eq.name[2:] in "gt ge lt le eq".split(" "):
        denom = eq.name != "f_eq"
        tmp2 = simplify(eq.children[0] - eq.children[1])
        tmp, denom = clear_div(tmp2, denom)
        tmp = simplify(tmp)

        value2 = eq.name[2:]
        if denom is False:
            value2 = {"ge":"le", "le":"ge", "gt":"lt", "lt":"gt", "eq":"eq"}[value2]
        value2 = "f_"+value2
        out = TreeNode(value2, [tmp, tree_form("d_0")])
        return out
    eq = flatten_tree(eq)
    if basic:
        eq = convert_to_basic(eq)
    eq = solve3(eq)
    return eq

