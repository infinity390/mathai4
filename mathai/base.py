import copy
from fractions import Fraction
def use(eq):
    return TreeNode(eq.name, [use(child) for child in eq.children])
def use2(eq):
    return use(tree_form(str_form(eq).replace("f_wmul", "f_mul")))
def contains_list_or_neg(node):
    stack = [node]
    while stack:
        n = stack.pop()
        if n.name == "f_list" or n.name.startswith("v_-"):
            return True
        stack.extend(n.children)
    return False

class TreeNode:
    matmul = None
    
    def __init__(self, name, children=None):
        if children is None:
            children = []

        # copy once
        children = copy.deepcopy(children)
        self.name = name

        if name == "f_add" or (name == "f_mul" and TreeNode.matmul is None):
            keyed = [(str_form(c), c) for c in children]
            self.children = [c for _, c in sorted(keyed)]

        elif name == "f_mul" and TreeNode.matmul == False:
            sortable = []
            fixed = []
            for c in children:
                if not contains_list_or_neg(c):
                    sortable.append(c)
                else:
                    fixed.append(c)
                    
            if len(sortable) > 1:
                sortable = TreeNode("f_dmul", list(sorted(sortable, key=lambda x: str_form(x))))
                sortable.name = "f_mul"
                
            elif len(sortable) == 1:
                sortable = sortable[0]
                
            if isinstance(sortable, TreeNode):
                fixed.append(sortable)
            if len(fixed) > 1:
                self.name = "f_wmul"
            elif len(fixed) == 1:
                self.name = fixed[0].name
                fixed = fixed[0].children
                
                
            self.children = fixed
        else:
            self.children = children


    def fx(self, fxname):
        return TreeNode("f_" + fxname, [self])
    def copy_tree(self):
        return copy.deepcopy(self)
    def __repr__(self):
        return string_equation(str_form(self))

    def __eq__(self, other):
        if isinstance(other, int):
            other = tree_form("d_" + str(other))
        elif not isinstance(other, TreeNode):
            return NotImplemented
        return str_form(self) == str_form(other)

    def __add__(self, other):
        if isinstance(other, int):
            other = tree_form("d_" + str(other))
        return TreeNode("f_add", [self, other])

    def __radd__(self, other):
        return self.__add__(other)

    def __mul__(self, other):
        if isinstance(other, int):
            other = tree_form("d_" + str(other))
        return TreeNode("f_mul", [self, other])

    def __rmul__(self, other):
        return self.__mul__(other)

    def __sub__(self, other):
        if isinstance(other, int):
            other = tree_form("d_" + str(other))
        return self + (tree_form("d_-1") * other)

    def __rsub__(self, other):
        if isinstance(other, int):
            other = tree_form("d_" + str(other))
        return other + (tree_form("d_-1") * self)

    def __pow__(self, other):
        if isinstance(other, int):
            other = tree_form("d_" + str(other))
        return TreeNode("f_pow", [self, other])

    def __rpow__(self, other):
        if isinstance(other, int):
            other = tree_form("d_" + str(other))
        return TreeNode("f_pow", [other, self])

    def __truediv__(self, other):
        if isinstance(other, int):
            other = tree_form("d_" + str(other))
        return self * (other ** tree_form("d_-1"))

    def __rtruediv__(self, other):
        if isinstance(other, int):
            other = tree_form("d_" + str(other))
        return other * (self ** tree_form("d_-1"))

    def __and__(self, other):
        if isinstance(other, int):
            other = tree_form("d_" + str(other))
        return TreeNode("f_and", [self, other])

    def __rand__(self, other):
        return self.__and__(other)

    def __or__(self, other):
        if isinstance(other, int):
            other = tree_form("d_" + str(other))
        return TreeNode("f_or", [self, other])

    def __ror__(self, other):
        return self.__or__(other)

    def __neg__(self):
        return tree_form("d_-1") * self

    def __hash__(self):
        return hash(str_form(self))

def str_form(node):
    def recursive_str(node, depth=0):
        result = "{}{}".format(' ' * depth, node.name)
        for child in node.children:
            result += "\n" + recursive_str(child, depth + 1)
        return result
    if not isinstance(node, TreeNode):
        return "d_"+str(node)
    return recursive_str(node)
def replace(equation, find, r):
  if str_form(equation) == str_form(find):
    return r
  col = TreeNode(equation.name, [])
  for child in equation.children:
    col.children.append(replace(child, find, r))
  return col

def contain(equation, what):
    if equation == what:
        return True
    if equation.children == []:
        return False
    return any(contain(child, what) for child in equation.children)
def remove_duplicates_custom(lst, rcustom):
    result = []
    for item in lst:
        if not any(rcustom(item, x) for x in result):
            result.append(item)
    return result
def frac_to_tree(f):
    if isinstance(f, int):
        f = Fraction(f)
    if f.numerator == 0:
        return tree_form("d_0")
    if f.numerator == 1:
        if f.denominator == 1:
            return tree_form("d_1")
        return tree_form("d_"+str(f.denominator))**tree_form("d_-1")
    if f.denominator == 1:
        return tree_form("d_"+str(f.numerator))
    else:
        return tree_form("d_"+str(f.numerator))/tree_form("d_"+str(f.denominator))
def perfect_root(n, r):
    if r <= 0 or (n < 0 and r % 2 == 0):
        return False, None

    lo = 0
    hi = n if n > 1 else 1

    while lo <= hi:
        mid = lo + (hi - lo) // 2
        pow_val = 1

        for _ in range(r):
            pow_val *= mid
            if pow_val > n:
                break

        if pow_val == n:
            return True, mid
        elif pow_val < n:
            lo = mid + 1
        else:
            hi = mid - 1

    return False, None

def frac(eq):
    if eq.name[:2] == "d_":
        return Fraction(int(eq.name[2:]))
    if eq.name == "f_add":
        p = frac(eq.children[0])
        if p is None:
            return None
        for child in eq.children[1:]:
            tmp = frac(child)
            if isinstance(tmp, Fraction):
                p+= tmp
            else:
                return None
        return p
    if eq.name == "f_mul":
        p = frac(eq.children[0])
        if p is None:
            return None
        for child in eq.children[1:]:
            tmp = frac(child)
            if isinstance(tmp, Fraction):
                p*= tmp
            else:
                return None
        return p
    if eq.name == "f_pow":
        a = frac(eq.children[0])
        b = frac(eq.children[1])
        if a is None or b is None:
            return None
        if a == 0 and b <= 0:
            return None
        if b.denominator == 1:
            return a ** b.numerator
        found_c, c = perfect_root(a.numerator, b.denominator)
        found_d, d = perfect_root(a.denominator, b.denominator)
        if found_c and found_d:
            return Fraction(c,d) ** b.numerator
        return None
    return None
def factor_generation(eq):
    output = []
    if eq.name != "f_mul":
        tmp = TreeNode("f_mul", [])
        tmp.children.append(eq)
        eq = tmp
    if eq.name == "f_mul":
        for child in eq.children:
            if child.name == "f_pow":
                if child.children[1].name[:2] != "d_":
                    output.append(child)
                    continue
                if frac(child.children[1]) is not None and frac(child.children[1]).denominator == 1:
                    n = int(child.children[1].name[2:])
                    if n < 0:
                        for i in range(-n):
                            out = factor_generation(child.children[0])
                            out = [x**-1 for x in out]
                            output += out
                            #output.append(child.children[0]**-1)
                    else:
                        for i in range(n):
                            output.append(child.children[0])
                else:
                    output.append(child)
            else:
                output.append(child)
                
    return output
import math

def compute(eq):
    # Base case: leaf node
    if eq.children == []:
        if eq.name == "s_e":
            return math.e
        elif eq.name == "s_pi":
            return math.pi
        elif eq.name.startswith("d_"):
            return float(eq.name[2:])
        else:
            return None

    # Recursive case: compute child values
    values = [compute(child) for child in eq.children]
    if None in values:
        return None
    # Evaluate based on node type
    if eq.name == "f_add":
        return sum(values)
    elif eq.name == "f_abs":
        return math.fabs(values[0])
    elif eq.name == "f_sub":
        return values[0] - values[1]
    elif eq.name == "f_rad":
        return values[0] * math.pi / 180
    elif eq.name in ["f_wmul", "f_mul"]:
        result = 1.0
        for v in values:
            result *= v
        return result
    elif eq.name == "f_neg":
        return -values[0]
    elif eq.name == "f_div":
        return values[0] / values[1]
    elif eq.name == "f_pow":
        return values[0] ** values[1]
    elif eq.name == "f_sin":
        return math.sin(values[0])
    elif eq.name == "f_cos":
        return math.cos(values[0])
    elif eq.name == "f_tan":
        return math.tan(values[0])
    elif eq.name == "f_arcsin":
        return math.asin(values[0])
    elif eq.name == "f_arccos":
        return math.acos(values[0])
    elif eq.name == "f_arctan":
        return math.atan(values[0])
    elif eq.name == "f_log":
        return math.log(values[0])
    else:
        return None

def num_dem(equation):
    num = tree_form("d_1")
    den = tree_form("d_1")
    for item in factor_generation(equation):
        if item.name == "f_pow":
            c = frac(item.children[1])
            if c is not None and c < 0:
                t = frac_to_tree(-c)
                if t == tree_form("d_1"):
                    den = den * item.children[0]
                else:
                    den = den * item.children[0]**t
            else:
                den = den * item
        else:
            num = num*item
    return num, den

def summation(lst):
    if len(lst) == 0:
        return tree_form("d_0")
    s = lst[0]
    for item in lst[1:]:
        s += item
    return s
def vlist(eq):
    out = []
    if eq.name[:2] == "v_":
        out.append(eq.name)
    for child in eq.children:
        out += vlist(child)
    return sorted(list(set(out)), key=lambda x: int(x[2:]))
def product(lst):
    if lst == []:
        return tree_form("d_1")
    s = lst[0]
    for item in lst[1:]:
        s *= item
    return s
def flatten_tree(node):
    if node is None:
        return None
    if not node.children:
        return node
    ad = []
    if node.name in ["f_add", "f_mul", "f_and", "f_or", "f_wmul"]:
        merged_children = []
        for child in node.children:
            flattened_child = flatten_tree(child)
            if flattened_child.name == node.name:
                merged_children.extend(flattened_child.children)
            else:
                merged_children.append(flattened_child)
        return TreeNode(node.name, merged_children)
    else:
        node.children = [flatten_tree(child) for child in node.children]
        return node
def dowhile(eq, fx):
    if eq is None:
        return None
    while True:
        orig = copy.deepcopy(eq)
        eq2 = fx(eq)
        if eq2 is None:
            return None
        eq = copy.deepcopy(eq2)
        if eq == orig:
            return orig
def tree_form(tabbed_strings):
    lines = tabbed_strings.split("\n")
    root = TreeNode("Root")
    current_level_nodes = {0: root}
    stack = [root]
    for line in lines:
        level = line.count(' ')
        node_name = line.strip()
        node = TreeNode(node_name)
        while len(stack) > level + 1:
            stack.pop()
        parent_node = stack[-1]
        parent_node.children.append(node)
        current_level_nodes[level] = node
        stack.append(node)
    return root.children[0]
def string_equation_helper(equation_tree):
    if equation_tree.children == []:
        if equation_tree.name[:2]=="g_":
            return '"'+equation_tree.name[2:]+'"'
        return equation_tree.name
    extra = ""
    if equation_tree.name == "f_neg":
        return "-"+string_equation_helper(equation_tree.children[0])
    if equation_tree.name == "f_not":
        return "~"+string_equation_helper(equation_tree.children[0])
    if equation_tree.name == "f_list":
        return "["+",".join([string_equation_helper(child) for child in equation_tree.children])+"]"
    if equation_tree.name == "f_index":
        return string_equation_helper(equation_tree.children[0])+"["+",".join([string_equation_helper(child) for child in equation_tree.children[1:]])+"]"
    s = "(" 
    if len(equation_tree.children) == 1 or equation_tree.name[2:] in [chr(ord("A")+i) for i in range(26)]+["limitpinf", "subs", "try", "ref","limit", "integrate", "exist", "forall", "sum2", "int", "pdif", "dif", "A", "B", "C", "covariance", "sum"]:
        s = equation_tree.name[2:] + s
    sign = {"f_not":"~", "f_wadd":"+", "f_wmul":"*", "f_intersection":"&", "f_union":"|", "f_sum2":",", "f_exist":",", "f_forall":",", "f_sum":",","f_covariance": ",", "f_B":",", "f_imply":"->", "f_ge":">=", "f_le":"<=", "f_gt":">", "f_lt":"<", "f_cosec":"?" , "f_equiv": "<->", "f_sec":"?", "f_cot": "?", "f_dot": ".", "f_circumcenter":"?", "f_transpose":"?", "f_exp":"?", "f_abs":"?", "f_log":"?", "f_and":"&", "f_or":"|", "f_sub":"-", "f_neg":"?", "f_inv":"?", "f_add": "+", "f_mul": "*", "f_pow": "^", "f_poly": ",", "f_div": "/", "f_sub": "-", "f_dif": ",", "f_sin": "?", "f_cos": "?", "f_tan": "?", "f_eq": "=", "f_sqrt": "?"}
    arr = []
    k = None
    if equation_tree.name not in sign.keys():
        k = ","
    else:
        k = sign[equation_tree.name]
    for child in equation_tree.children:
        arr.append(string_equation_helper(copy.deepcopy(child)))
    outfinal = s + k.join(arr) + ")"+extra
    
    return outfinal.replace("+-", "-")
def string_equation(eq):
    alpha = ["x", "y", "z"]+[chr(x+ord("a")) for x in range(0,23)]
    beta = [chr(x+ord("A")) for x in range(0,26)]
    eq = tree_form(eq)
    
    for i, letter in enumerate(alpha):
        eq = replace(eq, tree_form("v_"+str(i)), tree_form(letter))
    for i, letter in enumerate(beta):
        eq = replace(eq, tree_form("v_-"+str(i+1)), tree_form(letter))
    for i in range(100, 150):
        eq = replace(eq, tree_form("v_"+str(i)), tree_form("c"+str(i-100)))
    eq = str_form(eq)
    
    eq = eq.replace("d_", "")
    eq = eq.replace("s_", "")
    eq = eq.replace("v_", "")
    eq = eq.replace("'", "")
    outfinal = string_equation_helper(tree_form(eq))
    if outfinal[0] == "(" and outfinal[-1] == ")":
        return outfinal[1:-1]
    return outfinal
