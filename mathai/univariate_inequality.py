import itertools
from .base import *
from .inverse import inverse
from collections import Counter
from .simplify import simplify
from .expand import expand
from .fraction import fraction
import copy
from .diff import diff
from .logic import logic0
from .tool import poly, poly_simplify
def intersection2(domain, lst):
    domain = copy.deepcopy(domain)
    if domain == [True]:
        return lst
    elif domain == [True]:
        return []
    lst = [item for item in lst if item not in domain]
    out = []
    for item2 in lst:
        for index in range(len(domain)):
            if isinstance(domain[index], bool) and domain[index]:
                if index == 0 and compute(item2) < compute(domain[index+1]):
                    out.append(item2)
                    break
                elif index == len(domain)-1 and compute(domain[index-1]) < compute(item2):
                    out.append(item2)
                    break
                elif index != 0 and index != len(domain)-1 and compute(domain[index-1]) < compute(item2) and compute(item2) < compute(domain[index+1]):
                    out.append(item2)
                    break
    return list(set(out))
def flip_less_than(inter):
    inter = copy.deepcopy(inter)
    return [not item if isinstance(item, bool) else item for item in inter]
def intersection(domain_1, domain_2):
    domain_1, domain_2 = copy.deepcopy(domain_1), copy.deepcopy(domain_2)
    if domain_1 == [True]:
        return domain_2
    if domain_2 == [True]:
        return domain_1
    if domain_1 == [False] or domain_2 == [False]:
        return [False]
    def simplify_ranges(ranges):
        simplified_ranges = []
        i = 0
        while i < len(ranges):
            if i + 2 < len(ranges) and ranges[i] is True and ranges[i + 2] is True:
                simplified_ranges.append(True)
                i += 3
            elif i + 2 < len(ranges) and ranges[i] is False and ranges[i + 2] is False:
                simplified_ranges.append(False)
                i += 3
            else:
                simplified_ranges.append(ranges[i])
                i += 1
        return simplified_ranges
    result = domain_1 + domain_2
    result = [item for item in result if not isinstance(item, bool)]
    result = list(set(result))
    result = sorted(result, key=lambda x: compute(x))
    i = len(result)
    while i>=0:
        result.insert(i, True)
        i = i - 1
    result[0] = domain_1[0] and domain_2[0]
    result[-1] = domain_1[-1] and domain_2[-1]
    def find_fraction_in_list(fraction_list, target_fraction):
        for i in range(1, len(fraction_list)-1, 2):
            if fraction_list[i] == target_fraction:
                return i
        return -1
    for i in range(2, len(result)-1, 2):
        if result[i+1] in domain_1:
            result[i] = result[i] and domain_1[find_fraction_in_list(domain_1, result[i+1])-1]
        if result[i+1] in domain_2:
            result[i] = result[i] and domain_2[find_fraction_in_list(domain_2, result[i+1])-1]
        if result[i-1] in domain_1:
            result[i] = result[i] and domain_1[find_fraction_in_list(domain_1, result[i-1])+1]
        if result[i-1] in domain_2:
            result[i] = result[i] and domain_2[find_fraction_in_list(domain_2, result[i-1])+1]
    result = simplify_ranges(result)
    return result
class Range:
    def __init__(self, r=[True], p=[], z=[]):
        self.r = r
        self.p = p
        self.z = z
        self.do = True
    def unfix(self):
        self.do = False
        return self
    def fix(self):
        if not self.do:
            return
        def simplify_ranges(ranges):
            simplified_ranges = []
            i = 0
            while i < len(ranges):
                if i + 2 < len(ranges) and ranges[i] is True and ranges[i + 2] is True:
                    simplified_ranges.append(True)
                    i += 3
                elif i + 2 < len(ranges) and ranges[i] is False and ranges[i + 2] is False:
                    simplified_ranges.append(False)
                    i += 3
                else:
                    simplified_ranges.append(ranges[i])
                    i += 1
            return simplified_ranges
        self.r = simplify_ranges(self.r)
        common = set(self.p) & set(self.z)
        self.z = list(set(self.z) - common)
        self.p = list(set(self.p) - common)
        self.p = list(set(self.p) - set(intersection2(self.r, self.p)))
        self.z = list(set(intersection2(self.r, self.z)))
        return self
    def __or__(self, other):
        return (self.unfix().__invert__().unfix() & other.unfix().__invert__().unfix()).unfix().__invert__().fix()
    def __invert__(self):
        tmp =  Range(flip_less_than(self.r), self.z, list(set(self.p)-set(self.z)))
        return tmp
    def __and__(self, other):
        a = intersection(self.r, other.r)
        b = intersection2(self.r, other.p)
        c = intersection2(other.r, self.p)
        tmp = Range(a, list(set(b)|set(c)|(set(self.p)&set(other.p))), list(set(self.z)|set(other.z)))
        return tmp
    def __str__(self):
        if self.r == [False] and self.p == [] and self.z == []:
            return "{}"
        out = []
        out2 = ""
        if self.r != [False]:
            for i in range(0, len(self.r), 2):
                string = ""
                if self.r[i]:
                    if i == 0:
                        string += "(-inf,"
                        if len(self.r)==1:
                            string += "+inf)"
                        else:
                            string += str(self.r[i+1])+")"
                    elif i == len(self.r)-1 and len(self.r)!=1:
                        string += "("+str(self.r[i-1])+",+inf)"
                    else:
                        string += "("+str(self.r[i-1])+","+str(self.r[i+1])+")"
                    out.append(string)
        if self.p != []:
            out.append("{"+",".join([str(item) for item in self.p])+"}")
        if self.z != []:
            out2 = "{"+",".join([str(item) for item in self.z])+"}"
        if out2 == "":
            return "U".join(out)
        else:
            return "U".join(out)+"-"+out2
def prepare(eq):
    if eq.name[2:] in "and not or".split(" "):
        output = TreeNode(eq.name, [])
        for child in eq.children:
            out = prepare(child)
            if out is None:
                return None
            output.children.append(out)
        output = TreeNode(output.name, output.children)
        return output
    elif eq.name[2:] in "gt lt eq ge le".split(" "):
        eq = simplify(eq)
        out = prepare(eq.children[0])
        if out is None:
            return None
        output = TreeNode(eq.name, [out, tree_form("d_0")])
        output = logic0(output)
        return output
    else:
        eq = logic0(eq)
        if eq.name in ["s_true", "s_false"]:
            return eq
        if len(vlist(eq)) != 1:
            if "v_" not in str_form(eq):
                return eq
            return None
        out = poly(eq, vlist(eq)[0])
        if out is None or len(out) > 3:
            output = []
            for item in factor_generation(eq):
                if item.name == "f_pow" and item.children[1].name == "d_-1":
                    out2 = poly(item.children[0], vlist(eq)[0])
                    if out2 is not None and len(out2) <= 3:
                        output.append(poly_simplify(item.children[0])**-1)
                    else:
                        return None
                else:
                    out2 = poly(item, vlist(eq)[0])
                    if out2 is not None and len(out2) <= 3:
                        output.append(poly_simplify(item))
                    else:
                        return None
            return simplify(product(output))
        else:
            return poly_simplify(eq)
dic_table = {}
def helper(eq, var="v_0"):
    global dic_table
    eq2 = copy.deepcopy(eq)
    if eq2 in dic_table.keys():
        return dic_table[eq2]
    if eq.children[0].name == "f_add":
        eq.children[0] = simplify(expand(eq.children[0]))
        #eq = simplify(factor2(eq))
    equ = False
    sign= True
    if eq.name in ["f_gt", "f_ge"]:
        sign = True
    elif eq.name in ["f_lt", "f_le"]:
        sign = False
    if eq.name in ["f_ge", "f_le"]:
        equ = True
    if eq.name == "f_eq":
        equ= True
    critical = []
    equal = []
    more = []
    _, d = num_dem(eq.children[0])
    d = simplify(d)
    #d = factor2(d)
    for item in factor_generation(d):
        item = simplify(expand(item))
        if len(vlist(item)) != 0:
            v = vlist(item)[0]
            if diff(diff(item, v), v) != tree_form("d_0"):
                continue
            out = inverse(item, vlist(item)[0])
            more.append(simplify(out))
    #eq.children[0] = factor2(eq.children[0])
    for item in factor_generation(eq.children[0]):
        item = simplify(expand(item))
        if len(vlist(item)) == 0:
            if compute(item) <0:
                sign = not sign
            continue
        v = vlist(item)[0]
        if item.name == "f_pow" and item.children[1].name== "d_-1":
            item = item.children[0]
            if diff(diff(item, v), v) != tree_form("d_0"):
                a = replace(diff(diff(item, v), v), tree_form(v), tree_form("d_0"))/tree_form("d_2")
                if "v_" in str_form(a):
                    return None
                if compute(a) < 0:
                    sign = not sign
                continue
            else:
                tmp2 = diff(copy.deepcopy(item))
                if compute(tmp2)<0:
                    sign = not sign
                    item = simplify(item * tree_form("d_-1"))
                out = inverse(item, vlist(item)[0])
                critical.append(out)
        else:
            if diff(diff(item, v), v) != tree_form("d_0"):
                a = replace(diff(diff(item, v), v), tree_form(v), tree_form("d_0"))/tree_form("d_2")
                if "v_" in str_form(a):
                    return None
                if compute(a) < 0:
                    sign = not sign
                continue
            else:
                tmp2 = diff(copy.deepcopy(item))
                if compute(tmp2)<0:
                    sign = not sign
                    item = simplify(item * tree_form("d_-1"))
                out = inverse(item, vlist(item)[0])
                critical.append(out)
                if equ:
                    equal.append(out)
    equal = list(set([simplify(item) for item in equal]))
    more = list(set([simplify(item) for item in more]))
    critical = [simplify(item) for item in critical]
    critical = Counter(critical)
    critical = sorted(critical.items(), key=lambda x: compute(x[0]))
    i = len(critical)
    element = sign
    while i>=0:
        critical.insert(i, element)
        if i>0 and critical[i-1][1] % 2 != 0:
            element = not element
        i = i - 1
    for i in range(1, len(critical), 2):
        critical[i] = critical[i][0]
    if eq.name == "f_eq":
        final = Range([False], equal, more)
        dic_table[eq2] = final
        return final
    final = Range(critical, equal, more)
    dic_table[eq2] = final
    return final
def wavycurvy(eq):
    if eq.name == "s_true":
        return Range([True])
    if eq.name == "s_false":
        return Range([False])
    if eq.name not in ["f_and", "f_or", "f_not"]:
        out = helper(eq)
        if out is None:
            return None
        return out
    lst= [wavycurvy(child) for child in eq.children]
    if None in lst:
        return None
    ra = lst[0]
    if eq.name == "f_and":
        for child in lst[1:]:
            ra = ra & child
    elif eq.name == "f_or":
        for child in lst[1:]:
            ra = ra | child
    elif eq.name == "f_not":
        ra = ~ra
    return ra
def absolute(equation):
    def mul_abs(eq):
        if eq.name == "f_abs" and eq.children[0].name == "f_mul":
            return simplify(product([item.fx("abs") for item in factor_generation(eq.children[0])]))
        return TreeNode(eq.name, [mul_abs(child) for child in eq.children])
    equation = mul_abs(equation)
    def collectabs(eq):
        out = []
        if eq.name == "f_abs":
            out.append(eq)
            return out
        for child in eq.children:
            out += collectabs(child)
            if out != []:
                return out
        return out
    def abc(eq, arr):
        def trans(eq):
            nonlocal arr
            out = {}
            if eq.name == "f_abs":
                x = arr.pop(0)
                if x == 0 or x==2:
                    return eq.children[0]
                else:
                    return -eq.children[0]
            else:  
                return TreeNode(eq.name, [trans(child) for child in eq.children])
        return trans(eq)
    out = list(set(collectabs(equation)))
    if out == []:
        return logic0(equation)
    else:
        a = TreeNode("f_ge", [out[0].children[0], tree_form("d_0")]) & replace(equation, out[0], out[0].children[0])
        b = TreeNode("f_lt", [out[0].children[0], tree_form("d_0")]) & replace(equation, out[0], -out[0].children[0])
        return a | b
def handle_sqrt(eq):
    d= []
    def helper2(eq):
        nonlocal d
        if eq.name in ["f_lt", "f_gt", "f_le", "f_ge","f_eq"]:
            out = []
            def helper(eq):
                nonlocal out
                if eq.name == "f_pow" and frac(eq.children[1]) == Fraction(1,2):
                    out.append(simplify(eq))
                x = [helper(child) for child in eq.children]
            helper(eq)
            for item in out:
                n = tree_form("d_1")
                if eq.name == "f_eq":
                    eq2 = inverse(simplify(eq.children[0]), str_form(item))
                else:
                    eq2, sgn = inverse(simplify(eq.children[0]), str_form(item), True)
                    if sgn == False:
                        n = tree_form("d_-1")
                d.append(TreeNode("f_ge", [eq2,tree_form("d_0")]))
                eq3 = simplify(expand(simplify(eq2**2)))
                return simplify(TreeNode(eq.name, [simplify(n*item.children[0]-eq3*n), tree_form("d_0")]))
        return TreeNode(eq.name, [helper2(child) for child in eq.children])
    out = helper2(eq)
    if len(d) == 0:
        return out
    return TreeNode("f_and", [helper2(eq)]+d)
def domain(eq):
    eq = simplify(eq)
    out = []
    def helper2(eq):
        nonlocal out
        if eq.name == "f_pow" and frac(eq.children[1]) is not None and frac(eq.children[1]).denominator == 2:
            if "v_" in str_form(eq.children[0]):
                out.append(TreeNode("f_ge", [eq.children[0], tree_form("d_0")]))
                out.append(TreeNode("f_ge", [eq, tree_form("d_0")]))
        if eq.name == "f_pow" and frac(eq.children[1]) is not None and frac(eq.children[1]) <0:
            tmp = TreeNode("f_eq", [eq.children[0], tree_form("d_0")]).fx("not")
            if "v_" in str_form(tmp):
                out.append(tmp)
        x = [helper2(child) for child in eq.children]
    helper2(eq)
    out = list(set([simplify(item) for item in out]))
    if out == []:
        return eq
    if len(out)==1:
        out = out[0]
    else:
        out = TreeNode("f_and", list(out))
    if eq.name in ["f_lt", "f_gt", "f_le", "f_ge", "f_eq"]:
        return eq & out
    return out
