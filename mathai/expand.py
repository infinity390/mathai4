
import itertools
from .base import *
from .simplify import solve, simplify

def expand(eq):
    if eq is None:
        return None
    if eq.name == "f_mul" or eq.name == "f_pow":
        if eq.name == "f_pow":
            eq = TreeNode("f_pow", [eq]) 
        ac = []
        addchild = []
        for child in eq.children:
            tmp5 = [solve(x) for x in factor_generation(child)]
            ac += tmp5
        tmp3 = []
        for child in ac:
            tmp2 = []
            if child.name == "f_add":
                if child.children != []:
                    for child2 in child.children:
                        tmp2.append(child2)
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
        
        if isinstance(addchild, list) and len(flatten(addchild))>0:
            add= 0
            for item in itertools.product(*addchild):
                mul = 1
                for item2 in item:
                    mul = mul * item2
                    mul = simplify(mul)
                add = add + mul
                add = simplify(add)
            eq = add
        eq = simplify(eq)
        
    return TreeNode(eq.name, [expand(child) for child in eq.children])

