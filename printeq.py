from base import *
from simplify import solve
import copy

def abstractexpr(eq):
    
    if eq.name in ["f_mul", "f_pow"]:
        
        lst = factor_generation(eq)
        deno = [item.children[0]**int(item.children[1].name[3:]) for item in lst if item.name == "f_pow" and item.children[1].name[:3] == "d_-"]
        if eq.name == "f_mul" and any(item.name[:2] == "d_" and int(item.name[2:]) < 0 for item in lst):
            return solve(-eq).fx("neg")
        if deno != []:
            
            num = [item for item in lst if item.name != "f_pow" or item.children[1].name[:3] != "d_-"]
            if num == []:
                num = [tree_form("d_1")]
            return TreeNode("f_div", [solve(product(num)), solve(product(deno))])
    
    
    return TreeNode(eq.name, [abstractexpr(child) for child in eq.children])

def printeq_str(eq):
    return str(dowhile(copy.deepcopy(eq), abstractexpr))

def printeq(eq):
    print(printeq_str(eq))

def printeq_log(lst):
    for item in lst:
        print("  "*item[0] + item[1])
