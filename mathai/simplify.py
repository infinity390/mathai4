import math
from .base import *
from fractions import Fraction
nolog = False
def set_nolog(val):
    global nolog
    nolog = val
def _solve(eq):
    def solve_add(eq):
        def multiplied(eq):
            if eq.name[:2] == "d_":
                return int(eq.name[2:]), "const"
            if eq.name == "f_mul":
                arth = 1
                for i in range(len(eq.children)-1,-1,-1):
                    if eq.children[i].name[:2] == "d_":
                        arth *= int(eq.children[i].name[2:])
                        eq.children.pop(i)
                if eq.children == []:
                    eq = "const"
                elif len(eq.children) == 1:
                    eq = eq.children[0]
                return arth, eq
            return 1, eq
        if eq.name != "f_add":
            return eq
        dic = {"const":0}
        for child in eq.children:
            a, b = multiplied(child)
            if b == "const":
                dic[b] += a
            elif b in dic.keys():
                dic[b] += a
            else:
                dic[b] = a
        summation = TreeNode("f_add", [])
        for key in sorted(dic.keys(), key=lambda x: str_form(x)):
            n = dic[key]
            if n == 0:
                continue
            if key == "const":
                summation.children.append(tree_form("d_"+str(n)))
            else:
                if n == 1:
                    summation.children.append(key)
                else:
                    if key.name == "f_mul":
                        key.children.append(tree_form("d_"+str(n)))
                    else:
                        key = tree_form("d_"+str(n))*key
                    summation.children.append(key)
        
        if len(summation.children)==1:
            summation = summation.children[0]
            
        if summation.name[:2] == "f_" and len(summation.children)==0:
            summation = tree_form("d_0")
        return summation

    def solve_mul(eq):
        def multiplied(eq):
            if eq.name[:2] == "d_":
                return int(eq.name[2:]), "const"
            if eq.name != "f_pow" or eq.children[1].name[:2] != "d_":
                return 1, eq
            else:
                return int(eq.children[1].name[2:]), eq.children[0]
        if eq.name == "f_pow":
            if eq.children[1] == 1:
                return eq.children[0]
            return eq
        if eq.name != "f_mul":
            return eq
        
        dic = {"const":1}
        for child in eq.children:
            
            a, b = multiplied(child)
            if b == "const":
                dic[b] *= a
            elif b in dic.keys():
                dic[b] += a
            else:
                dic[b] = a
                
        summation = TreeNode("f_mul", [])
        if dic["const"] == 0:
            return tree_form("d_0")
        for key in sorted(dic.keys(), key=lambda x: str_form(x)):
            n = dic[key]
            if n == 0:
                
                continue
            if key == "const":
                if n != 1:
                    summation.children.append(tree_form("d_"+str(n)))
            else:
                if n== 1:
                    summation.children.append(key)
                else:
                    summation.children.append(key**tree_form("d_"+str(n)))

        if len(summation.children)==1:
            summation = summation.children[0]
        
        if summation.name[:2] == "f_" and len(summation.children)==0:
            summation = tree_form("d_1")

        return summation
    def solve_u(eq):
        if eq.name in ["f_pow", "f_mul"]:
            return solve_mul(eq)
        return solve_add(eq)
    def recur_solve(eq):
        eq = solve_u(eq)
        if eq.children == []:
            pass

        elif eq.name in ("f_add", "f_mul"):
            merged_children = []
            for child in eq.children:
                if child.name == eq.name:
                    merged_children.extend(child.children)
                else:
                    merged_children.append(child)

            eq = TreeNode(eq.name, merged_children)
        return TreeNode(eq.name, [recur_solve(child) for child in eq.children])

    return recur_solve(eq)
def _convert_sub2neg(eq):
    if eq.name == "f_neg":
        return -_convert_sub2neg(eq.children[0])
    elif eq.name == "f_sub":
        return _convert_sub2neg(eq.children[0]) - _convert_sub2neg(eq.children[1])
    elif eq.name == "f_sqrt":
        return _convert_sub2neg(eq.children[0])**(tree_form("d_2")**-1)
    elif eq.name == "f_div":
        if eq.children[0] == 0:
            return tree_form("d_0")
        return _convert_sub2neg(eq.children[0])*_convert_sub2neg(eq.children[1])**-1
    return TreeNode(eq.name, [_convert_sub2neg(child) for child in eq.children])
def solve(eq, specialfx=False):
    if specialfx:
        eq = _convert_sub2neg(eq)
    
    eq = flatten_tree(eq)
    
    return dowhile(eq, _solve)
def solve2(eq):
    return solve(eq, True)    
def clear_div(eq, denom=False):
    lst = factor_generation(eq)
    if tree_form("d_0") in lst:
        return tree_form("d_0"), True
    lst3 = [item for item in lst if "v_" not in str_form(item) and compute(item) < 0]
    
    sign = True
    if len(lst3) % 2 == 1:
        sign = False
    if denom:
        eq2 = [item for item in lst if "v_" not in str_form(item)]
        eq3 = [item for item in lst if "v_" in str_form(item)]
        if eq3 == []:
            return solve(product(eq2)),True
        return solve(product(eq3)),sign
        #return eq if sign else -eq, sign
    lst = [item for item in lst if not(item.name == "f_pow" and frac(item.children[1]) is not None and frac(item.children[1]) == -1)]

    lst2 = [item for item in lst if "v_" in str_form(item)]
    if lst2 == []:
        return solve(product(lst)),sign
    return solve(product(lst2)),sign

def simplify(eq):
    
    if "v_" not in str_form(eq):
        n = frac(eq)
        if n is not None:
            if n.numerator == 0:
                return tree_form("d_0")
            if n.denominator != 1:
                return tree_form("d_"+str(n.numerator))/tree_form("d_"+str(n.denominator))
            else:
                return tree_form("d_"+str(n.numerator))
    
    if contain(eq, tree_form("s_inf")):
        return eq
    error = False
    eq = flatten_tree(eq)
    if eq.name in ["f_and", "f_or", "f_not"]:
        return TreeNode(eq.name, [simplify(child) for child in eq.children])
        
    if eq.name in ["f_lt", "f_gt", "f_le", "f_ge", "f_eq"]:
        tmp, sign = clear_div(simplify(eq.children[0]-eq.children[1]), eq.name != "f_eq")
        name2 = eq.name
        if not sign:
            name2 = {"f_lt":"f_gt", "f_gt":"f_lt", "f_eq":"f_eq", "f_le":"f_ge", "f_ge":"f_le"}[name2]

        return TreeNode(name2, [tmp, tree_form("d_0")])

    eq = solve(eq, True)
    def helper(eq):
        
        if eq.name == "f_pow" and  eq.children[0].name[:2] == "d_" and eq.children[1].name[:2] == "d_":
            
            a, b = int(eq.children[0].name[2:]), int(eq.children[1].name[2:])
            a = a**abs(b)
            if b == 0 and a == 0:
                error= True
                return eq
            if b == 0:
                b = 1
            b = int(b/abs(b))
            if b == 1:
                eq = tree_form("d_"+str(a))
            else:
                eq = tree_form("d_"+str(a))**-1
        return TreeNode(eq.name, [helper(child) for child in eq.children])
    def helper2(eq):
        
        def even(eq):
            return eq.name[:2] == "d_" and int(eq.name[2:])%2==0
        def even2(eq):
            return any(even(item) for item in factor_generation(eq))
        if eq.name == "f_pow" and eq.children[0].name == "f_pow":
            if even2(eq.children[0].children[1]) or even2(eq.children[1]) and not even2(solve(eq.children[0].children[1] * eq.children[1])):
                return eq.children[0].children[0].fx("abs") ** solve(eq.children[0].children[1] * eq.children[1])
            else:
                return eq.children[0].children[0] ** solve(eq.children[0].children[1] * eq.children[1])
        return TreeNode(eq.name, [helper2(child) for child in eq.children])
    def helper3(eq):
        
        if eq.name == "f_mul":
            n = Fraction(1)
            for i in range(len(eq.children)-1,-1,-1):
                child= eq.children[i]
                if child.name == "f_pow" and child.children[0].name[:2] == "d_" and child.children[1] == -1:
                    if int(child.children[0].name[2:]) == 0:
                        error = True
                        return eq
                    n = n*Fraction(1,int(child.children[0].name[2:]))
                    eq.children.pop(i)
                elif child.name[:2] == "d_":
                    n = n*int(child.name[2:])
                    eq.children.pop(i)
            if n.denominator == 1:
                eq.children.append(tree_form("d_"+str(n.numerator)))
            else:
                eq.children.append(tree_form("d_"+str(n.numerator))*tree_form("d_"+str(n.denominator))**-1)
            
            if len(eq.children) == 1:
                eq = eq.children[0]
        return TreeNode(eq.name, [helper3(child) for child in eq.children])
    def helper4(eq):
        nonlocal error
        if eq == tree_form("d_-1")**tree_form("d_-1"):
            return tree_form("d_-1")
        def perfect_nth_root_value(x, n):
            """Return integer y if x is a perfect n-th power (y**n == x), else None."""
            if x < 0 and n % 2 == 0:
                return None  # even root of negative number not real
            
            sign = -1 if x < 0 else 1
            x = abs(x)

            # approximate integer root
            y = round(x ** (1.0 / n))
            
            if y ** n == x:
                return sign * y
            return None
        def pp(eq, n):
            if n == 1:
                return eq
            return eq**tree_form("d_"+str(n))
        if eq.name == "f_pow" and eq.children[0].name[:2] == "d_" and frac(eq.children[1]) is not None:
            f = frac(eq.children[1])
            r = f.denominator
            f = frac(eq.children[1]).numerator
            if r > 1:
                n = int(eq.children[0].name[2:])
                if n < 0 and r==2:
                    out = perfect_nth_root_value(-n, 2)
                    if out is not None:
                        return pp( tree_form("d_"+str(out))*tree_form("s_i") , f)
                    else:
                        return pp( (tree_form("d_"+str(-n))**(tree_form("d_2")**-1))*tree_form("s_i"), f)
                else:
                    out = perfect_nth_root_value(n, r)
                    if out is not None:
                        return pp( tree_form("d_"+str(out)), f)
        if not nolog:
            if eq.name == "f_mul" and len(eq.children)== 2:
                for i in range(2):
                    if eq.children[i].name[:2] == "d_" and eq.children[1-i].name == "f_log":
                        return (eq.children[1-i].children[0]**eq.children[i]).fx("log")
            if eq.name == "f_pow" and eq.children[0] == tree_form("s_e") and eq.children[1].name == "f_log":
                return eq.children[1].children[0]
            
        if eq.name == "f_pow" and eq.children[0] == tree_form("d_1"):
            eq = tree_form("d_1")
        if eq.name == "f_pow" and eq.children[0] == tree_form("d_0"):
            if frac(eq.children[1]) is not None and frac(eq.children[1]) <= 0:
                error = True
            else:
                eq = tree_form("d_0")
                
        if eq.name =="f_pow" and eq.children[0] == tree_form("s_i") and frac(eq.children[1])is not None and frac(eq.children[1]).denominator == 1:
            n = frac(eq.children[1]).numerator
            eq = {0:tree_form("d_1"), 1:tree_form("s_i"), 2:tree_form("d_-1"), 3:-tree_form("s_i")}[n%4]
        if eq.name == "f_mul":
            dic = {}
            for child in eq.children:
                head = child
                tail = None
                if child.name == "f_pow":
                    head = child.children[0]
                    tail = child.children[1]
                if tail is None:
                    tail = tree_form("d_1")
                if head not in dic.keys():
                    dic[head] = tail
                else:
                    dic[head] += tail
            if len(eq.children) > len(dic.keys()):
                eq = product([key if dic[key] == 1 else key**dic[key] for key in dic.keys()])
        if eq.name == "f_abs" and eq.children[0].name == "f_pow" and frac(eq.children[0].children[1]) == Fraction(1,2):
            eq = eq.children[0]
        if eq.name == "f_pow" and eq.children[0].name == "f_pow" and eq.children[0].children[1] == tree_form("d_2")**-1 and eq.children[1] == tree_form("d_2"):
            eq = eq.children[0].children[0]
        if (eq.name == "f_sin" and eq.children[0].name == "f_arcsin") or (eq.name == "f_cos" and eq.children[0].name == "f_arccos") or (eq.name == "f_tan" and eq.children[0].name == "f_arctan"):
            eq = eq.children[0].children[0]
        if (eq.name == "f_cos" and eq.children[0].name == "f_arcsin") or (eq.name == "f_sin" and eq.children[0].name == "f_arccos"):
            eq2 = eq.children[0].children[0]
            eq2 = (tree_form("d_1") - eq2*eq2)**(tree_form("d_1")/tree_form("d_2"))
            eq = eq2
        if (eq.name == "f_arcsin" and eq.children[0].name == "f_sin") or (eq.name == "f_arccos" and eq.children[0].name == "f_cos") or (eq.name == "f_arctan" and eq.children[0].name == "f_tan"):
            eq = eq.children[0].children[0]
        if eq.name == "f_abs" and eq.children[0].name[:2] == "d_":
            eq = tree_form("d_"+str(abs(int(eq.children[0].name[2:]))))
        if eq.name == "f_abs" and "v_" not in str_form(eq.children[0]):
            if compute(eq.children[0]) > 0.00001:
                eq = eq.children[0]
            elif compute(eq.children[0]) < 0.00001:
                eq = -eq.children[0]
        if eq.name == "f_pow" and  eq.children[0].name[:2] == "d_" and frac(eq.children[1]) is not None:
            f = frac(eq.children[1])
            if f.denominator != 1:
                return helper(eq.children[0]**tree_form("d_"+str(f.numerator)))**(tree_form("d_"+str(f.denominator))**-1)
        
        return TreeNode(eq.name, [helper4(child) for child in eq.children])
    
    def helper7(eq):
        
        if eq.name == "f_pow" and eq.children[0].name == "f_mul":
            return product([child**eq.children[1] for child in eq.children[0].children])
        return TreeNode(eq.name, [helper7(child) for child in eq.children])
    
    def helper6(eq):
        if eq.name == "f_mul":
            lst = factor_generation(eq)
            rm = []
            for i in range(len(lst)):
                if i in rm:
                    continue
                for j in range(len(lst)):
                    if i ==j or j in rm:
                        continue
                    if solve(helper2(lst[i]**-1)) == lst[j]:
                        rm += [i,j]
                        break
            if rm != []:
                for item in sorted(rm)[::-1]:
                    lst.pop(item)
                return product(lst)
        return TreeNode(eq.name, [helper6(child) for child in eq.children])
    def helper5(eq):
        
        if eq.name == "f_pow" and eq.children[1].name[:2] == "d_" and abs(int(eq.children[1].name[2:]))%2==0 and eq.children[0].name == "f_abs":
            return eq.children[0].children[0]**eq.children[1]
        return TreeNode(eq.name, [helper5(child) for child in eq.children])
    def helper8(eq):
        if eq.name == "f_pow" and eq.children[0].name == "f_abs" and frac(eq.children[1]) is not None and frac(eq.children[1]).numerator % 2==0:
            return eq.children[0].children[0] ** eq.children[1]
        if eq.name == "f_abs" and eq.children[0].name == "f_abs":
            return eq.children[0]
        if eq.name == "f_cos" and eq.children[0].name == "f_abs":
            return eq.children[0].children[0].fx("cos")
        return TreeNode(eq.name, [helper8(child) for child in eq.children])
    def fx1(eq):
        for item in [helper, helper3, helper4, helper6,solve,helper2,helper5]:
            eq = dowhile(eq, item)
        return eq
    def fx2(eq):
        for item in [helper, helper3, helper4, helper6,helper7,helper2,helper5]:
            eq = dowhile(eq, item)
        return eq
    def fx3(eq):
        for item in [fx1, fx2]:
            eq = dowhile(eq, item)
        return eq
    eq = dowhile(eq, fx3)
    eq = dowhile(eq, helper8)
    if error:
        return None
    return solve(eq)

