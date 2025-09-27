import itertools
from .base import *

def logic0(eq):
     if eq.children is None or len(eq.children)==0:
         return eq
     if eq.name in ["f_eq", "f_lt", "f_gt" "f_ge"] and eq.children[1].name[:2]=="d_" and eq.children[0].name[:2]=="d_":
        a, b = int(eq.children[0].name[2:]), int(eq.children[1].name[2:])
        if eq.name == "f_eq":
            return tree_form("s_true") if a==b else tree_form("s_false")
        if eq.name == "f_ge":
            return tree_form("s_true") if a>=b else tree_form("s_false")
        if eq.name == "f_lt":
            return tree_form("s_true") if a < b else tree_form("s_false")
     if eq.name == "f_ge":
        return TreeNode("f_gt", eq.children) | TreeNode("f_eq", eq.children)

     if eq.name == "f_gt":
        return TreeNode("f_lt", eq.children).fx("not") & TreeNode("f_eq", eq.children).fx("not")

     if eq.name == "f_le":
        return TreeNode("f_lt", eq.children) | TreeNode("f_eq", eq.children)
     return TreeNode(eq.name, [logic0(child) for child in eq.children])
def logic3(eq):
    if eq.name == "f_forall" and eq.children[1] in [tree_form("s_true"), tree_form("s_false")]:
        return eq.children[1]
    if eq.name == "f_not" and eq.children[0].name == "f_exist":
        return TreeNode("f_forall", [eq.children[0].children[0], eq.children[0].children[1].fx("not")])
    if eq.name == "f_exist" and eq.children[1].name == "f_or":
        return TreeNode("f_or", [TreeNode("f_exist", [eq.children[0], child]) for child in eq.children[1].children])
    if eq.name == "f_forall" and eq.children[1].name == "f_and":
        return TreeNode("f_and", [TreeNode("f_forall", [eq.children[0], child]) for child in eq.children[1].children])
    if eq.name == "f_exist":
        return TreeNode("f_forall", [eq.children[0], eq.children[1].fx("not")]).fx("not")
    return TreeNode(eq.name, [logic3(child) for child in eq.children])
def logic2(eq):
    if eq.name in ["f_exist", "f_forall"]:
        return TreeNode(eq.name, [eq.children[0], logic2(eq.children[1])])
    if eq.name not in ["f_and", "f_or", "f_not", "f_imply", "f_equiv"]:
        return eq
    def convv(eq):
        if eq == tree_form("s_true"):
            return True
        if eq == tree_form("s_false"):
            return False
        return None
    def conv2(val):
        if val:
            return tree_form("s_true")
        return tree_form("s_false")
    if all(convv(child) is not None for child in eq.children):
        if eq.name == "f_not":
            return conv2(not convv(eq.children[0]))
        elif eq.name == "f_or":
            return conv2(convv(eq.children[0]) or convv(eq.children[1]))
        elif eq.name == "f_and":
            return conv2(convv(eq.children[0]) and convv(eq.children[1]))
    if eq == tree_form("s_false").fx("not"):
        return tree_form("s_true")
    if eq.name == "f_not":
        if eq.children[0].name == "f_not":
            return eq.children[0].children[0]
        elif eq.children[0].name in ["f_or", "f_and"]:
            out = TreeNode({"f_or":"f_and", "f_and":"f_or"}[eq.children[0].name], [])
            for child in eq.children[0].children:
                out.children.append(child.fx("not"))
            return out
    if eq.name in ["f_and", "f_or"]:
        for i in range(len(eq.children)):
            for j in range(len(eq.children)):
                if i ==j:
                    continue
                if eq.children[i] == eq.children[j].fx("not"):
                    eq2 = copy.deepcopy(eq)
                    eq2.children.pop(max(i, j))
                    eq2.children.pop(min(i, j))
                    eq2.children.append({"f_or":tree_form("s_true"), "f_and":tree_form("s_false")}[eq.name])
                    if len(eq2.children) == 1:
                        return eq2.children[0]
                    return eq2
    if eq.name in ["f_and", "f_or"]:
        for i in range(len(eq.children)):
            if eq.children[i] == tree_form("s_false"):
                eq2 = copy.deepcopy(eq)
                eq2.children.pop(i)
                if eq.name == "f_and":
                    return tree_form("s_false")
                if len(eq2.children) == 1:
                    return eq2.children[0]
                return eq2
            elif eq.children[i] == tree_form("s_true"):
                eq2 = copy.deepcopy(eq)
                eq2.children.pop(i)
                if eq.name == "f_or":
                    return tree_form("s_true")
                if len(eq2.children) == 1:
                    return eq2.children[0]
                return eq2
    if eq.name in ["f_and", "f_or"]:
        lst = remove_duplicates_custom(eq.children, lambda x,y: x==y)
        if len(lst) < len(eq.children):
            if len(lst) == 1:
                return lst[0]
            return TreeNode(eq.name, lst)
    
    if eq.name in ["f_and", "f_or"] and any(child.children is not None and len(child.children)!=0 for child in eq.children):
        for i in range(len(eq.children),1,-1):
            for item in itertools.combinations(enumerate(eq.children), i):
                op = "f_and"
                if eq.name == "f_and":
                    op = "f_or"
                item3 = []
                for item4 in item:
                    item3.append(item4[0])
                item5 = []
                for item4 in item:
                    item5.append(item4[1])
                item = item5
                out = None
                for j in range(len(item)):
                    out = set(item[j].children)
                    for item2 in item:
                        if item2.name == op:
                            out = out & set(item2.children)
                        else:
                            out = out & set([item2])
                    if out == set(item[j].children):
                        break
                    out = None
                if out is None:
                    continue
                out = list(out)
                if out == []:
                    continue
                if len(out) != 1:
                    out = [TreeNode(op, out)]
                for item4 in list(set(range(len(eq.children))) - set(item3)):
                    out.append(eq.children[item4])
                if len(out) == 1:
                    return out[0]
                output = flatten_tree(TreeNode(eq.name, out))
                return output
    return TreeNode(eq.name, [flatten_tree(logic2(child)) for child in eq.children])
def logic1(eq):
    def helper(eq):
        if eq.name in ["f_exist", "f_forall"]:
            return TreeNode(eq.name, [eq.children[0], logic1(eq.children[1])])
        if eq.name not in ["f_and", "f_or", "f_not", "f_imply", "f_equiv"]:
            return eq
        if eq.name == "f_equiv":
            A, B = eq.children
            A, B = logic1(A), logic1(B)
            A, B = dowhile(A, logic2), dowhile(B, logic2)
            return flatten_tree((A & B) | (A.fx("not") & B.fx("not")))
        if eq.name == "f_imply":
            
            A, B = eq.children
            A, B = logic1(A), logic1(B)
            A, B = dowhile(A, logic2), dowhile(B, logic2)
            return flatten_tree(A.fx("not") | B)
        return TreeNode(eq.name, [helper(child) for child in eq.children])
    if eq.name in ["f_exist", "f_forall"]:
        return TreeNode(eq.name, [eq.children[0], logic1(eq.children[1])])
    if eq.name not in ["f_and", "f_or", "f_not", "f_imply", "f_equiv"]:
        return eq
    eq = helper(eq)
    eq = flatten_tree(eq)
    
    if len(eq.children) > 2:
        lst = []
        l = len(eq.children)

        # Handle last odd child directly
        if l % 2 == 1:
            last_child = eq.children[-1]
            # expand/simplify only if needed
            if isinstance(last_child, TreeNode):
                last_child = dowhile(last_child, logic2)
            lst.append(last_child)
            l -= 1

        # Pairwise combine children
        for i in range(0, l, 2):
            left, right = eq.children[i], eq.children[i+1]
            pair = TreeNode(eq.name, [left, right])
            simplified = dowhile(logic1(pair), logic2)
            lst.append(simplified)

        # If only one element left, just return it instead of nesting
        if len(lst) == 1:
            return flatten_tree(lst[0])

        # Otherwise rewrap
        return flatten_tree(TreeNode(eq.name, lst))

    if eq.name == "f_and":
        lst= []
        for child in eq.children:
            if child.name == "f_or":
                lst.append(child.children)
            else:
                lst.append([child])
        out = TreeNode("f_or", [])
        for item in itertools.product(*lst):
            c = TreeNode("f_and", list(item))
            out.children.append(c)
        if len(out.children) == 1:
            out = out.children[0]
        return flatten_tree(out)
    elif eq.name == "f_or":
        lst= []
        for child in eq.children:
            if child.name == "f_and":
                lst.append(child.children)
            else:
                lst.append([child])
        out = TreeNode("f_and", [])
        for item in itertools.product(*lst):
            c = TreeNode("f_or", list(item))
            out.children.append(c)
        if len(out.children) == 1:
            out = out.children[0]
        return flatten_tree(out)
    return TreeNode(eq.name, [logic1(child) for child in eq.children])
