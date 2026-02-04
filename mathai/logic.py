from .expand import expand
import itertools
from .base import *

def set_sub(eq):
  if eq.name == "f_sub":
    return eq.children[0] & eq.children[1].fx("not")
  return TreeNode(eq.name, [set_sub(child) for child in eq.children])
def compute_logic(eq):
     if eq is None:
          return None
     if eq.name == "s_true":
          return True
     if eq.name == "s_false":
          return False
     if eq.name == "f_and":
          return all(compute_logic(child) for child in eq.children)
     if eq.name == "f_or":
          return any(compute_logic(child) for child in eq.children)
     if eq.name == "f_not":
          return not compute_logic(eq.children[0])
     if eq.name == "f_equiv":
          return compute_logic(eq.children[0]) == compute_logic(eq.children[1])
     if eq.name == "f_imply":
          if compute_logic(eq.children[0]):
               return compute_logic(eq.children[1])
          return True
     return None
def truth_gen(eq):
     dic = {}
     count = 0
     lst = []
     false_count = 0
     truth_count = 0
     for item in vlist(eq):
          dic[count] = item
          count += 1
     for item in itertools.product([False,True], repeat=count):
          orig = copy.deepcopy(eq)
          for i in range(count):
               eq = replace(eq, tree_form(dic[i]), tree_form("s_true") if item[i] else tree_form("s_false"))
          if compute_logic(eq):
               lst.append(list(item))
               truth_count += 1
          else:
               false_count += 1
          eq = orig
     if false_count == 0:
          return tree_form("s_true")
     elif truth_count == 0:
          return tree_form("s_false")
     outeq = []
     for item in lst:
          outeq2 = []
          for i in range(count):
               if item[i]:
                    outeq2.append(tree_form(dic[i]))
               else:
                    outeq2.append(tree_form(dic[i]).fx("not"))
          if len(outeq2) == 1:
               outeq2 = outeq2[0]
          else:
               outeq2 = TreeNode("f_and", outeq2)
          outeq.append(outeq2)
     if len(outeq) == 1:
          outeq = outeq[0]
     else:
          outeq = TreeNode("f_or", outeq)
     return outeq
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

def apply_absorption(eq: TreeNode) -> TreeNode:
    if eq.name not in ["f_and", "f_or"]:
        return eq
    eq.children = [apply_absorption(c) for c in eq.children]
    i = 0
    while i < len(eq.children):
        j = 0
        while j < len(eq.children):
            if i == j:
                j += 1
                continue
            a, b = eq.children[i], eq.children[j]
            if eq.name == "f_and" and b.name == "f_or" and a in b.children:
                eq.children.pop(j)
                if j < i:
                    i -= 1
                continue
            if eq.name == "f_or" and b.name == "f_and" and a in b.children:
                eq.children.pop(j)
                if j < i:
                    i -= 1
                continue
            j += 1
        i += 1
    if len(eq.children) == 1:
        return eq.children[0]
    return eq
def _logic2(eq):
    eq = flatten_tree(eq)
    if eq.name == "f_and":
        eq = and_all(eq.children)
    if eq.name == "f_or":
        eq = or_all(eq.children)
    
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
        lst = remove_duplicates_custom(eq.children, lambda x,y: x==y)
        if len(lst) < len(eq.children):
            if len(lst) == 1:
                return lst[0]
            return TreeNode(eq.name, lst)
    return eq
def logic2(eq):
     eq = _logic2(eq)
     return TreeNode(eq.name, [logic2(child) for child in eq.children])
def _logic4(eq):
     eq = flatten_tree(eq)
     if eq.name in ["f_or", "f_add"] and len(eq.children)>2:
          return eq
     if eq.name in ["f_or"]:
          out = []
          s = str_form(or_all(eq.children))
          s= s.replace("f_or","f_mul").replace("f_and", "f_add")
          s = tree_form(s)
          s = expand(s)
          s = str_form(s)
          s= s.replace("f_mul","f_or").replace("f_add", "f_and")
          s = tree_form(s)
          out.append(s)    
          return or_all(out)
     if eq.name in ["f_and"]:
          out = []
          s = str_form(and_all(eq.children))
          s= s.replace("f_or","f_add").replace("f_and", "f_mul")
          s = tree_form(s)
          s = expand(s)
          s = str_form(s)
          s= s.replace("f_add","f_or").replace("f_mul", "f_and")
          s = tree_form(s)
          out.append(s)    
          return and_all(out)
     return TreeNode(eq.name, [_logic4(child) for child in eq.children])
def logic4(eq):
     if eq.name == "f_or":
          item = eq.children[0]
          for i in range(1,len(eq.children)):
               item = _logic4(item | eq.children[i])
               item = dowhile(item, lambda x: apply_absorption(logic2(x)))
          return item
     if eq.name == "f_and":
          pass
     return eq
