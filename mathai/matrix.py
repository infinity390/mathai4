from .base import *
import copy
from .simplify import simplify
import itertools
def tree_to_py(node):
    if node.name=="f_list":
        return [tree_to_py(c) for c in node.children]
    return node
def py_to_tree(obj):
    if isinstance(obj,list):
        return TreeNode("f_list",[py_to_tree(x) for x in obj])
    return obj
def is_vector(x):
    return isinstance(x,list) and all(isinstance(item,TreeNode) for item in x)
def is_mat(x):
    return isinstance(x,list) and all(isinstance(item,list) for item in x)
def is_matrix(x):
    return isinstance(x, list) and all(isinstance(item, list) and (is_mat(item) or is_vector(item)) for item in x)
def dot(u,v):
    if len(u)!=len(v):
        raise ValueError("Vector size mismatch")
    s = tree_form("d_0")
    for a,b in zip(u,v):
        s = TreeNode("f_add",[s,TreeNode("f_mul",[a,b])])
    return s
def matmul(A, B):
    n = len(A)
    m = len(A[0])
    p = len(B[0])
    if m != len(B):
        raise ValueError("Matrix dimension mismatch")
    C = [[tree_form("d_0") for _ in range(p)] for _ in range(n)]
    for i in range(n):
        for j in range(p):
            for k in range(m):
                C[i][j] = TreeNode(
                    "f_add",
                    [C[i][j], TreeNode("f_mul", [A[i][k], B[k][j]])]
                )
    return C
def promote(node):
    if node.name=="f_list":
        return tree_to_py(node)
    return node
def contains_neg(node):
    if isinstance(node, list):
        return False
    if node.name.startswith("v_-"):
        return False
    for child in node.children:
        if not contains_neg(child):
            return False
    return True
def multiply(left,right):
    if left == tree_form("d_1"):
        return right
    if right == tree_form("d_1"):
        return left
    left2, right2 = left, right
    if left2.name != "f_pow":
        left2 = left2 ** 1
    if right2.name != "f_pow":
        right2 = right2 ** 1
    if left2.name == "f_pow" and right2.name == "f_pow" and left2.children[0]==right2.children[0]:
        return simplify(left2.children[0]**(left2.children[1]+right2.children[1]))
    A,B = promote(left), promote(right)
    if is_vector(A) and is_vector(B):
        return dot(A,B)
    if is_matrix(A) and is_matrix(B):
        return py_to_tree(matmul(A,B))
    for _ in range(2):
        if contains_neg(A) and is_vector(B):
            return py_to_tree([TreeNode("f_mul",[A,x]) for x in B])
        if contains_neg(A) and is_matrix(B):
            return py_to_tree([[TreeNode("f_mul",[A,x]) for x in row] for row in B])
        A, B = B, A
    return None
def add_vec(A, B):
    if len(A) != len(B):
        raise ValueError("Vector dimension mismatch")
    return [
        TreeNode("f_add", [A[i], B[i]])
        for i in range(len(A))
    ]
def matadd(A, B):
    if len(A) != len(B) or len(A[0]) != len(B[0]):
        raise ValueError("Matrix dimension mismatch")
    n = len(A)
    m = len(A[0])
    return [
        [
            TreeNode("f_add", [A[i][j], B[i][j]])
            for j in range(m)
        ]
        for i in range(n)
    ]
def addition(left,right):
    A,B = promote(left), promote(right)
    if is_vector(A) and is_vector(B):
        return add_vec(A,B)
    if is_matrix(A) and is_matrix(B):
        return py_to_tree(matadd(A,B))
    return None
def fold_wmul(root):
    stack = [(root, False)]
    newnode = {}
    while stack:
        node, visited = stack.pop()
        if not visited:
            stack.append((node, True))
            for child in node.children:
                stack.append((child, False))
        else:
            children = [newnode[c] for c in node.children]
            eq = TreeNode(node.name, children)
            if eq.name == "f_pow" and eq.children[1].name.startswith("d_"):
                n = int(eq.children[1].name[2:])
                if n == 1:
                    eq = eq.children[0]
                elif n > 1:
                    tmp = promote(eq.children[0])
                    if is_matrix(tmp):
                        orig = tmp
                        for _ in range(n - 1):
                            tmp = matmul(orig, tmp)
                        eq = py_to_tree(tmp)
            elif eq.name in ["f_wmul", "f_add"]:
                if len(eq.children) == 1:
                    eq = eq.children[0]
                else:
                    i = len(eq.children) - 1
                    while i > 0:
                        if eq.name == "f_wmul":
                            out = multiply(eq.children[i - 1], eq.children[i])
                        else:
                            out = addition(eq.children[i - 1], eq.children[i])
                        if out is not None:
                            eq.children.pop(i)
                            eq.children.pop(i - 1)
                            eq.children.insert(i - 1, out)
                        i -= 1
            newnode[node] = eq
    return newnode[root]
def flat(eq):
    return flatten_tree(eq)
def use(eq):
    return TreeNode(eq.name, [use(child) for child in eq.children])
def _matrix_solve(eq):
    eq = dowhile(eq, lambda x: fold_wmul(flat(x)))
    return eq
def matrix_solve(eq):
    return _matrix_solve(eq)
