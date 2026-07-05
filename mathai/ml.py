import itertools
from .parser import parse, remove_extra_brackets
from .simplify import simplify, addition_node_mat
from .base import *
import random
import copy
import math
def matrix_solve_ml(eq):
    def helper_matrix(eq):
        if eq.name == "f_transpose" and eq.children[0].name == "f_cap":
            eq2 = eq.children[0]
            return TreeNode("f_cap", [eq2.children[1], eq2.children[0], eq2.children[3], eq2.children[2], eq2.children[4]])
        if eq.name == "f_hadamard":
            con = Fraction(1)
            count = 0
            eq2 = copy.deepcopy(eq)
            for i in range(len(eq2.children)-1,-1,-1):
                if frac(eq2.children[i]) is not None:
                    con *= frac(eq2.children.pop(i))
                    count += 1
            if count > 1:
                if len(eq2.children) == 0:
                    return frac_to_tree(con)
                else:
                    eq2.children.append(frac_to_tree(con))
                    return eq2        
        if eq.name == "f_sigmoid" and len(eq.children) == 2 and eq.children[0].name == "d_1":
            eq2 = eq.children[1].fx("sigmoid")
            eq3 = TreeNode("f_wadd", [tree_form("d_1") , TreeNode("f_hadamard", [tree_form("d_-1"), eq2])])
            return TreeNode("f_hadamard", [eq2, eq3])
        if eq.name == "f_transpose" and eq.children[0].name == "f_cap":
            eq2 = eq.children[0]
            return TreeNode("f_cap", [eq2.children[1], eq2.children[0], eq2.children[3], eq2.children[2], eq2.children[4]])
        if eq.name == "f_transpose" and eq.children[0].name == "f_kronecker":
            return TreeNode("f_kronecker", [child.fx("transpose") for child in eq.children[0].children])
        if eq.name == "f_transpose" and eq.children[0].name in ["f_identity", "f_diag"]:
            return eq.children[0]
        if eq.name == "f_transpose" and eq.children[0].name in ["f_transpose"]:
            return eq.children[0].children[0]
        if eq.name in ["f_hadamard", "f_wmul"]:
            if tree_form("d_0") in eq.children:
                return tree_form("d_0")
            if tree_form("d_1") in eq.children and eq.name == "f_hadamard":
                out = [child for child in eq.children if child != tree_form("d_1")]
                if out == []:
                    return tree_form("d_1")
                if len(out) == 1:
                    return out[0]
                return TreeNode(eq.name, out)
        if eq.name in ["f_wmul"]:
            if any(child.name.startswith("f_identity") for child in eq.children):
                out = [child for child in eq.children if not child.name.startswith("f_identity")]
                if out == []:
                    return eq.children[0]
                if len(out) == 1:
                    return out[0]
                return TreeNode(eq.name, out)
        if eq.name in ["f_wadd"]:
            if tree_form("d_0") in eq.children:
                out = [child for child in eq.children if child != tree_form("d_0")]
                if out == []:
                    return tree_form("d_0")
                if len(out) == 1:
                    return out[0]
                return TreeNode(eq.name, out)
        return eq
    fx = lambda x: transform_dfs(simplify(x), helper_matrix)
    eq = dowhile(eq, fx)
    fx = lambda x: addition_node_mat(flatten_tree(x))
    eq = dowhile(eq, fx)
    return eq

def diff_matrix_scalar(eq):
    def helper(eq):
        name = eq.name
        mat = False
        if name in ["f_dif", "f_pdif"]:
            if "v_" not in str_form(eq.children[0]):
                return tree_form("d_0")
        if name in ["f_dif", "f_pdif"] and contain2(eq.children[1],"f_index"):
            mat = True
            v = eq.children[1].children[0]
            d = 1
            if eq.children[1].children[0].name == "f_index":
                d = 2
                v = eq.children[1].children[0].children[0]
            if not contain(eq.children[0], v) and name == "f_pdif":
                return tree_form("d_0")
            if eq.children[1].children[0] == eq.children[0]:
                return TreeNode("f_cap", [tree_form("d_1"), eq.children[0].fx("len"), tree_form("d_0"), eq.children[1].children[1], tree_form("d_1")])
            if d ==2 and eq.children[1].children[0].children[0] == eq.children[0]:
                return TreeNode("f_cap", [eq.children[0].fx("len"),\
                                          TreeNode("f_index", [eq.children[0], tree_form("d_0")]).fx("len"),\
                                          eq.children[1].children[0].children[1], eq.children[1].children[1], tree_form("d_1")])
        if name in ["f_dif", "f_pdif"]:
            if eq.children[0].name == "f_list":
                return TreeNode("f_list", [TreeNode(name, [child, eq.children[1]]) for child in eq.children[0].children])
            if eq.children[0].name == "f_transpose":
                return TreeNode(name, [eq.children[0].children[0], eq.children[1]]).fx("transpose")
            if eq.children[0].name in ["f_add", "f_wadd"]:
                return operation(eq.children[0].name, [TreeNode(name, [child, eq.children[1]]) for child in eq.children[0].children])
            if eq.children[0].name in ["f_mul", "f_wmul", "f_hadamard"]:
                op = eq.children[0].name
                op2 = "f_wadd"
                if op == "f_mul":
                    op2 = "f_add"
                tmp = operation(op2, [operation(op,[TreeNode(name, [child, eq.children[1]]) if index == index2 else child \
                                                    for index2, child in enumerate(eq.children[0].children)]) for index in range(len(eq.children[0].children))])
                return tmp
            if eq.children[0].name in ["f_F", "f_G", "f_sigmoid"]:
                op = "f_hadamard"               
                if len(eq.children[0].children) == 1:
                    a = TreeNode(eq.children[0].name, [tree_form("d_1"), eq.children[0].children[0]])
                    b = TreeNode(name, [eq.children[0].children[0], eq.children[1]])
                    return TreeNode(op, [a,b])
                else:
                    a = TreeNode(eq.children[0].name, [tree_form("d_1")+eq.children[0].children[0], eq.children[0].children[1]])
                    b = TreeNode(name, [eq.children[0].children[1], eq.children[1]])
                    return TreeNode(op, [a,b])
        return eq
    if eq is None:
        return None
    return dowhile(eq, lambda x: transform_dfs(x, helper))
def is_scalar(eq):
    if len(vlist(eq)) == 0:
        return True
    return all(str(tree_form(item)).islower() for item in vlist(eq))
def diff_matrix_matrix(eq):
    def helper(eq):
        name = eq.name
        if name in ["f_dif", "f_pdif"]:
            if is_scalar(eq.children[0]):
                return tree_form("d_0")
        if name in ["f_dif", "f_pdif"] and eq.children[0].name in ["f_mul", "f_hadamard"]:
            children = eq.children[0].children
            if any(is_scalar(child) for child in children):
                const = [child for child in children if is_scalar(child)]
                vars = [child for child in children if not is_scalar(child)]
                inner = vars[0] if len(vars) == 1 else TreeNode(eq.children[0].name, vars)
                outer = const[0] if len(const) == 1 else TreeNode(eq.children[0].name, const)
                lhs = inner
                if eq.children[0].name == "f_vec":
                    lhs = inner.fx("vec")
                return TreeNode(
                    eq.children[0].name,
                    [
                        outer,
                        TreeNode(eq.name, [lhs, eq.children[1]])
                    ]
                )
        if name in ["f_dif", "f_pdif"] and eq.children[1].name == "f_vec" and eq.children[0].name == "f_vec" and eq.children[0] != parse("W").fx("vec"):
            expr = copy.deepcopy(eq.children[0].children[0])
            if expr.name in ["f_F", "f_G", "f_sigmoid"]:
                op = "f_wmul"
                out = None
                if len(expr.children) == 1:
                    a = TreeNode(expr.name, [tree_form("d_1"), expr.children[0]])
                    b = TreeNode(name, [expr.children[0].fx("vec"), eq.children[1]])
                    out = TreeNode(op, [a.fx("vec").fx("diag"),b])
                else:
                    a = TreeNode(expr.name, [tree_form("d_1")+expr.children[0], expr.children[1]])
                    b = TreeNode(name, [expr.children[1].fx("vec"), eq.children[1]])
                    out = TreeNode(op, [a.fx("vec").fx("diag"),b])
                return out
            if expr.name == "f_transpose":
                a = expr.children[0]
                return TreeNode("f_wmul", [
                    a.fx("commutation"),
                    TreeNode(name, [a.fx("vec"), eq.children[1]])
                ])
            if expr.name == "f_index":
                return tree_form("d_0")
            if expr.name.startswith("v_"):
                if expr != eq.children[1].children[0]:
                    return tree_form("d_0")
                a = expr.fx("len")
                b = TreeNode("f_index", [expr, tree_form("d_0")]).fx("len")
                return (a * b).fx("identity")
            if expr.name == "f_broadcast" and expr.children[0].name.startswith("v_"):
                if expr.children[0] != eq.children[1].children[0]:
                    return tree_form("d_0")
                a = expr.children[0].fx("len")
                b = TreeNode("f_index", [expr.children[0], tree_form("d_0")]).fx("len")
                c = expr.children[1]
                z = TreeNode("f_zeros", [c, tree_form("d_1")])
                z = TreeNode("f_wadd", [tree_form("d_1"), z])
                return TreeNode("f_kronecker", [z,b.fx("identity")])
            if expr.name == "f_wadd":
                return operation("f_wadd", [
                    TreeNode(name, [child.fx("vec"), eq.children[1]])
                    for child in expr.children
                ])
            if expr.name == "f_wmul":
                lst = []
                children = expr.children
                target = eq.children[1]
                for i in range(len(children)):
                    lhs = children[:i]
                    rhs = children[i + 1:]
                    if rhs:
                        kron_left = operation("f_wmul", rhs).fx("transpose")
                    else:
                        kron_left = TreeNode(
                            "f_index",
                            [children[i], tree_form("d_0")]
                        ).fx("len").fx("identity")
                    if lhs:
                        kron_right = operation("f_wmul", lhs)
                    else:
                        kron_right = children[i].fx("len").fx("identity")
                    lst.append(TreeNode("f_wmul", [
                        TreeNode("f_kronecker", [kron_left, kron_right]),
                        TreeNode(name, [children[i].fx("vec"), target])
                    ]))
                return operation("f_wadd", lst)
            if expr.name == "f_hadamard":
                lst = []
                children = expr.children
                target = eq.children[1]

                for i in range(len(children)):
                    other = children[:i] + children[i + 1:]

                    if len(other) == 0:
                        diag = children[i].fx("len").fx("identity")
                    elif len(other) == 1:
                        diag = other[0].fx("diag")
                    else:
                        diag = TreeNode(
                            "f_hadamard",
                            other
                        ).fx("diag")

                    lst.append(TreeNode("f_wmul", [
                        diag,
                        TreeNode(name, [children[i].fx("vec"), target])
                    ]))
                return operation("f_wadd", lst)
        elif name in ["f_dif", "f_pdif"] and eq.children[1].name == "f_vec":
            lhs = eq.children[0]
            if lhs.name == "f_wadd":
                return operation("f_wadd", [TreeNode(name, [child, eq.children[1]]) for child in lhs.children])
            if lhs.name == "f_wmul":
                ch = lhs.children
                if len(ch) == 2:
                    if ch[0].name == "f_transpose" and ch[0].children[0] == ch[1]:
                        n = TreeNode(name, [ch[1], eq.children[1]]).fx("transpose")
                        return TreeNode("f_wmul", [n, TreeNode("f_hadamard", [tree_form("d_2"),ch[1]])])
        return eq
    if eq is None:
        return None
    return dowhile(eq, lambda x: transform_dfs(x, helper))
def power(a,b):
    pass
def zeros(a, b):
    arr = []
    for i in range(a):
        arr.append([])
        for j in range(b):
            arr[-1].append(0)
    return arr
def randos(low,high,a, b):
    arr = []
    for i in range(a):
        arr.append([])
        for j in range(b):
            tmp = random.uniform(low,high)
            arr[-1].append(tmp)
    return arr
def commutation(X):
    m = len(X)
    n = len(X[0]) if m > 0 else 0
    size = m * n
    K = [[0] * size for _ in range(size)]
    for i in range(m):
        for j in range(n):
            row_idx = j * m + i
            col_idx = i * n + j
            K[row_idx][col_idx] = 1
    return K
def hadamard_h(A, B):
    assert len(A)==len(B)
    assert len(A[0])==len(B[0])
    rows = len(A)
    cols = len(A[0])
    tmp = [
        [
            A[i][j] * B[i][j]
            for j in range(cols)
        ]
        for i in range(rows)
    ]
    return tmp
def hadamard(*lst_prod):
    result = lst_prod[0]
    for x in lst_prod[1:]:
        if isinstance(x, list) and isinstance(result, list):
            result = hadamard_h(result, x)
        elif isinstance(x, list):
            result = apply(x, lambda y: y*result)
        elif isinstance(result, list):
            result = apply(result, lambda y: y*x)
        else:
            result = result * x
    return result
def matadd_h(A, B):
    rows = len(A)
    cols = len(A[0])
    return [
        [
            A[i][j] + B[i][j]
            for j in range(cols)
        ]
        for i in range(rows)
    ]
def matmul_h(A, B):
    assert len(A[0])==len(B)
    rows = len(A)
    inner = len(A[0])
    cols = len(B[0])
    C = []
    for i in range(rows):
        row = []
        for j in range(cols):
            expr = 0
            for k in range(inner):
                left = A[i][k]
                right = B[k][j]
                expr = expr + (left * right)
            row.append(expr)
        C.append(row)
    return C
def kronecker_h(A, B):
    m = len(A)
    n = len(A[0])
    p = len(B)
    q = len(B[0])
    result = [[0] * (n * q) for _ in range(m * p)]
    for i in range(m):
        for j in range(n):
            for r in range(p):
                for c in range(q):
                    row_target = i * p + r
                    col_target = j * q + c
                    result[row_target][col_target] = A[i][j] * B[r][c]
    return result
def kronecker(*lst_prod):
    result = lst_prod[0]
    for x in lst_prod[1:]:
        result = kronecker_h(result, x)
    return result
def matmul(*lst_prod):
    result = lst_prod[0]
    for x in lst_prod[1:]:
        if isinstance(x, list) and isinstance(result, list):
            result = matmul_h(result, x)
        elif isinstance(x, list):
            result = apply(x, lambda y: y*result)
        elif isinstance(result, list):
            result = apply(result, lambda y: y*x)
        else:
            result = result * x
    return result
def matadd(*lst_prod):
    result = lst_prod[0]
    for x in lst_prod[1:]:
        if isinstance(x, list) and isinstance(result, list):
            result = matadd_h(result, x)
        elif isinstance(x, list):
            result = apply(x, lambda y: y+result)
        elif isinstance(result, list):
            result = apply(result, lambda y: y+x)
        else:
            result = result + x
    return result
def transpose(A):
    rows = len(A)
    cols = len(A[0])
    return [
        [
            A[i][j]
            for i in range(rows)
        ]
        for j in range(cols)
    ]
def shape(s):
    if not isinstance(s[0], list):
        return [len(s)]
    return [len(s), len(s[0])]  
def apply(arr, fx):
    if isinstance(arr, list):
        return [apply(item, fx) for item in arr]
    return fx(arr)
def exp(arg):
    return apply(arg, lambda x: math.exp(x))
def tanh(arg):
    return apply(arg, lambda x: math.tanh(x))
def sigmoid(arg):
    return apply(arg, lambda x: 1.0/(1.0 + math.exp(-x) ))
def identity(size):
    arr = zeros(size,size)
    for i in range(size):
        for j in range(size):
            if i == j:
                arr[i][j] = 1.0
    return arr
def vec(A):
    return [[A[i][j]] for j in range(len(A[0])) for i in range(len(A))]
def diag(v):
    if len(v) > 0 and isinstance(v[0], list):
        v = [x[0] for x in v]
    n = len(v)
    return [[v[i] if i == j else 0 for j in range(n)] for i in range(n)]
def broadcast(M, rows):
    r = len(M)
    c = len(M[0])
    
    # Broadcast rows
    if r == 1 and rows > 1:
        M = [M[0].copy() for _ in range(rows)]
        return M
    elif r != rows:
        raise ValueError(f"Cannot broadcast {r} rows to {rows}")
    return M
def reshape(jacobian, rows, cols):
    flat = []
    if len(jacobian) > 0 and isinstance(jacobian[0], list):
        for j in range(len(jacobian[0])):
            for i in range(len(jacobian)):
                flat.append(jacobian[i][j])
    else:
        flat = list(jacobian)
    if len(flat) != rows * cols:
        raise ValueError("Cannot reshape: incompatible dimensions.")
    return [[flat[j * rows + i] for j in range(cols)] for i in range(rows)]
def gen2(eq, w, active):
    def from_treenode(eq):
        nonlocal w, active
        
        alter = {"f_wadd":"matadd", "f_transpose":"transpose", "f_diag":"diag", "f_identity":"identity","f_wmul":"matmul", "f_mul":"hadamard",\
                 "f_kronecker":"kronecker", "f_commutation":"commutation","f_wmul":"matmul", "f_hadamard":"hadamard", "f_cap":"cap",\
                 "f_sigmoid":"sigmoid", "f_vec":"vec", "f_wpow":"pow", "f_exp":"exp", "f_len":"len", "f_reshape":"reshape", "f_broadcast":"broadcast", "f_zeros":"zeros"}
        if eq.name in alter.keys():
            return alter[eq.name]+"("+",".join([from_treenode(child) for child in eq.children])+")"

        alter2 = {"f_add":"+", "f_pow":"**"}
        if eq.name in alter2.keys():
            return alter2[eq.name].join([from_treenode(child) for child in eq.children])
        
        if eq.name == "f_index":
            return from_treenode(eq.children[0])+"["+from_treenode(eq.children[1])+"]"
        if eq.name == "f_list":
            return "["+",".join([from_treenode(child) for child in eq.children])+"]"
        if eq in w:
            return f"w[{w.index(eq)}]"

        return str(eq)
    return from_treenode(eq)
class NeuralNetwork:
    def __init__(self, struct, rand_range=None, active=None):
        self.struct = struct
        self.update_fx = {}
        self.var_list = [tree_form(f"v_-{i}") for i in range(1,26+1-4) if tree_form(f"v_-{i}") not in []]
        if active is None:
            self.active = {"F":parse("wpow(wadd(1,exp(hadamard(-1,Z))),-1)"), "G":parse("wpow(wadd(1,exp(hadamard(-1,Z))),-1)")}
            self.active = {"F":matrix_solve_ml(parse("sigmoid(Z)")), "G":matrix_solve_ml(parse("sigmoid(Z)"))}
        else:
            self.active = active
        self.o = None
        self.lst_w = None
        self.gradient = None
        self.learn = None
        self.bc = 1
        self.loss = None
        self.model_type = None
        if rand_range is None:
            self.init_mat = lambda x,y: zeros(x, y)
        else:
            self.init_mat = lambda x,y: randos(rand_range[0], rand_range[1], x, y)
    def model(self, t="dense"):
        self.model_type = t
        if t == "dense":
            return self.model_dense()
        elif t == "cnn":
            return self.model_cnn()
        else:
            return self.model_rnn_vanilla()
    def model_cnn(self):
        pass
    def model_dense(self):
        lst_z = []
        lst_w = []
        lst_b = []
        x = parse("X")
        y = parse("Y")
        var_i = tree_form("v_11")
        var_j = parse("j")
        lst_z.append(x)
        for i in range(len(self.struct)-1):
            lst_w.append(self.var_list.pop(0))
            lst_b.append(self.var_list.pop(0))
            eq = TreeNode("f_wmul", [lst_z[-1], lst_w[-1]])
            eq = TreeNode("f_wadd", [eq, TreeNode("f_broadcast", [lst_b[-1], parse("m")])])
            lst_z.append(eq)
            tmp = replace(self.active["F"], parse("Z"), lst_z[-1])
            lst_z[-1] = matrix_solve_ml(tmp)
        self.lst_w = lst_w + lst_b
        self.o = lst_z[-1]
        eq = TreeNode("f_hadamard", [tree_form("d_-1"), y])
        eq = TreeNode("f_wadd", [self.o, eq])
        eq = eq.fx("vec")
        eq = TreeNode("f_wmul", [eq.fx("transpose"), eq])
        L = TreeNode("f_hadamard", [eq, (parse("m") * tree_form("d_2")) ** tree_form("d_-1")])
        self.loss = L
        gradient = []
        for i in range(2):
            for j in range(len(self.struct)-1):
                item = [lst_w, lst_b][i][j]
                tmp = TreeNode("f_pdif", [L, item.fx("vec")])
                tmp = matrix_solve_ml(diff_matrix_matrix(tmp))
                tmp = TreeNode("f_reshape", [tmp, item.fx("len"), TreeNode("f_index", [item, tree_form("d_0")]).fx("len")])
                eq = TreeNode("f_hadamard",[tree_form("d_-1"), parse("n"),tmp])
                eq = TreeNode("f_wadd", [item, eq])
                eq = matrix_solve_ml(eq)
                gradient.append(eq)
        self.gradient = gradient
        lst_1 = []
        lst_2 = []
        for i in range(1,len(self.struct)):
            lst_1.append(self.init_mat(1, self.struct[i]))
            lst_2.append(self.init_mat(self.struct[i-1], self.struct[i]))
        self.learn = lst_2 + lst_1
        return self
    def model_rnn_vanilla(self):
        Hht0 = self.var_list.pop(0)
        lst_Hht = [Hht0]
        Wxh = self.var_list.pop(0)
        Whh = self.var_list.pop(0)
        Why = self.var_list.pop(0)
        Bh = self.var_list.pop(0)
        By = self.var_list.pop(0)
        var_i = tree_form("v_11")
        var_j = parse("j")
        x = parse("X")
        y = parse("Y")
        lst_o = []
        for i in range(self.struct[3]):
            eq = TreeNode("f_wadd", [TreeNode("f_wmul", [TreeNode("f_index",[x,tree_form(f"d_{i}")]) , Wxh]) , TreeNode("f_wmul", [lst_Hht[i], Whh]) , Bh])
            tmp = replace(self.active["F"], parse("Z"), eq)
            lst_Hht.append(tmp)
            eq = TreeNode("f_wadd", [TreeNode("f_wmul", [lst_Hht[-1], Why]) , By])
            tmp = replace(self.active["G"], parse("Z"), eq)
            lst_o.append(tmp)
        self.lst_w = [Wxh, Whh, Why, By, Bh, Hht0]
        self.o = lst_o
        L_lst = []
        for i, item in enumerate(self.o):
            eq = TreeNode("f_index", [y, tree_form(f"d_{i}")])
            eq = TreeNode("f_hadamard", [eq, tree_form("d_-1")])
            eq = TreeNode("f_wadd", [eq, item]).fx("vec")
            eq = TreeNode("f_wmul", [eq.fx("transpose"), eq])
            eq = TreeNode("f_hadamard", [eq, tree_form("d_2") ** tree_form("d_-1")])
            L_lst.append(eq)
        L = operation("f_wadd", L_lst)
        L = matrix_solve_ml(L)
        gradient = []
        # x, y, h, t
        for i in range(2):
            for j in range(3):
                item = [[Wxh, Whh, Why], [By, Bh, Hht0]][i][j]
                tmp = TreeNode("f_pdif", [L, item.fx("vec")])
                tmp = matrix_solve_ml(diff_matrix_matrix(tmp))
                tmp = TreeNode("f_reshape", [tmp, item.fx("len"), TreeNode("f_index", [item, tree_form("d_0")]).fx("len")])
                eq = TreeNode("f_hadamard", [tree_form("d_-1"), parse("n"),tmp])
                eq = TreeNode("f_wadd", [item, eq])
                eq = matrix_solve_ml(eq)
                gradient.append(eq)
        self.gradient = gradient
        lst_1 = []
        for item in [[self.struct[0], self.struct[2]], [self.struct[2], self.struct[2]], [self.struct[2], self.struct[1]],\
                     [1, self.struct[1]], [1, self.struct[2]], [1,self.struct[2]]]:
            lst_1.append(self.init_mat(*item))
        self.learn = lst_1
        return self
    def predict(self, given_x):
        global exp, hadamard, zeros, transpose, matadd, tanh, sigmoid, identity, power, vec, diag, matmul, reshape, kronecker, commutation, broadcast
        env = {
            "w": self.learn,
            "m": 1,
            "identity":identity,
            "X": [given_x] if self.model_type == "dense" else transpose([transpose(given_x)]),
            "zeros":zeros,
            "hadamard": hadamard,
            "exp":exp,
            "tanh":tanh,
            "sigmoid":sigmoid,
            "matadd": matadd,
            "pow":power,
            "diag":diag,
            "vec":vec,
            "matmul":matmul,
            "reshape":reshape,
            "kronecker":kronecker,
            "commutation":commutation,
            "transpose":transpose,
            "broadcast":broadcast
        }
        if self.model_type == "dense":
            return eval(gen2(self.o, self.lst_w, self.active), {}, env)[0]
        else:
            return transpose([eval(gen2(item, self.lst_w, self.active), {}, env)[0] for item in self.o])
    def train(self, train_x, train_y, learning_rate, epoch, batch_size=1):
        global exp, hadamard, zeros, transpose, matadd, tanh, sigmoid, identity, power, vec, diag, matmul, reshape, kronecker, commutation, broadcast
        self.bc = batch_size
        if self.model_type != "dense":
            train_x = [transpose(item) for item in train_x]
            train_y = [transpose(item) for item in train_y]
        env = {
            "identity":identity,
            "n": learning_rate,
            "sigmoid":sigmoid,
            "matadd": matadd,
            "zeros":zeros,
            "hadamard": hadamard,
            "exp":exp,
            "tanh":tanh,
            "pow":power,
            "diag":diag,
            "vec":vec,
            "matmul":matmul,
            "reshape":reshape,
            "kronecker":kronecker,
            "commutation":commutation,
            "transpose":transpose,
            "broadcast":broadcast
        }
        for j in range(len(self.lst_w)):
            tmp = f"fx_{j} = lambda X,Y,w,m: "+gen2(self.gradient[j], self.lst_w, self.active)
            exec(tmp, env)
        def make_batches(data, batch_size):
            return [
                data[i:i + batch_size]
                for i in range(0, len(data), batch_size)
            ]
        data_x_batch = None
        data_y_batch = None
        data_x = None
        data_y = None
        m = 1
        index_count = None
        if self.model_type == "dense":
            data_x_batch = make_batches(copy.deepcopy(train_x), batch_size)
            data_y_batch = make_batches(copy.deepcopy(train_y), batch_size)
            index_count = len(data_x_batch)
        else:
            index_count = len(train_x)
        for k in range(epoch):            
            for i in range(index_count):
                learn_new = copy.deepcopy(self.learn)
                if self.model_type != "dense":
                    data_x = transpose([train_x[i]])
                    data_y = transpose([train_y[i]])
                else:
                    data_x = data_x_batch[i]
                    m = len(data_x)
                    data_y = data_y_batch[i]
                for j in range(len(self.lst_w)):
                    learn_new[j] = env[f"fx_{j}"](data_x, data_y, self.learn, m)
                self.learn = copy.deepcopy(learn_new)
            if k % round(epoch/10.0) == 0:
                print(f"epoches done {k+1}/{epoch}")
        print("training done.")
        print()
def cap(a, b, x, y, val):
    arr = zeros(a,b)
    arr[x][y] = val
    return arr
class NeuralNetworkScalar:
    def __init__(self, struct, rand_range=None, active=None):
        self.struct = struct
        self.update_fx = {}
        self.var_list = [tree_form(f"v_-{i}") for i in range(1,26+1-4) if tree_form(f"v_-{i}") not in [parse("G"), parse("F")]]
        if active is None:
            self.active = {"F":parse("sigmoid(Z)"), "G":parse("sigmoid(Z)")}
        else:
            self.active = active
        self.o = None
        self.lst_w = None
        self.gradient = None
        self.learn = None
        self.model_type = None
        if rand_range is None:
            self.init_mat = lambda x,y: zeros(x, y)
        else:
            self.init_mat = lambda x,y: randos(rand_range[0], rand_range[1], x, y)
    def model(self, t="dense"):
        self.model_type = t
        if t == "dense":
            return self.model_dense()
        else:
            return self.model_rnn_vanilla()
    def model_dense(self):
        lst_z = []
        lst_w = []
        lst_b = []
        x = parse("X")
        y = parse("Y")
        var_i = tree_form("v_11")
        var_j = parse("j")
        lst_z.append(x)
        for i in range(len(self.struct)-1):
            lst_w.append(self.var_list.pop(0))
            lst_b.append(self.var_list.pop(0))
            eq = TreeNode("f_wmul", [lst_z[-1], lst_w[-1]])
            eq = TreeNode("f_wadd", [eq, lst_b[-1]])
            lst_z.append(eq)
            tmp = replace(self.active["F"], parse("Z"), lst_z[-1])
            lst_z[-1] = matrix_solve_ml(tmp)
        self.lst_w = lst_w + lst_b
        self.o = lst_z[-1]
        eq = TreeNode("f_hadamard", [tree_form("d_-1"), y])
        eq = TreeNode("f_wadd", [self.o, eq])
        eq = copy.deepcopy(eq)
        L = TreeNode("f_wmul", [eq,eq.fx("transpose")])
        L = TreeNode("f_hadamard", [L, tree_form("d_2")**tree_form("d_-1")])
        L = matrix_solve_ml(L)
        gradient = []
        for i in range(2):
            for j in range(len(self.struct)-1):
                item = [lst_w, lst_b][i][j]
                if i == 0:
                    item = TreeNode("f_index", [TreeNode("f_index", [item, tree_form("v_11")]), parse("j")])
                else:
                    item = TreeNode("f_index", [TreeNode("f_index", [item, tree_form("d_0")]), parse("j")])
                tmp = diff_matrix_scalar(TreeNode("f_pdif", [L, item]))
                eq = TreeNode("f_hadamard", [tree_form("d_-1").fx("list").fx("list"), parse("n").fx("list").fx("list"),tmp])
                eq = TreeNode("f_wadd", [parse("z").fx("list").fx("list"), eq])
                eq = matrix_solve_ml(eq)
                gradient.append(eq)
        self.gradient = gradient
        lst_1 = []
        lst_2 = []
        for i in range(1,len(self.struct)):
            lst_1.append(self.init_mat(1, self.struct[i]))
            lst_2.append(self.init_mat(self.struct[i-1], self.struct[i]))
        self.learn = lst_2 + lst_1
        return self
    def model_rnn_vanilla(self):
        Hht0 = self.var_list.pop(0)
        lst_Hht = [Hht0]
        Wxh = self.var_list.pop(0)
        Whh = self.var_list.pop(0)
        Why = self.var_list.pop(0)
        Bh = self.var_list.pop(0)
        By = self.var_list.pop(0)
        var_i = tree_form("v_11")
        var_j = parse("j")
        x = parse("X")
        y = parse("Y")
        lst_o = []
        for i in range(self.struct[3]):
            eq = TreeNode("f_wadd", [TreeNode("f_wmul", [TreeNode("f_index",[x,tree_form(f"d_{i}")]) , Wxh]) , TreeNode("f_wmul", [lst_Hht[i], Whh]) , Bh])
            eq = replace(self.active["F"], parse("Z"), eq)
            lst_Hht.append(eq)
            eq = TreeNode("f_wadd", [TreeNode("f_wmul", [lst_Hht[-1], Why]) , By])
            eq = replace(self.active["G"], parse("Z"), eq)
            lst_o.append(eq)
        self.lst_w = [Wxh, Whh, Why, By, Bh, Hht0]
        self.o = lst_o
        L_lst = []
        for i, item in enumerate(self.o):
            eq = TreeNode("f_index", [y, tree_form(f"d_{i}")])
            eq = TreeNode("f_hadamard", [eq, tree_form("d_-1")])
            eq = TreeNode("f_wadd", [eq, item])
            eq = TreeNode("f_wmul", [eq, eq.fx("transpose")])
            eq = TreeNode("f_hadamard", [eq, tree_form("d_2") ** tree_form("d_-1")])
            L_lst.append(eq)
        L = operation("f_wadd", L_lst)
        L = matrix_solve_ml(L)
        gradient = []
        # x, y, h, t
        for i in range(2):
            for j in range(3):
                item = [[Wxh, Whh, Why], [By, Bh, Hht0]][i][j]
                if i == 0:
                    item = TreeNode("f_index", [TreeNode("f_index", [item, tree_form("v_11")]), parse("j")])
                else:
                    item = TreeNode("f_index", [TreeNode("f_index", [item, tree_form("d_0")]), parse("j")])
                tmp = diff_matrix_scalar(TreeNode("f_pdif", [L, item]))
                eq = TreeNode("f_hadamard", [tree_form("d_-1").fx("list").fx("list"), parse("n").fx("list").fx("list"),tmp])
                eq = TreeNode("f_wadd", [parse("z").fx("list").fx("list"), eq])
                eq = matrix_solve_ml(eq)
                gradient.append(eq)
        self.gradient = gradient
        lst_1 = []
        for item in [[self.struct[0], self.struct[2]], [self.struct[2], self.struct[2]], [self.struct[2], self.struct[1]],\
                     [1, self.struct[1]], [1, self.struct[2]], [1,self.struct[2]]]:
            lst_1.append(self.init_mat(*item))
        self.learn = lst_1
        return self
    def predict(self, given_x):
        global shape, exp, hadamard, zeros, matmul, transpose, matadd, tanh, sigmoid
        env = {
            "w": self.learn,
            "cap": cap,
            "X": [given_x] if self.model_type == "dense" else transpose([transpose(given_x)]),
            "transpose":transpose,
            "matmul":matmul,
            "zeros":zeros,
            "hadamard": hadamard,
            "exp":exp,
            "tanh":tanh,
            "sigmoid":sigmoid,
            "matadd": matadd
        }
        if self.model_type == "dense":
            return eval(gen2(self.o, self.lst_w, self.active), {}, env)[0]
        else:
            return transpose([eval(gen2(item, self.lst_w, self.active), {}, env)[0] for item in self.o])
    def train(self, train_x, train_y, learning_rate, epoch):
        global shape, exp, hadamard, zeros, matmul, transpose, matadd, tanh, sigmoid
        if self.model_type != "dense":
            train_x = [transpose(item) for item in train_x]
            train_y = [transpose(item) for item in train_y]
        env = {
            "cap": cap,
            "n": learning_rate,
            "sigmoid":sigmoid,
            "matadd": matadd,
            "transpose":transpose,
            "matmul":matmul,
            "zeros":zeros,
            "hadamard": hadamard,
            "exp":exp,
            "tanh":tanh
        }
        for j in range(len(self.lst_w)):
            tmp = f"fx_{j} = lambda z,X,Y,i,j,w: "+gen2(self.gradient[j], self.lst_w, self.active)
            exec(tmp, env)
        for k in range(epoch):            
            for data_index in range(len(train_x)):
                learn_new = copy.deepcopy(self.learn)
                data_x = None
                data_y = None
                if self.model_type == "dense":
                    data_x = [train_x[data_index]]
                    data_y = [train_y[data_index]]
                else:
                    data_x = transpose([train_x[data_index]])
                    data_y = transpose([train_y[data_index]])
                for j in range(len(self.lst_w)):
                    s = shape(self.learn[j])
                    for x in range(s[0]):
                        for y in range(s[1]):
                            z = self.learn[j][x][y]
                            out = env[f"fx_{j}"](z, data_x, data_y, x, y, self.learn)
                            learn_new[j][x][y] = out[0][0]
                self.learn = copy.deepcopy(learn_new)
            if k % round(epoch/10.0) == 0:
                print(f"epoches done {k+1}/{epoch}")
        print("training done.")
        print()
