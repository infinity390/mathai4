import itertools
from .parser import parse, remove_extra_brackets
from .simplify import simplify, addition_node_mat
from .base import *
import random
import copy
import math
def matrix_solve_ml(eq):
    def helper_matrix(eq):
        if eq.name == "f_vec" and eq.children[0].name == "f_flatten":
            return eq.children[0].children[0].fx("vec")
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
        if eq.name == "f_relu" and len(eq.children) == 2 and eq.children[0].name == "d_1":
            return eq.children[1].fx("drelu")
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

def is_scalar(eq):
    if len(vlist(eq)) == 0:
        return True
    return all(str(tree_form(item)).islower() for item in vlist(eq))
def diff_matrix_matrix(eq):
    def helper(eq):
        name = eq.name
        if eq.name == "f_vec" and eq.children[0].name == "f_vec":
            return eq.children[0]
        if eq.name == "f_vec" and eq.children[0].name == "f_flatten":
            return eq.children[0].children[0].fx("vec")
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
            if expr.name in ["f_F", "f_G", "f_sigmoid", "f_relu", "f_conv"]:
                op = "f_wmul"
                out = None
                if len(expr.children) == 1:
                    a = TreeNode(expr.name, [tree_form("d_1"), expr.children[0]])
                    b = TreeNode(name, [expr.children[0].fx("vec"), eq.children[1]])
                    out = TreeNode(op, [a.fx("vec").fx("diag"),b])
                elif len(expr.children) == 2 and expr.name == "f_conv":
                    kernel = expr.children[1]
                    image = expr.children[0]

                    # image shape = (B, H, W, C)
                    image_h = TreeNode(
                        "f_index",
                        [image, tree_form("d_0")]
                    ).fx("len")

                    image_w = TreeNode(
                        "f_index",
                        [
                            TreeNode("f_index", [image, tree_form("d_0")]),
                            tree_form("d_0")
                        ]
                    ).fx("len")

                    # kernel shape = (B, KH, KW, C)
                    kernel_h = TreeNode(
                        "f_index",
                        [kernel, tree_form("d_0")]
                    ).fx("len")

                    kernel_w = TreeNode(
                        "f_index",
                        [
                            TreeNode("f_index", [kernel, tree_form("d_0")]),
                            tree_form("d_0")
                        ]
                    ).fx("len")

                    patches_mat = TreeNode(
                        "f_patches",
                        [image, kernel_h, kernel_w]
                    )

                    toeplitz_mat = TreeNode(
                        "f_toeplitz",
                        [kernel, image_h, image_w]
                    )

                    dA = TreeNode(
                        name,
                        [image.fx("vec"), eq.children[1]]
                    )

                    dK = TreeNode(
                        name,
                        [kernel.children[0].fx("vec"), eq.children[1]]
                    )

                    out = TreeNode("f_wadd",[TreeNode("f_wmul", [toeplitz_mat, dA]),TreeNode("f_wmul", [patches_mat, dK])])
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
                b = expr.children[0].fx("vec").fx("len")
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
def zeros(*shape):
    if len(shape) == 0:
        return 0
    return [zeros(*shape[1:]) for _ in range(shape[0])]

def randos(low, high, *shape):
    if len(shape) == 0:
        return random.uniform(low, high)

    return [
        randos(low, high, *shape[1:])
        for _ in range(shape[0])
    ]
def flatten(A):
    def flatten_all(x):
        if not isinstance(x, list):
            return [x]
        out = []
        for item in x:
            out.extend(flatten_all(item))
        return out

    return [flatten_all(batch) for batch in A]
def conv(X, K):
    B = len(X)
    H = len(X[0])
    W = len(X[0][0])
    C = len(X[0][0][0])

    P = len(K[0])
    
    OH = H - P + 1
    OW = W - P + 1

    out = []

    for b in range(B):
        batch = []

        for i in range(OH):
            row = []

            for j in range(OW):
                s = 0.0

                for u in range(P):
                    for v in range(P):
                        for c in range(C):
                            s += X[b][i + u][j + v][c] * K[0][u][v][c]

                row.append([s])

            batch.append(row)

        out.append(batch)
        
    return out
def toeplitz(K, H, W):
    # K has shape (B, KH, KW, C)

    B = len(K)
    KH = len(K[0])
    KW = len(K[0][0])
    C = len(K[0][0][0])

    OH = H - KH + 1
    OW = W - KW + 1

    rows_per = OH * OW
    cols_per = H * W * C

    rows = B * rows_per
    cols = B * cols_per

    T = [[0 for _ in range(cols)] for _ in range(rows)]

    for b in range(B):
        row_offset = b * rows_per
        col_offset = b * cols_per

        row = 0

        for i in range(OH):
            for j in range(OW):

                for u in range(KH):
                    for v in range(KW):
                        for c in range(C):

                            col = ((i + u) * W + (j + v)) * C + c

                            T[row_offset + row][col_offset + col] = K[b][u][v][c]

                row += 1

    return T
def patches(X, kh, kw):
    B = len(X)
    H = len(X[0])
    W = len(X[0][0])
    C = len(X[0][0][0])

    OH = H - kh + 1
    OW = W - kw + 1

    out = []

    for b in range(B):
        for i in range(OH):
            for j in range(OW):

                patch = []

                for u in range(kh):
                    for v in range(kw):
                        for c in range(C):
                            patch.append(
                                X[b][i + u][j + v][c]
                            )

                out.append(patch)
    return out
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
    if isinstance(A, list) and isinstance(B, list):
        assert len(A) == len(B)
        return [hadamard_h(a, b) for a, b in zip(A, B)]

    return A * B


def hadamard(*lst_prod):
    result = lst_prod[0]

    for x in lst_prod[1:]:
        if isinstance(result, list) and isinstance(x, list):
            result = hadamard_h(result, x)
        elif isinstance(result, list):
            result = apply(result, lambda y: y * x)
        elif isinstance(x, list):
            result = apply(x, lambda y: result * y)
        else:
            result *= x

    return result
def matadd_h(A, B):
    if isinstance(A, list) and isinstance(B, list):
        assert len(A) == len(B)
        return [matadd_h(a, b) for a, b in zip(A, B)]

    return A + B


def matadd(*lst_prod):
    result = lst_prod[0]

    for x in lst_prod[1:]:
        if isinstance(result, list) and isinstance(x, list):
            result = matadd_h(result, x)
        elif isinstance(result, list):
            result = apply(result, lambda y: y + x)
        elif isinstance(x, list):
            result = apply(x, lambda y: result + y)
        else:
            result += x

    return result
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
def depth(x):
    d = 0
    while isinstance(x, list):
        d += 1
        x = x[0]
    return d
def matmul(*lst_prod):
    result = lst_prod[0]
    for index, x in enumerate(lst_prod[1:]):
        if not isinstance(result, list) or not isinstance(x, list):
            if isinstance(result, list):
                result = apply(result, lambda y: y * x)
            elif isinstance(x, list):
                result = apply(x, lambda y: result * y)
            else:
                result *= x
            continue
        
        # ranks
        r1 = depth(result)
        r2 = depth(x)

        # matrix @ matrix
        if r1 == 2 and r2 == 2:
            result = matmul_h(result, x)

        # batch matrix @ matrix
        elif r1 == 3 and r2 == 2:
            result = [matmul_h(A, x) for A in result]

        # matrix @ batch matrix
        elif r1 == 2 and r2 == 3:
            result = [matmul_h(result, B) for B in x]

        # batch matrix @ batch matrix
        elif r1 == 3 and r2 == 3:
            if len(result) == 1:
                result = [matmul_h(result[0], B) for B in x]
            elif len(x) == 1:
                result = [matmul_h(A, x[0]) for A in result]
            else:
                assert len(result) == len(x)
                result = [
                    matmul_h(A, B)
                    for A, B in zip(result, x)
                ]

        else:
            raise ValueError(
                f"Unsupported matmul ranks ({r1}, {r2})"
            )
        
    return result
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
    out = []

    def helper(x):
        if isinstance(x, list):
            for y in x:
                helper(y)
        else:
            out.append([x])   # column vector

    helper(A)
    return out
def diag(v):
    if len(v) > 0 and isinstance(v[0], list):
        v = [x[0] for x in v]
    n = len(v)
    return [[v[i] if i == j else 0 for j in range(n)] for i in range(n)]

def broadcast(M, rows):
    if not isinstance(M, list):
        raise ValueError("Cannot broadcast a scalar.")

    r = len(M)

    if r == rows:
        return M

    if r != 1:
        raise ValueError(f"Cannot broadcast first dimension {r} to {rows}")

    return [copy.deepcopy(M[0]) for _ in range(rows)]
def relu(arg):
    return apply(arg, lambda x: max(0, x))
def drelu(arg):
    return apply(arg, lambda x: 1 if x > 0 else 0)

def reshape(data, *shape):
    # Flatten (column-major if input is a 2D matrix)
    flat = []

    def flatten(x):
        if isinstance(x, list):
            for item in x:
                flatten(item)
        else:
            flat.append(x)

    flatten(data)

    # Check total size
    total = 1
    for s in shape:
        total *= s

    if len(flat) != total:
        raise ValueError("Cannot reshape: incompatible dimensions.")

    # Build recursively
    idx = 0

    def build(shape):
        nonlocal idx

        if len(shape) == 0:
            val = flat[idx]
            idx += 1
            return val

        return [build(shape[1:]) for _ in range(shape[0])]

    return build(shape)
def gen2(eq, w):
    def from_treenode(eq):
        nonlocal w
        
        alter = {"f_wadd":"matadd", "f_transpose":"transpose", "f_diag":"diag", "f_identity":"identity","f_wmul":"matmul", "f_mul":"hadamard",\
                 "f_kronecker":"kronecker", "f_commutation":"commutation","f_wmul":"matmul",\
                 "f_hadamard":"hadamard", "f_cap":"cap", "f_relu":"relu", "f_drelu":"drelu",\
                 "f_conv":"conv", "f_patches":"patches","f_toeplitz":"toeplitz", "f_flatten":"flatten", \
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
    def __init__(self, struct, rand_range=None):
        self.struct = struct
        self.var_list = [tree_form(f"v_-{i}") for i in range(1,26+1-4)]
        self.o = None
        self.lst_w = None
        self.gradient = None
        self.learn = None
        self.bc = 1
        self.loss = None
        self.model_type = None
        if rand_range is None:
            self.init_mat = lambda *args: zeros(*args)
        else:
            self.init_mat = lambda *args: randos(*(rand_range+list(args)))
    def model(self, t="image"):
        self.model_type = t
        if t == "image":
            return self.model_image()
        elif t == "sequence":
            return self.model_rnn_vanilla()
    def model_image(self):
        lst_z = []
        lst_w = []
        x = parse("X")
        y = parse("Y")
        lst_z.append(x)
        flattening_needed = False
        for i in range(1,len(self.struct)):
            if self.struct[i]["type"] == "convolution":
                flattening_needed = True
                lst_w.append(self.var_list.pop(0))
                eq = TreeNode("f_broadcast", [lst_w[-1], parse("m")])
                tmp = TreeNode("f_conv", [lst_z[-1], eq])
                tmp = replace(self.struct[i]["activation"], parse("Z"), tmp)
                lst_z[-1] = matrix_solve_ml(tmp)
            elif self.struct[i]["type"] == "dense":
                lst_w.append(self.var_list.pop(0))
                lst_w.append(self.var_list.pop(0))
                if flattening_needed:
                    flattening_needed = False
                    lst_z[-1] = lst_z[-1].fx("flatten")
                eq = TreeNode("f_wmul", [lst_z[-1], lst_w[-2]])
                eq = TreeNode("f_wadd", [eq, TreeNode("f_broadcast", [lst_w[-1], parse("m")])])
                lst_z.append(eq)
                tmp = replace(self.struct[i]["activation"], parse("Z"), lst_z[-1])
                lst_z[-1] = matrix_solve_ml(tmp)
        self.lst_w = lst_w
        self.o = lst_z[-1]
        eq = TreeNode("f_hadamard", [tree_form("d_-1"), y])
        eq = TreeNode("f_wadd", [self.o, eq])
        eq = eq.fx("vec")
        eq = TreeNode("f_wmul", [eq.fx("transpose"), eq])
        L = TreeNode("f_hadamard", [eq, (parse("m") * tree_form("d_2")) ** tree_form("d_-1")])
        self.loss = L
        gradient = []
        index = 0
        self.learn = []
        for i in range(1,len(self.struct)):
            if self.struct[i]["type"] == "dense":
                prev = None
                if self.struct[i-1]["type"] == "convolution":
                    a,b,c = self.struct[0]["dim"]
                    for j in range(1,i):
                        d,e,_ = self.struct[j]["dim"]
                        a,b = a-d + 1, b-e + 1
                    prev = a*b
                else:
                    prev = self.struct[i-1]["dim"][0]
                curr = self.struct[i]["dim"][0]
                self.learn.append(self.init_mat(prev, curr))
                self.learn.append(self.init_mat(1, curr))
            elif self.struct[i]["type"] == "convolution":
                self.learn.append(self.init_mat(*([1]+self.struct[i]["dim"])))
        for j in range(len(lst_w)):
            item = lst_w[j]
            tmp = TreeNode("f_pdif", [L, item.fx("vec")])
            tmp = matrix_solve_ml(diff_matrix_matrix(tmp))
            d = []
            k = self.learn[j]
            while isinstance(k, list):
                d.append(len(k))
                k = k[0]
            tmp = TreeNode("f_reshape", [tmp]+[tree_form(f"d_{item}") for item in d])
            eq = TreeNode("f_hadamard",[tree_form("d_-1"), parse("n"), tmp])
            eq = TreeNode("f_wadd", [item, eq])
            eq = matrix_solve_ml(eq)
            gradient.append(eq)
        self.gradient = gradient
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
            tmp = replace(parse("sigmoid(Z)"), parse("Z"), eq)
            lst_Hht.append(tmp)
            eq = TreeNode("f_wadd", [TreeNode("f_wmul", [lst_Hht[-1], Why]) , By])
            tmp = replace(parse("sigmoid(Z)"), parse("Z"), eq)
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
        global exp, hadamard, zeros, transpose, matadd, tanh, sigmoid, identity, power, vec, diag, flatten,\
               matmul, reshape, kronecker, commutation, broadcast, drelu, relu, patches, conv, toeplitz
        env = {
            "w": self.learn,
            "m": 1,
            "identity":identity,
            "X": [given_x] if self.model_type == "image" else transpose([transpose(given_x)]),
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
            "broadcast":broadcast,
            "relu":relu,
            "drelu":drelu,
            "conv":conv,
            "patches":patches,
            "topelitz": toeplitz,
            "flatten":flatten
        }
        if self.model_type == "image":
            return eval(gen2(self.o, self.lst_w), {}, env)[0]
        else:
            return transpose([eval(gen2(item, self.lst_w), {}, env)[0] for item in self.o])
    def train(self, train_x, train_y, learning_rate, epoch, batch_size=1):
        global exp, hadamard, zeros, transpose, matadd, tanh, sigmoid, identity, power, vec, diag, flatten,\
               matmul, reshape, kronecker, commutation, broadcast, drelu, relu, patches, conv, toeplitz
        self.bc = batch_size
        if self.model_type != "image":
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
            "broadcast":broadcast,
            "relu":relu,
            "drelu":drelu,
            "conv":conv,
            "patches":patches,
            "toeplitz":toeplitz,
            "flatten":flatten
        }
        for j in range(len(self.lst_w)):
            tmp = f"fx_{j} = lambda X,Y,w,m: "+gen2(self.gradient[j], self.lst_w)
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
        if self.model_type == "image":
            data_x_batch = make_batches(copy.deepcopy(train_x), batch_size)
            data_y_batch = make_batches(copy.deepcopy(train_y), batch_size)
            index_count = len(data_x_batch)
        else:
            index_count = len(train_x)
        for k in range(epoch):            
            for i in range(index_count):
                learn_new = copy.deepcopy(self.learn)
                if self.model_type != "image":
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
