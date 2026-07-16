import itertools
from .parser import parse, remove_extra_brackets
from .simplify import simplify, addition_node_mat
from .structure import make_formula_function
from .base import *
import random
import copy
import math

def diffmat_formula_init():
    formula_list = [

        # ---------------- Elementary ----------------
        ("wmul(identity(a),b)", "b"),
        ("wmul(b,identity(a))", "b"),
        (
            "pdif(hadamard(k,a),x)",
            "hadamard(k,pdif(a,x))"
        ),

        (
            "pdif(wadd(a,b),x)",
            "wadd(pdif(a,x),pdif(b,x))"
        ),

        (
            "pdif(vec(wadd(a,b)),vec(x))",
            "wadd(pdif(vec(a),vec(x)),pdif(vec(b),vec(x)))"
        ),

        (
            "transpose(transpose(x))",
            "x"
        ),

        (
            "vec(vec(x))",
            "vec(x)"
        ),
        (
            "pdif(vec(flatten(a)),vec(x))",
            "pdif(vec(a),vec(x))"
        ),
        (
            "pdif(vec(x),vec(x))",
            "identity(len(x)*len(index(x,0)))"
        ),

        # ---------------- Transpose ----------------

        (
            "pdif(vec(transpose(a)),vec(x))",
            "wmul(commutation(len(a),len(index(a,0))),pdif(vec(a),vec(x)))"
        ),

        # ---------------- Hadamard ----------------

        (
            "pdif(vec(hadamard(a,b)),vec(x))",
            "wadd("
            "wmul(diag(b),pdif(vec(a),vec(x))),"
            "wmul(diag(a),pdif(vec(b),vec(x)))"
            ")"
        ),

        # ---------------- Broadcast ----------------

        (
            "pdif(vec(broadcast(a,r)),vec(x))",
            "wmul(kronecker(identity(len(vec(a))),wadd(1,zeros(r,1))),pdif(vec(a),vec(x)))"
        ),

        # ---------------- Sigmoid ----------------

        (
            "pdif(vec(sigmoid(a)),vec(x))",
            "wmul("
            "diag(vec("
            "hadamard("
            "sigmoid(a),"
            "wadd(1,hadamard(-1,sigmoid(a)))"
            ")"
            ")),"
            "pdif(vec(a),vec(x))"
            ")"
        ),
        # ---------------- Matrix multiplication ----------------

        (
            "pdif(vec(wmul(a,b)),vec(x))",
            "wadd("
            "wmul(kronecker(transpose(b),identity(len(a))),pdif(vec(a),vec(x))),"
            "wmul(kronecker(identity(len(index(b,0))),a),pdif(vec(b),vec(x)))"
            ")"
        ),

        # ---------------- Quadratic form ----------------

        (
            "pdif(wmul(transpose(y),y),vec(x))",
            "wmul(transpose(pdif(y,vec(x))),hadamard(2,y))"
        ),
        (
            "pdif(vec(conv(a,b)),vec(x))",
            "wadd("
                "wmul(im2col(a,b), pdif(vec(b),vec(x))),"
                "wmul(col2im(b,a), pdif(vec(a),vec(x)))"
            ")"
        )
    ]
    formula_list = [[simplify(parse(x[0])), simplify(parse(x[1])), [], None, [], [parse("k").name], {}, [parse("m").name]] for x in formula_list]
    return make_formula_function(formula_list)
helpermat_fx = diffmat_formula_init()
def helper(eq):
    global helpermat_fx
    eq = simplify(eq)
    if eq.name in ["f_pdif"] and not contain(eq.children[0],eq.children[1].children[0]):
        return tree_form("d_0")
    if eq.name in ["f_pdif"] and "v_" not in str_form(eq.children[0]):
        return tree_form("d_0")
    out = helpermat_fx(eq)
    if out is None:
        return eq
    return out
def diff_matrix_matrix(eq):
    if eq is None:
        return None
    return dowhile(eq, lambda x: transform_dfs(x, lambda y: dowhile(y, helper)))
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
    B = len(A)
    H = len(A[0])
    W = len(A[0][0])
    C = len(A[0][0][0])

    out = []

    for b in range(B):
        row = []
        for i in range(H):
            for j in range(W):
                for c in range(C):
                    row.append(A[b][i][j][c])
        out.append(row)

    return out
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
def im2col(image, kernel):
    B = len(image)
    H = len(image[0])
    W = len(image[0][0])
    C = len(image[0][0][0])

    Kh = len(kernel[0])
    Kw = len(kernel[0][0])

    out_h = H - Kh + 1
    out_w = W - Kw + 1

    P = Kh * Kw * C

    cols = []

    for b in range(B):
        for i in range(out_h):
            for j in range(out_w):

                row = [0] * (B * P)

                idx = b * P

                for r in range(Kh):
                    for c in range(Kw):
                        for ch in range(C):
                            row[idx] = image[b][i+r][j+c][ch]
                            idx += 1

                cols.append(row)

    return cols
def col2im(kernel, image):
    B = len(image)
    H = len(image[0])
    W = len(image[0][0])
    C = len(image[0][0][0])

    K = len(kernel)          # 1 or B
    Kh = len(kernel[0])
    Kw = len(kernel[0][0])

    assert K == 1 or K == B

    OH = H - Kh + 1
    OW = W - Kw + 1

    rows = B * OH * OW
    cols = B * H * W * C

    J = [[0 for _ in range(cols)] for _ in range(rows)]

    row = 0

    for b in range(B):
        kb = 0 if K == 1 else b

        for i in range(OH):
            for j in range(OW):

                for u in range(Kh):
                    for v in range(Kw):
                        for ch in range(C):

                            col = (
                                (((b * H + (i + u)) * W + (j + v)) * C)
                                + ch
                            )

                            J[row][col] = kernel[kb][u][v][ch]

                row += 1

    return J
def commutation(m, n):
    size = m * n
    K = [[0] * size for _ in range(size)]
    
    for i in range(m):
        for j in range(n):
            # 1. Where X[i][j] lives in column-major vec(X)
            src_idx = j * m + i
            
            # 2. Where X[i][j] lives in column-major vec(X^T)
            # (Since X^T has 'n' rows, the item is at row j, col i of X^T)
            dest_idx = i * n + j
            
            # 3. Direct the row of the matrix multiplication to pull from the source
            K[dest_idx][src_idx] = 1
            
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
            print(len(lst_prod), result, x)
            raise ValueError(
                f"Unsupported matmul ranks ({r1}, {r2})"
            )
        
    return result
def kronecker_h(A, B):
    m = len(A)     # Rows of A
    n = len(A[0])  # Columns of A
    p = len(B)     # Rows of B
    q = len(B[0])  # Columns of B
    
    # Initialize the structural grid: (rows x columns)
    result = [[0] * (n * q) for _ in range(m * p)]
    
    # COLUMN-MAJOR TRAVERSAL: Outer loops handle columns, inner loops handle rows
    for j in range(n):          # 1. Choose column of A
        for c in range(q):      # 2. Choose column of B
            # At this stage, our target column is locked
            col_target = j * q + c
            
            for i in range(m):  # 3. Sweep down rows of A
                for r in range(p): # 4. Sweep down rows of B
                    row_target = i * p + r
                    
                    # Fill the matrix strictly down the current column
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

    def visit(x):
        if isinstance(x, list):
            for y in x:
                visit(y)
        else:
            out.append([x])

    visit(A)
    return out
def diag(v):
    # Column vector
    if len(v) > 0 and isinstance(v[0], list):
        if len(v[0]) != 1:
            raise ValueError("diag expects a column vector.")
        v = [x[0] for x in v]

    # Reject nested tensors
    for x in v:
        if isinstance(x, list):
            raise ValueError("diag received a tensor instead of a vector.")

    n = len(v)

    result = [[0] * n for _ in range(n)]

    for i in range(n):
        result[i][i] = v[i]

    return result
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
    # --- Helper 1: Calculate structural dimensions of the input nested list ---
    def get_shape(x):
        if not isinstance(x, list):
            return []
        return [len(x)] + get_shape(x[0])
    
    in_shape = get_shape(data)
    
    # --- Helper 2: Index-based element retrieval for arbitrary dimensions ---
    def get_element(x, indices):
        for idx in indices:
            x = x[idx]
        return x

    # Step 1: Flatten in column-major order (deepest dimensions vary slowest)
    flat = []
    if in_shape:
        # Generate indices in column-major order (right-to-left counter increment)
        num_elements = 1
        for s in in_shape:
            num_elements *= s
            
        for i in range(num_elements):
            # Decompose linear index to column-major matrix subscripts
            indices = [0] * len(in_shape)
            rem = i
            for d in reversed(range(len(in_shape))):
                indices[d] = rem % in_shape[d]
                rem //= in_shape[d]
            flat.append(get_element(data, indices))
    else:
        flat = [data]

    # Validate size compatibility
    total = 1
    for s in shape:
        total *= s
    if len(flat) != total:
        raise ValueError(f"Cannot reshape: incompatible dimensions. Total size {len(flat)} vs {total}")

    # Step 2: Reconstruct the target tensor in column-major order
    # Initialize an empty nested list structure matching the target shape
    def make_empty(shp):
        if len(shp) == 1:
            return [None] * shp[0]
        return [make_empty(shp[1:]) for _ in range(shp[0])]
    
    out_structure = make_empty(shape)

    # Helper to write value via multi-dimensional index
    def set_element(x, indices, val):
        for idx in indices[:-1]:
            x = x[idx]
        x[indices[-1]] = val

    # Fill the structure using column-major index ordering
    for i in range(total):
        indices = [0] * len(shape)
        rem = i
        for d in reversed(range(len(shape))):
            indices[d] = rem % shape[d]
            rem //= shape[d]
        set_element(out_structure, indices, flat[i])

    return out_structure
def gen2(eq, w):
    def from_treenode(eq):
        nonlocal w
        
        alter = {"f_wadd":"matadd", "f_transpose":"transpose", "f_diag":"diag", "f_identity":"identity","f_wmul":"matmul", "f_mul":"hadamard",\
                 "f_kronecker":"kronecker", "f_commutation":"commutation","f_wmul":"matmul",\
                 "f_hadamard":"hadamard", "f_cap":"cap", "f_relu":"relu", "f_drelu":"drelu",\
                 "f_conv":"conv", "f_im2col":"im2col","f_col2im":"col2im", "f_flatten":"flatten", \
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
                lst_z[-1] = tmp
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
                lst_z[-1] = tmp
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
            tmp = diff_matrix_matrix(tmp)
            d = []
            k = self.learn[j]
            while isinstance(k, list):
                d.append(len(k))
                k = k[0]
            tmp = TreeNode("f_reshape", [tmp]+[tree_form(f"d_{item}") for item in d])
            eq = TreeNode("f_hadamard",[tree_form("d_-1"), parse("n"), tmp])
            eq = TreeNode("f_wadd", [item, eq])
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
        gradient = []
        # x, y, h, t
        for i in range(2):
            for j in range(3):
                item = [[Wxh, Whh, Why], [By, Bh, Hht0]][i][j]
                tmp = TreeNode("f_pdif", [L, item.fx("vec")])
                tmp = diff_matrix_matrix(tmp)
                tmp = TreeNode("f_reshape", [tmp, item.fx("len"), TreeNode("f_index", [item, tree_form("d_0")]).fx("len")])
                eq = TreeNode("f_hadamard", [tree_form("d_-1"), parse("n"),tmp])
                eq = TreeNode("f_wadd", [item, eq])
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
               matmul, reshape, kronecker, commutation, broadcast, drelu, relu, im2col, conv, col2im
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
            "im2col":im2col,
            "col2im": col2im,
            "flatten":flatten
        }
        if self.model_type == "image":
            return eval(gen2(self.o, self.lst_w), {}, env)[0]
        else:
            return transpose([eval(gen2(item, self.lst_w), {}, env)[0] for item in self.o])
    def train(self, train_x, train_y, learning_rate, epoch, batch_size=1):
        global exp, hadamard, zeros, transpose, matadd, tanh, sigmoid, identity, power, vec, diag, flatten,\
               matmul, reshape, kronecker, commutation, broadcast, drelu, relu, im2col, conv, col2im
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
            "im2col":im2col,
            "col2im":col2im,
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
