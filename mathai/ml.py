import itertools
from .parser import parse, remove_extra_brackets
from .simplify import simplify, addition_node_mat
from .structure import make_formula_function
from .base import *
import random
import copy
import math
from .cl_math import CLMath

cl_math = CLMath()

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

def gen2(eq, w):
    def from_treenode(eq):
        nonlocal w

        # Assuming you have a module or helper class 'cl_math' that wraps your OpenCL operations
        alter = {
            "f_index": "cl_math.index", # index(arr,n) access nth element of arr
            
            # Element-wise operations
            "f_wadd": "cl_math.matadd",         # Custom parallel addition
            "f_hadamard": "cl_math.hadamard",     # Element-wise multiplication
            "f_sigmoid": "cl_math.sigmoid",       # 1 / (1 + exp(-x))
            "f_exp": "cl_math.exp",               # Element-wise exponential
            "f_wpow": "cl_math.pow",             # Element-wise power
            
            # Structural & shape manipulations (these often just change metadata/strides)
            "f_transpose": "cl_math.transpose",   # Transpose dimension axes
            "f_diag": "cl_math.diag",             # Extract/create diagonal
            "f_identity": "cl_math.identity",     # Identity matrix generation
            "f_flatten": "cl_math.flatten",       # Flatten array to 1D
            "f_reshape": "cl_math.reshape",       # Reshape without copying memory
            "f_broadcast": "cl_math.broadcast",   # Replicate dimensions
            
            # Linear Algebra
            "f_wmul": "cl_math.matmul",           # Parallel matrix multiplication (your GPU kernel)
            "f_kronecker": "cl_math.kronecker",   # Kronecker tensor product
            "f_commutation": "cl_math.commutation", # Commutation matrix
            "f_vec": "cl_math.vec",               # Vectorization operation
            
            # Deep Learning Specific operations
            "f_conv": "cl_math.conv",             # Convolutional operations
            "f_im2col": "cl_math.im2col",         # Image-to-column layout conversion
            "f_col2im": "cl_math.col2im",         # Column-to-image layout conversion
            
            # Helpers
            "f_zeros": "cl_math.zeros",           # Zero-filled GPU buffers
            "f_len": "cl_math.len"                # Check dimension size
        }
        
        if eq.name in alter.keys():
            
            return alter[eq.name]+"("+",".join([from_treenode(child) for child in eq.children])+")"

        alter2 = {"f_add":"+", "f_pow":"**", "f_mul":"*"}
        if eq.name in alter2.keys():
            return "("+alter2[eq.name].join([from_treenode(child) for child in eq.children])+")"
        
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
            self.init_mat = lambda *args: cl_math.zeros(*args)
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
            if isinstance(k, cl_math.type_arr):
                d = list(k.shape)
            else:
                d = []
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
        global transpose, cl_math
        env = {
            "w": self.learn,
            "m": 1,
            "X": cl_math.wrap(given_x) if self.model_type == "image" else transpose([transpose(given_x)]),
            "cl_math": cl_math
        }
        if self.model_type == "image":
            return eval(gen2(self.o, self.lst_w), {}, env)[0]
        else:
            return transpose([eval(gen2(item, self.lst_w), {}, env)[0] for item in self.o])
    def train(self, train_x, train_y, learning_rate, epoch, batch_size=1):
        global cl_math
        self.bc = batch_size
        if self.model_type != "image":
            train_x = [transpose(item) for item in train_x]
            train_y = [transpose(item) for item in train_y]
        
        env = {
            "n": learning_rate,
            "cl_math": cl_math
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
            data_x_batch = make_batches(train_x.copy(), batch_size)
            data_y_batch = make_batches(train_y.copy(), batch_size)
            index_count = len(data_x_batch)
        else:
            index_count = len(train_x)
        for k in range(epoch):            
            for i in range(index_count):
                learn_new = self.learn.copy()
                if self.model_type != "image":
                    data_x = transpose([train_x[i]])
                    data_y = transpose([train_y[i]])
                else:
                    data_x = data_x_batch[i]
                    m = len(data_x)
                    data_y = data_y_batch[i]
                for j in range(len(self.lst_w)):
                    learn_new[j] = env[f"fx_{j}"](cl_math.to_gpu(data_x), cl_math.to_gpu(data_y), self.learn, m)
                self.learn = learn_new.copy()
            if k % round(epoch/10.0) == 0:
                print(f"epoches done {k+1}/{epoch}")
        print("training done.")
        print()
