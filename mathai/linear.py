from .diff import diff
from .simplify import simplify, solve
from .fraction import fraction
from .expand import expand
from .base import *
def rref(matrix):
    rows, cols = len(matrix), len(matrix[0])
    lead = 0
    for r in range(rows):
        if lead >= cols:
            return matrix
        i = r
        while fraction(simplify(matrix[i][lead])) == tree_form("d_0"):
            i += 1
            if i == rows:
                i = r
                lead += 1
                if lead == cols:
                    return matrix
        matrix[i], matrix[r] = matrix[r], matrix[i]
        lv = matrix[r][lead]
        matrix[r] = [fraction(simplify(m / lv)) for m in matrix[r]]
        for i in range(rows):
            if i != r:
                lv = matrix[i][lead]
                matrix[i] = [fraction(simplify(m - lv * n)) for m, n in zip(matrix[i], matrix[r])]
        lead += 1
    return matrix
def islinear(eq, fxconst):
    eq =simplify(eq)
    if eq.name == "f_pow" and fxconst(eq):#"v_" in str_form(eq):
        return False
    for child in eq.children:
        out = islinear(child, fxconst)
        if not out:
            return out
    return True
def linear(eqlist, fxconst):
    final = []
    extra = []
    for i in range(len(eqlist)-1,-1,-1):
        if eqlist[i].name == "f_mul" and not islinear(expand2(eqlist[i]), fxconst):
            if "v_" in str_form(eqlist[i]):
                eqlist[i] = TreeNode("f_mul", [child for child in eqlist[i].children if fxconst(child)])
            if all(islinear(child, fxconst) for child in eqlist[i].children):
                for child in eqlist[i].children:
                    extra.append(TreeNode("f_eq", [child, tree_form("d_0")]))
                eqlist.pop(i)
            else:
                final.append(TreeNode("f_eq", [eqlist[i], tree_form("d_0")]))
                eqlist.pop(i)
    
    if extra != []:
        final.append(TreeNode("f_or", extra))
    if eqlist == []:
        if len(final)==1:
            
            return final[0]
        return TreeNode("f_and", final)
    eqlist = [eq for eq in eqlist if fxconst(eq)]
    if not all(islinear(eq, fxconst) for eq in eqlist):
        return TreeNode("f_and", copy.deepcopy(final+eqlist))
    vl = []
    def varlist(eq, fxconst):
        nonlocal vl
        if eq.name[:2] == "v_" and fxconst(eq):
            vl.append(eq.name)
        for child in eq.children:
            varlist(child, fxconst)
    for eq in eqlist:
        varlist(eq, fxconst)
    vl = list(set(vl))
    if len(vl) > len(eqlist):
        return TreeNode("f_and", final+[TreeNode("f_eq", [x, tree_form("d_0")]) for x in eqlist])
    m = []
    for eq in eqlist:
        s = copy.deepcopy(eq)
        row = []
        for v in vl:
            row.append(diff(eq, v))
            s = replace(s, tree_form(v), tree_form("d_0"))
        row.append(s)
        m.append(row)
    for i in range(len(m)):
        for j in range(len(m[i])):
            m[i][j] = simplify(expand(m[i][j]))
    #print(m)
    m = rref(m)
    
    for i in range(len(m)):
        for j in range(len(m[i])):
            m[i][j] = fraction(m[i][j])
    #print(m)
    for item in m:
        if all(item2==tree_form("d_0") for item2 in item[:-1]) and item[-1] != tree_form("d_0"):
            return tree_form("d_false")
    
    output = []
    for index, row in enumerate(m):
        count = 0
        for item in row[:-1]:
            if item == tree_form("d_1"):
                count += 1
                if count == 2:
                    break
            elif item == tree_form("d_0") and count == 1:
                break
        if count == 0:
            continue
        output.append(tree_form(vl[index])+row[-1])
    if len(output) == 1 and len(final)==0:
        return TreeNode("f_eq", [output[0], tree_form("d_0")])
    return TreeNode("f_and", final+[TreeNode("f_eq", [x, tree_form("d_0")]) for x in output])

def rmeq(eq):
    if eq.name == "f_eq":
        return rmeq(eq.children[0])
    return TreeNode(eq.name, [rmeq(child) for child in eq.children])

def mat0(eq, lst=None):
    def findeq(eq):
        out = []
        if "f_list" not in str_form(eq) and "f_eq" not in str_form(eq):
            return [str_form(eq)]
        else:
            for child in eq.children:
                out += findeq(child)
        return out
    eqlist = findeq(eq)
    eqlist = [tree_form(x) for x in eqlist]
    eqlist = [rmeq(x) for x in eqlist]
    eqlist = [TreeNode("f_mul", factor_generation(x)) for x in eqlist if x != tree_form("d_0")]
    eqlist = [x.children[0] if len(x.children) == 1 else x for x in eqlist]
    out = None
    
    if lst is None:
        out = linear(copy.deepcopy(eqlist), lambda x: "v_" in str_form(x))
    else:
        out = linear(copy.deepcopy(eqlist), lambda x: any(contain(x, item) for item in lst))
    def rms(eq):
        if eq.name in ["f_and", "f_or"] and len(eq.children) == 1:
            return eq.children[0]
        return TreeNode(eq.name, [rms(child) for child in eq.children])
    return rms(out)
def linear_solve(eq, lst=None):
    if eq.name == "f_and":
        eq2 = copy.deepcopy(eq)
        eq2.name = "f_list"
        return mat0(eq2, lst)
    elif eq.name == "f_eq":
        return mat0(eq, lst)
    return TreeNode(eq.name, [linear_solve(child, lst) for child in eq.children])
