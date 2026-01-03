from .inverse import inverse
import itertools
from .diff import diff
from .simplify import simplify
from .fraction import fraction
from .expand import expand
from .base import *
from .factor import factorconst
from .tool import poly
def ss(eq):
    return dowhile(eq, lambda x: fraction(expand(simplify(x))))
def rref(matrix):
    rows, cols = len(matrix), len(matrix[0])
    lead = 0
    for r in range(rows):
        if lead >= cols:
            return matrix
        i = r
        while ss(matrix[i][lead]) == tree_form("d_0"):
            i += 1
            if i == rows:
                i = r
                lead += 1
                if lead == cols:
                    return matrix
        matrix[i], matrix[r] = matrix[r], matrix[i]
        lv = matrix[r][lead]
        matrix[r] = [ss(m / lv) for m in matrix[r]]
        for i in range(rows):
            if i != r:
                lv = matrix[i][lead]
                matrix[i] = [ss(m - lv * n) for m, n in zip(matrix[i], matrix[r])]
        lead += 1
    return matrix
def islinear(eq, fxconst):
    eq =simplify(eq)
    if all(fxconst(tree_form(item)) and poly(eq, item) is not None and len(poly(eq, item)) <= 2 for item in vlist(eq)):
        return True
    return False
def linear(eqlist, fxconst):
    orig = [item.copy_tree() for item in eqlist]
    #eqlist = [eq for eq in eqlist if fxconst(eq)]
    
    if eqlist == [] or not all(islinear(eq, fxconst) for eq in eqlist):
        return None
        #return TreeNode("f_and", [TreeNode("f_eq", [x, tree_form("d_0")]) for x in orig])
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
        return TreeNode("f_and", [TreeNode("f_eq", [x, tree_form("d_0")]) for x in eqlist])
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

    m = rref(m)
    
    for i in range(len(m)):
        for j in range(len(m[i])):
            m[i][j] = fraction(m[i][j])
    
    output = []
    for index, row in enumerate(m):
        if not all(item == 0 for item in row[:-1]):
            output.append(summation([tree_form(vl[index2])*coeff for index2, coeff in enumerate(row[:-1])])+row[-1])
        elif row[-1] != 0:
            return tree_form("s_false")
    if len(output) == 1:
        return TreeNode("f_eq", [output[0], tree_form("d_0")])
    if len(output) == 0:
        return tree_form("s_false")
    return TreeNode("f_and", [TreeNode("f_eq", [x, tree_form("d_0")]) for x in output])
def order_collinear_indices(points, idx):
    """
    Arrange a subset of collinear points (given by indices) along their line.
    
    points: list of (x, y) tuples
    idx: list of indices referring to points
    Returns: list of indices sorted along the line
    """
    if len(idx) <= 1:
        return idx[:]
    
    # Take first two points from the subset to define the line
    p0, p1 = points[idx[0]], points[idx[1]]
    dx, dy = p1[0] - p0[0], p1[1] - p0[1]
    
    # Projection factor for sorting
    def projection_factor(i):
        vx, vy = points[i][0] - p0[0], points[i][1] - p0[1]
        return compute((vx * dx + vy * dy) / (dx**2 + dy**2))
    
    # Sort indices by projection
    sorted_idx = sorted(idx, key=projection_factor)
    return list(sorted_idx)
def linear_or(eq):
    eqlst =[]
    if eq.name != "f_or":
        eqlst = [eq]
    else:
        eqlst = eq.children
    v = vlist(eq)
    p = []
    line = {}
    for i in range(len(eqlst)):
        line[i] = []
    for item in itertools.combinations(enumerate(eqlst), 2):
        x, y = item[0][0],  item[1][0]
        item = [item[0][1], item[1][1]]
        
        out = linear_solve(TreeNode("f_and", list(item)))

        if out is None:
            return None
        
        if out.name == "f_and" and all(len(vlist(child)) == 1 for child in out.children) and set(vlist(out)) == set(v) and all(len(vlist(simplify(child))) >0 for child in out.children):
            t = {}
            for child in out.children:
                t[v.index(vlist(child)[0])] = simplify(inverse(child.children[0], vlist(child)[0]))
            t2 = []
            for key in sorted(t.keys()):
                t2.append(t[key])
            t2 = tuple(t2)
            if t2 not in p:
                p.append(t2)
            line[x] += [p.index(t2)]
            line[y] += [p.index(t2)]
    line2 = []
    for key in sorted(line.keys()):
        line2.append(order_collinear_indices(p, list(set(line[key]))))

    return v, p, line2, eqlst
def linear_solve(eq, lst=None):
    eq = simplify(eq)
    eqlist = []
    if eq.name =="f_and" and all(child.name == "f_eq" and child.children[1] == 0 for child in eq.children):
        
        eqlist = [child.children[0] for child in eq.children]
    else:
        return eq
    out = None
    if lst is None:
        out = linear(copy.deepcopy(eqlist), lambda x: "v_" in str_form(x))
    else:
        out = linear(copy.deepcopy(eqlist), lambda x: any(contain(x, item) for item in lst))
    if out is None:
        return None
    return simplify(out)
