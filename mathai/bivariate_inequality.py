import math
from .linear import linear_or
from functools import reduce
import operator
from .base import *
from .simplify import simplify
from .expand import expand
from .logic import logic0

def shoelace_area(vertices):
    n = len(vertices)
    area = 0.0
    for i in range(n):
        j = (i + 1) % n
        area += vertices[i][0] * vertices[j][1]
        area -= vertices[j][0] * vertices[i][1]
    return abs(area) / 2.0

def triangle_area(p1, p2, p3):
    area = 0.0
    area += p1[0] * (p2[1] - p3[1])
    area += p2[0] * (p3[1] - p1[1])
    area += p3[0] * (p1[1] - p2[1])
    return abs(area) / 2.0

def is_point_inside_polygon(point, vertices):
    if len(vertices) < 3:
        return False

    polygon_area = shoelace_area(vertices)
    
    total_triangle_area = 0.0
    n = len(vertices)
    for i in range(n):
        j = (i + 1) % n
        total_triangle_area += triangle_area(point, vertices[i], vertices[j])
    
    tolerance = 1e-5
    return abs(total_triangle_area - polygon_area) < tolerance

def distance_point_to_segment(px, py, x1, y1, x2, y2):
    dx, dy = x2 - x1, y2 - y1
    if dx == dy == 0:
        return ((px - x1)**2 + (py - y1)**2)**0.5
    t = max(0, min(1, ((px - x1) * dx + (py - y1) * dy) / (dx*dx + dy*dy)))
    proj_x = x1 + t * dx
    proj_y = y1 + t * dy
    return ((px - proj_x)**2 + (py - proj_y)**2)**0.5

def deterministic_middle_point(vertices, grid_resolution=100):
    xs = [v[0] for v in vertices]
    ys = [v[1] for v in vertices]
    xmin, xmax = min(xs), max(xs)
    ymin, ymax = min(ys), max(ys)
    
    best_point = None
    max_dist = -1
    
    for i in range(grid_resolution + 1):
        for j in range(grid_resolution + 1):
            px = xmin + (xmax - xmin) * i / grid_resolution
            py = ymin + (ymax - ymin) * j / grid_resolution
            if not is_point_inside_polygon((px, py), vertices):
                continue
            min_edge_dist = float('inf')
            n = len(vertices)
            for k in range(n):
                x1, y1 = vertices[k]
                x2, y2 = vertices[(k + 1) % n]
                d = distance_point_to_segment(px, py, x1, y1, x2, y2)
                min_edge_dist = min(min_edge_dist, d)
            if min_edge_dist > max_dist:
                max_dist = min_edge_dist
                best_point = (px, py)
    
    return best_point

def build(eq):
    eq = TreeNode("f_or", eq)
    eq = flatten_tree(eq)
    orig = eq.copy_tree()
    def fxhelper3(eq):
        if eq.name[2:] in "le ge lt gt".split(" "):
            return TreeNode("f_eq", [child.copy_tree() for child in eq.children])
        return TreeNode(eq.name, [fxhelper3(child) for child in eq.children])
    eq = fxhelper3(eq)

    result = linear_or(eq)
    
    if result is None:
        return None
    
    maxnum = tree_form("d_2")
    if len(result[1]) != 0:
        maxnum = max([max([simplify(item2.fx("abs")) for item2 in item], key=lambda x: compute(x)) for item in result[1]], key=lambda x: compute(x))
        maxnum += 1
        maxnum = simplify(maxnum)
    eq = flatten_tree(eq | simplify(TreeNode("f_or", [TreeNode("f_eq", [tree_form(item)+maxnum, tree_form("d_0")])|\
                                                      TreeNode("f_eq", [tree_form(item)-maxnum, tree_form("d_0")]) for item in ["v_0","v_1"]])))
    result2 = linear_or(eq)
    if result2 is None:
        return None
    
    point_lst = result2[2]

    def gen(point):
        nonlocal point_lst
        out = []
        for item in point_lst:
            p = None
            if point in item:
                p = item.index(point)
            else:
                continue
            if p < len(item)-1:
                out.append(item[p+1])
            if p > 0:
                out.append(item[p-1])
        return list(set(out))
    start = list(range(len(result2[1])))
    graph= {}
    for item in start:
        graph[item] = gen(item)

    points = {}
    for index, item in enumerate(result2[1]):
        points[index] = [compute(item2) for item2 in item]

    res = []
    for index, item in enumerate(result2[1]):
        if any(simplify(item2.fx("abs")-maxnum)!=0 and abs(compute(item2))>compute(maxnum) for item2 in item):
            res.append(index)
            
    graph = {k: sorted(v) for k, v in graph.items()}

    def dfs(current, parent, path, visited, cycles):
        path.append(current)
        visited.add(current)
        for neighbor in graph[current]:
            if neighbor == parent:
                continue
            if neighbor in visited:
                idx = path.index(neighbor)
                cycle = path[idx:]
                cycles.append(cycle)
            else:
                dfs(neighbor, current, path, visited, cycles)
        path.pop()
        visited.remove(current)

    cycles = []
    for start in sorted(graph.keys()):
        path = []
        visited = set()
        dfs(start, -1, path, visited, cycles)

    def normalize(cycle):
        k = len(cycle)
        if k < 3:
            return None
        candidates = []
        for direction in [cycle, list(reversed(cycle))]:
            doubled = direction + direction[:-1]
            for i in range(k):
                rot = tuple(doubled[i:i + k])
                candidates.append(rot)
        return min(candidates)

    unique = set()
    for c in cycles:
        norm = normalize(c)
        if norm:
            unique.add(norm)

    cycles = sorted(list(unique), key=lambda x: (len(x), x))

    start = list(range(len(result2[1])))
    for i in range(len(cycles)-1,-1,-1):
        if any(item in cycles[i] for item in res) or\
           any(is_point_inside_polygon([compute(item2) for item2 in list(result2[1][p])], [[compute(item2) for item2 in result2[1][item]] for item in cycles[i]]) for p in list(set(start) - set(cycles[i]))) or\
           any(len(set(graph[item]) & set(cycles[i]))>2 for item in cycles[i]):
            cycles.pop(i)
            
    point_lst = [index for index, item in enumerate(result2[1]) if item in result[1]]

    border = []
    for item in start:
        for item2 in graph[item]:
            a = result2[1][item]
            b = result2[1][item2]
            
            if a[0] == b[0] and simplify(a[0].fx("abs") - maxnum) == 0:
                continue
            if a[1] == b[1] and simplify(a[1].fx("abs") - maxnum) == 0:
                continue
            
            border.append(tuple(sorted([item, item2])))

    line = []
    for key in graph.keys():
        for item in list(set(point_lst)&set(graph[key])):
            line.append(tuple(sorted([item, key])))
    line = list(set(line+border))
    point_in = [deterministic_middle_point([[compute(item3) for item3 in result2[1][item2]] for item2 in item]) for item in cycles]
    def work(eq, point):
        nonlocal result2
        if eq.name[:2] == "d_":
            return float(eq.name[2:])
        if eq.name in result2[0]:
            return point[result2[0].index(eq.name)]
        if eq.name == "f_add":
            return sum(work(item, point) for item in eq.children)
        if eq.name == "f_mul":
            return math.prod(work(item, point) for item in eq.children)
        if eq.name == "f_sub":
            return work(eq.children[0], point) - work(eq.children[1], point)
        return {"eq": lambda a,b: abs(a-b)<0.001, "gt":lambda a,b: False if abs(a-b)<0.001 else a>b, "lt":lambda a,b: False if abs(a-b)<0.001 else a<b}[eq.name[2:]](work(eq.children[0], point), work(eq.children[1], point))

    data = []
    for index, item in enumerate(result2[2][:-4]):
        a = tuple([item for item in point_lst if work(orig.children[index], [compute(item2) for item2 in result2[1][item]])])
        #a = tuple(set(item) & set(point_lst))
        #b = tuple(set([tuple(sorted([item[i], item[i+1]])) for i in range(len(item)-1)]) & set(line))
        b = None
        if orig.children[index] == "f_eq":
            b = tuple([tuple(item) for item in line if work(orig.children[index], [compute(item2) for item2 in result2[1][item[1]]]) and work(orig.children[index], [compute(item2) for item2 in result2[1][item[0]]])])
        else:
            b = tuple([tuple(item) for item in line if work(orig.children[index], [compute(item2) for item2 in result2[1][item[1]]]) or work(orig.children[index], [compute(item2) for item2 in result2[1][item[0]]])])
        c = tuple([tuple(item) for index2, item in enumerate(cycles) if work(orig.children[index], point_in[index2])])
        data.append((a,b,c))
        
    total = tuple([tuple(point_lst), tuple(line), tuple(cycles)])
    final = {}
    for index, item in enumerate(orig.children):
        final[item] = tuple(data[index])
    return final, total, result2[1]

def inequality_solve(eq):
    
    eq = logic0(eq)
    element = []
    def helper(eq):
        nonlocal element
        
        if eq.name[2:] in "le ge lt gt eq".split(" ") and "v_" in str_form(eq):
            element.append(eq)
        return TreeNode(eq.name, [helper(child) for child in eq.children])
    helper(eq)
    
    out = build(list(set(element)))

    if out is None:
        return eq
    
    def helper2(eq):
        nonlocal out
        if eq == tree_form("s_true"):
            return [set(item) for item in out[1]]
        if eq == tree_form("s_false"):
            return [set(), set(), set()]
        if eq in out[0].keys():
            return [set(item) for item in out[0][eq]]
        if eq.name == "f_or":
            result = [helper2(child) for child in eq.children]
            a = []
            b = []
            c = []
            for item in result:
                a += [item[0]]
                b += [item[1]]
                c += [item[2]]
            x = a[0]
            for item in a[1:]:
                x |= item
            y = b[0]
            for item in b[1:]:
                y |= item
            z = c[0]
            for item in c[1:]:
                z |= item
            return [x, y, z]
        if eq.name == "f_and":
            result = [helper2(child) for child in eq.children]
            a = []
            b = []
            c = []
            for item in result:
                a += [item[0]]
                b += [item[1]]
                c += [item[2]]
            x = a[0]
            for item in a[1:]:
                x &= item
            y = b[0]
            for item in b[1:]:
                y &= item
            z = c[0]
            for item in c[1:]:
                z &= item
            return [x, y, z]
        if eq.name == "f_not":
            eq2 = helper2(eq.children[0])
            a,b,c= eq2
            d,e,f= [set(item) for item in out[1]]
            return [d-a,e-b,f-c]
        return helper2(dowhile(eq, lambda x: logic0(expand(simplify(eq)))))
    out2 = helper2(eq)
    
    out = list(out)
    out[1] = [set(item) for item in out[1]]
    if tuple(out[1]) == (set(), set(), set()):
        return eq
    if tuple(out[1]) == tuple(out2):
        return tree_form("s_true")
    if tuple(out2) == (set(), set(), set()):
        return tree_form("s_false")
    return eq
