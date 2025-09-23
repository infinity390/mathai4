from base import *
from simplify import solve
from expand import expand
def fraction(eq):
    if eq.name == "f_eq":
        return TreeNode(eq.name, [fraction(eq.children[0]), fraction(eq.children[1])])
    if eq.name == "f_add":
        con = []
        for child in eq.children:
            if child.name == "f_pow" and child.children[1].name[:2] == "d_" and int(child.children[1].name[2:])<0:
                den = []
                n = int(child.children[1].name[2:])
                if n == -1:
                    den.append(child.children[0])
                else:
                    den.append(TreeNode("f_pow", [child.children[0], tree_form("d_"+str(-n))]))
                con.append([[], den])
            elif child.name == "f_mul":
                num = []
                den = []
                for child2 in child.children:
                    if child2.name == "f_pow" and child2.children[1].name[:2] == "d_" and int(child2.children[1].name[2:])<0:
                        n = int(child2.children[1].name[2:])
                        if n == -1:
                            den.append(child2.children[0])
                        else:
                            den.append(TreeNode("f_pow", [child2.children[0], tree_form("d_"+str(-n))]))
                    else:
                        num.append(child2)
                con.append([num, den])
            else:
                con.append([[child], []])
        if len(con)>1 and any(x[1] != [] for x in con):
            a = TreeNode("f_add", [])
            for i in range(len(con)):
                b = TreeNode("f_mul", [])
                if con[i][0] != []:
                    b.children += con[i][0]
                for j in range(len(con)):
                    if i ==j:
                        continue
                    b.children +=  con[j][1]
                if len(b.children) == 1:
                    a.children.append(b.children[0])
                elif len(b.children) > 1:
                    a.children.append(b)
                else:
                    a.children.append(tree_form("d_1"))
            c = TreeNode("f_mul", [])
            for i in range(len(con)):
                c.children += con[i][1]
            if len(c.children)==1:
                c = c.children[0]
            c = TreeNode("f_pow", [c, tree_form("d_-1")])
            return TreeNode("f_mul", [expand(a),c])
    arr = TreeNode(eq.name, [])
    for child in eq.children:
        arr.children.append(fraction(child))
    return solve(arr)
