def uncommute(eq, inside=False):
    if not inside and eq.name in ["f_add", "f_mul"]:
        return TreeNode(eq.name[:2]+"w"+eq.name[2:], [uncommute(child) for child in eq.children])
    return TreeNode(eq.name, [uncommute(child, True) if eq.name == "f_list" else uncommute(child, inside) for child in eq.children])

def matrix_solve(eq):
    if eq.name == "f_wadd":
        output = None
        output2 = []
        for child in eq.children:
            if child.name == "f_list":
                if output is None:
                    output = []
                    for i in range(len(child.children)):
                        output.append([child.children[i]])
                else:
                    for i in range(len(child.children)):
                        output[i] += [child.children[i]]
                output.append(child)
            else:
                output2.append(child)
        output = [summation(item) for item in output]
