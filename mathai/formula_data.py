from .base import *
from .simplify import simplify
from .parser import parse
from .formula_compiler import formula_list_compiler
formula_data = {}
def integration_formula_init():
    formula_list = [
        ("integrate(x^2*e^(a*x+b),x)", "((x^2/a)-(2*x/(a^2))+(2/(a^3)))*e^(a*x+b)", ["v_3", "v_4"],{"v_3": 0}, 6),
        ("integrate(x*e^(a*x+b),x)", "((x/a)-(1/(a^2)))*e^(a*x+b)", ["v_3", "v_4"], {"v_3": 0}, 6),
        ("integrate(e^(a*x+b),x)", "e^(a*x+b)/a", ["v_3", "v_4"], {"v_3": 0}, 6),
        ("integrate((a*x+b)^c,x)", "(a*x+b)^(c+1)/(a*(c+1))", ["v_3", "v_4", "v_5"], {"v_3": 0, "v_5": -1}, 6),
        ("integrate(1/cos(a*x+b)^2,x)", "tan(a*x+b)/a", ["v_3", "v_4"], {"v_3": 0}, 6),
        ("integrate(1/sin(a*x+b)^2,x)", "-cot(a*x+b)/a", ["v_3", "v_4"], {"v_3": 0}, 6),
        ("integrate(1/cos(a*x+b),x)", "log(abs((1+sin(a*x+b))/cos(a*x+b)))/a", ["v_3", "v_4"], {"v_3": 0}, 6),
        ("integrate(1/sin(a*x+b),x)", "log(abs(tan((a*x+b)/2)))/a", ["v_3", "v_4"], {"v_3": 0}, 6),
        ("integrate(1/(a*x+b),x)", "log(abs(a*x+b))/a", ["v_3", "v_4"], {"v_3": 0}, 6),
        ("integrate(sin(a*x+b)^2/cos(a*x+b)^2,x)", "tan(a*x+b)/a-x", ["v_3", "v_4"], {"v_3": 0}, 2),
        ("integrate(sin(a*x+b)/(cos(a*x+b)^2),x)", "1/(a*cos(a*x+b))", ["v_3", "v_4"], {"v_3": 0}, 2),
        ("integrate(sin(a*x+b),x)", "-cos(a*x+b)/a", ["v_3", "v_4"], {"v_3": 0}, 6),
        ("integrate(cos(a*x+b),x)", "sin(a*x+b)/a", ["v_3", "v_4"], {"v_3": 0}, 6),
        ("integrate(x,x)", "x^2/2", [], {}, 6),
        ("integrate(a*b,x)", "a*integrate(b,x)", ["v_3"], {"v_3":1}, 6),
        ("integrate(a+b,x)", "integrate(a,x)+integrate(b,x)", [], {"v_3":0, "v_4":0}, 6)
    ]
    formula_list = [[simplify(parse(x[0])), simplify(parse(x[1])), ["v_0"], "v_0", x[2], x[3], [], [], [], x[4]] for x in formula_list]
    return formula_list_compiler(formula_list)
def diff_formula_init():
    formula_list = [
        (f"pdif(a^b,x)", f"b*(a^(b-1))*pdif(a,x) + (a^b)*log(a)*pdif(b,x)"),
        (f"pdif(sin(a),x)", f"cos(a)*pdif(a,x)"),
        (f"pdif(cos(a),x)", f"-sin(a)*pdif(a,x)"),
        (f"pdif(arcsin(a),x)", f"(1/sqrt(1-a^2))*pdif(a,x)"),
        (f"pdif(arccos(a),x)", f"(-1/sqrt(1-a^2))*pdif(a,x)"),
        (f"pdif(arctan(a),x)", f"(1/(1+a^2))*pdif(a,x)"),
        (f"pdif(x,x)", "1"),
        (f"pdif(k,x)", "0"),
        (f"pdif(c*d,x)", f"pdif(c,x)*d+c*pdif(d,x)"),
        (f"pdif(f+g,x)", f"pdif(f,x)+pdif(g,x)"),
        (f"pdif(k*a,x)", f"k*pdif(a,x)"),
    ]
    formula_list = [[simplify(parse(x[0])), simplify(parse(x[1])), ["v_0"], "v_0", [parse("k").name],\
                     {"v_5":1, "v_6":1, parse("f").name:0, parse("g").name:0}, [], [], []] for x in formula_list]
    return formula_list_compiler(formula_list)
def lambda_formula_init():
    formula_expand = [

        # Identity
        ("I",
         "lambda(x,x)"),

        # true
        ("true",
         "lambda(t,lambda(f,t))"),

        # false
        ("false",
         "lambda(t,lambda(f,f))"),

        # 0
        ("0",
         "lambda(f,lambda(x,x))"),

    ]
    formula_reduce = [

        ("apply(lambda(a,a),b)", "b"),
        ("apply(lambda(a,apply(c,a)),b)", "apply(c,b)")
    ]
    formula_compress = [

        (
            "lambda(x,x)",
            "I"
        ),
        (
            "lambda(t,lambda(f,t))",
            "true"
        ),

        (
            "lambda(t,lambda(f,f))",
            "false"
        ),

        (
            "lambda(f,lambda(x,x))",
            "0"
        )

    ]
    ig_list = [parse(item).name for item in "x y f t A B C D E F G H I J K L M N O P Q R S T U V X W Y Z".split(" ")]
    formula_expand = [[parse(x[0]), parse(x[1]), ig_list, None, [], "forbid", None, [], [], 1] for x in formula_expand]
    formula_reduce = [[parse(x[0]), parse(x[1]), ig_list, None, [], "forbid", None, [], [], 1] for x in formula_reduce]
    formula_compress = [[parse(x[0]), parse(x[1]), ig_list, None, [], "forbid", None, [], [], 1] for x in formula_compress]
    return formula_list_compiler(formula_expand), formula_list_compiler(formula_reduce), formula_list_compiler(formula_compress)
def limit_formula_init():
    formula_list = [
        ("limitpinf(a*b,x)", "a*limitpinf(b,x)", ["v_3"], {"v_3":1}),
        ("limitpinf(x^c*e^(d*x),x)", "0", [], {}),
        ("limitpinf(x*e^(d*x),x)", "0", [], {}),
        ("limitpinf(e^(d*x),x)", "0", [], {}),
        ("limitpinf(a+b,x)", "limitpinf(a,x)+limitpinf(b,x)", [], {}),
    ]
    formula_list = [[simplify(parse(x[0])), simplify(parse(x[1])), ["v_0"], "v_0", x[2], x[3], [], [], []] for x in formula_list]
    return formula_list_compiler(formula_list)
def trig_formula_init():
    formula_list = [
        ("1/(1+sin(x))", "(1-sin(x))/cos(x)^2", [], {}),
        ("1/(1+cos(x))", "(1-cos(x))/sin(x)^2", [], {"v_0":0}),
    ]
    formula_list = [[simplify(parse(x[0])), simplify(parse(x[1])), [], None, x[2], x[3], None, None, None] for x in formula_list]
    return formula_list_compiler(formula_list)
def diffmat_formula_init():
    formula_list = [
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
        (
            "pdif(vec(transpose(a)),vec(x))",
            "wmul(commutation(len(a),len(index(a,0))),pdif(vec(a),vec(x)))"
        ),
        (
            "pdif(vec(hadamard(a,b)),vec(x))",
            "wadd("
            "wmul(diag(b),pdif(vec(a),vec(x))),"
            "wmul(diag(a),pdif(vec(b),vec(x)))"
            ")"
        ),
        (
            "pdif(vec(broadcast(a,r)),vec(x))",
            "wmul(kronecker(identity(len(vec(a))),wadd(1,zeros(r,1))),pdif(vec(a),vec(x)))"
        ),
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
        (
            "pdif(vec(wmul(a,b)),vec(x))",
            "wadd("
            "wmul(kronecker(transpose(b),identity(len(a))),pdif(vec(a),vec(x))),"
            "wmul(kronecker(identity(len(index(b,0))),a),pdif(vec(b),vec(x)))"
            ")"
        ),
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
    formula_list = [[simplify(parse(x[0])), simplify(parse(x[1])), [], None, [parse("k").name], {}, parse("m").name, [], []] for x in formula_list]
    return formula_list_compiler(formula_list)
def factor_formula_init():
    formula_list = [
        ("a*x^2+b*x+c", "a*(x-(-b+sqrt(b^2 - 4*a*c))/(2*a))*(x-(-b-sqrt(b^2 - 4*a*c))/(2*a))",\
         ["v_3", "v_4", "v_5"],{"v_3": 0}),
    ]
    formula_list = [[simplify(parse(x[0])), simplify(parse(x[1])), [], "v_0", x[2], x[3], None, [simplify(parse("b^2 - 4*a*c"))], [], 2] for x in formula_list]
    return formula_list_compiler(formula_list)
def load_formula(label):
    global formula_data
    return formula_data[label]

def init_formula(label_list="all"):
    global formula_data
    print("initialising formula compilation...")
    all_label_list = "integration differentiation lambda_calculus trigonometry_5 limit_inf matrix_vec_calculus quadratic".split(" ")
    label_vs_fx = [integration_formula_init, diff_formula_init, lambda_formula_init, trig_formula_init, limit_formula_init, diffmat_formula_init, factor_formula_init]
    if label_list == "all":
        label_list = all_label_list
    for item in label_list:
        out = label_vs_fx[all_label_list.index(item)]()
        print(f"{item} formula set compiled")
        formula_data[item] = out
    print()
    return None
