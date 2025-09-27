import copy
from expand import expand
from parser import parse
from printeq import printeq, printeq_log
from simplify import solve, simplify
from integrate import integrate, typesqint
from diff import diff
from base import *
from factor import _factorconst, factor
from fraction import fraction
from inverse import inverse
from trig import trig0, trig1, trig2, trig3, trig4
from logic import logic0, logic1, logic2, logic3
from apart import apart

def console():
    eq = None
    orig = None
    while True:
        command = input(">>> ")
        try:
            orig = copy.deepcopy(eq)
            if command == "expand":
                eq = expand(eq)
            elif command.split(" ")[0] == "inverse":
                eq=simplify(eq)
                if eq.name == "f_eq":
                    eq3 = eq.children[0]-eq.children[1]
                    eq2 = parse(command.split(" ")[1])
                    out = inverse(eq3, str_form(eq2))
                    eq = TreeNode(eq.name, [eq2,out])
            elif command == "apart":
                eq = apart(eq, vlist(eq)[0])
            elif command == "rawprint":
                print(eq)
            elif command == "logic0":
                eq = logic0(eq)
            elif command == "logic1":
                eq = logic1(eq)
            elif command == "logic2":
                eq = logic2(eq)
            elif command == "logic3":
                eq = logic3(eq)
            elif command == "trig0":
                eq = trig0(eq)
            elif command == "trig1":
                eq = trig1(eq)
            elif command == "factor":
                eq = factor(eq)
            elif command == "trig2":
                eq = trig2(eq)
            elif command == "trig3":
                eq = trig3(eq)
            elif command == "trig4":
                eq = trig4(eq)
            elif command == "simplify":
                eq = _factorconst(eq)
                eq = simplify(eq)
            elif command == "fraction":
                eq = fraction(eq)
            elif command.split(" ")[0] in ["integrate", "sqint"]:
                if command.split(" ")[0] == "sqint":
                    typesqint()
                out = integrate(eq, parse(command.split(" ")[1]).name)
                if out is None:
                    print("failed to integrate")
                else:
                    eq, logs = out
                    eq = simplify(eq)
                    printeq_log(logs)
                    print()
            elif command.split(" ")[0] == "diff":
                eq = diff(eq, parse(command.split(" ")[1]).name)
            else:
                eq = parse(command)
            eq = copy.deepcopy(eq)
            printeq(eq)
        except:
            eq = copy.deepcopy(orig)
            print("error")
