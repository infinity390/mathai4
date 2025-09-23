import copy
from expand import expand
from parser import parse
from printeq import printeq, printeq_log
from simplify import solve, simplify
from integrate import integrate
from diff import diff
from base import *
from factor import _factorconst
from fraction import fraction
from trig import trig0, trig1, trig2, trig3, trig4
def console():
    eq = None
    orig = None
    while True:
        command = input(">>> ")
        try:
            if command == "expand":
                eq = expand(eq)
            elif command == "trig0":
                eq = trig0(eq)
            elif command == "trig1":
                eq = trig1(eq)
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
            elif command.split(" ")[0] == "integrate":
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
                orig = copy.deepcopy(eq)
                eq = parse(command)
            eq = copy.deepcopy(eq)
            printeq(eq)
        except:
            eq = orig
            print("error")
