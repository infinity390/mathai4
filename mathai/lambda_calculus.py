from .parser import parse
from .base import *
from .formula_data import load_formula

def lambda_expand_h(eq):
    out = load_formula("lambda_calculus")[0](eq)
    if out is not None:
        return out
    return eq
def lambda_expand(eq):
    out = load_formula("lambda_calculus")[0](eq)
    if out is not None:
        eq = copy.deepcopy(out)
    return dowhile(eq, lambda x: transform_dfs(x, lambda_expand_h))
def lambda_reduce_h(eq):
    out = load_formula("lambda_calculus")[1](eq)
    if out is not None:
        return out
    return eq
def lambda_reduce(eq):
    out = load_formula("lambda_calculus")[1](eq)
    if out is not None:
        eq = copy.deepcopy(out)
    return dowhile(eq, lambda x: transform_dfs(x, lambda_reduce_h))
def lambda_compress_h(eq):
    out = load_formula("lambda_calculus")[2](eq)
    if out is not None:
        return out
    return eq
def lambda_compress(eq):
    out = load_formula("lambda_calculus")[2](eq)
    if out is not None:
        eq = copy.deepcopy(out)
    return dowhile(eq, lambda x: transform_dfs(x, lambda_compress_h))
