from .parser import parse
from .base import *
from .formula_data import load_formula

lambda_expand = lambda x: load_formula("lambda_expand")(x)
lambda_reduce = lambda x: load_formula("lambda_reduce")(x)
lambda_compress = lambda x: load_formula("lambda_compress")(x)
