from .base import *
from .simplify import simplify
from .formula_data import load_formula

expect = lambda x: load_formula("statistics_1")(x)
expect_2 = lambda x: load_formula("statistics_2")(x)
