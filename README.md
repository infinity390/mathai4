# Math AI Documentation
## Source
Github repository of the code
https://github.com/infinity390/mathai4

## Philosophy
I think it is a big realization in computer science and programming to realize that computers can solve mathematics.  
This understanding should be made mainstream. It can help transform education, mathematical research, and computation of mathematical equations for work.

## Societal Implications Of Such A Computer Program And The Author's Comment On Universities Of India
I think mathematics is valued by society because of education. Schools and universities teach them.  
So this kind of software, if made mainstream, could bring real change.

## The Summary Of How Computer "Solves" Math
Math equations are a tree data structure (`TreeNode` class).  
We can manipulate the math equations using various algorithms (functions provided by the `mathai` library).  
We first parse the math equation strings to get the tree data structure (`parse` function in `mathai`).

## The Library
Import the library by doing:

```python
from mathai import *
```

### str_form
It is the string representation of a `TreeNode` math equation.

#### Example
```text
(cos(x)^2)+(sin(x)^2)
```

Is represented internally as:

```text
f_add
 f_pow
  f_cos
   v_0
  d_2
 f_pow
  f_sin
   v_0
  d_2
```

#### Leaf Nodes

**Variables** (start with a `v_` prefix):

- `v_0` -> x
- `v_1` -> y
- `v_2` -> z
- `v_3` -> a

**Numbers** (start with `d_` prefix; only integers):

- `d_-1` -> -1
- `d_0` -> 0
- `d_1` -> 1
- `d_2` -> 2

#### Branch Nodes
- `f_add` -> addition
- `f_mul` -> multiplication
- `f_pow` -> power

### parse
Takes a math equation string and outputs a `TreeNode` object.

```python
from mathai import *

equation = parse("sin(x)^2+cos(x)^2")
print(equation)
```

#### Output
```text
(cos(x)^2)+(sin(x)^2)
```

### simplify
It simplifies and cleans up a given math equation.

```python
from mathai import *

equation = simplify(parse("(x+x+x+x-1-1-1-1)*(4*x-4)*sin(sin(x+x+x)*sin(3*x))"))
printeq(equation)
```

#### Output
```text
((-4+(4*x))^2)*sin((sin((3*x))^2))
```

### Incomplete Documentation, Will be updated and completed later on

### Demonstrations

#### Example Demonstration 1 (absolute value inequalities)
```python
from mathai import *
question_list_from_lecture = [
    "2*x/(2*x^2 + 5*x + 2) > 1/(x + 1)",
    "(x + 2)*(x + 3)/((x - 2)*(x - 3)) <= 1",
    "(5*x - 1) < (x + 1)^2 & (x + 1)^2 < 7*x - 3",
    "(2*x - 1)/(2*x^3 + 3*x^2 + x) > 0",
    "abs(x + 5)*x + 2*abs(x + 7) - 2 = 0",
    "x*abs(x) - 5*abs(x + 2) + 6 = 0",
    "x^2 - abs(x + 2) + x > 0",
    "abs(abs(x - 2) - 3) <= 2",
    "abs(3*x - 5) + abs(8 - x) = abs(3 + 2*x)",
    "abs(x^2 + 5*x + 9) < abs(x^2 + 2*x + 2) + abs(3*x + 7)"
]

for item in question_list_from_lecture:
  eq = simplify(parse(item))
  eq = dowhile(eq, absolute)
  eq = simplify(factor1(fraction(eq)))
  eq = prepare(eq)
  eq = factor2(eq)
  c = wavycurvy(eq & domain(eq)).fix()
  print(c)
```
#### Output

```
(-2,-1)U(-(2/3),-(1/2))
(-inf,0)U(2,3)U{0}
(2,4)
(-inf,-1)U(-(1/2),0)U(1/2,+inf)
{-4,-3,-(3/2)-(sqrt(57)/2)}
{-1,(5/2)-(sqrt(89)/2),(5/2)+(sqrt(41)/2)}
(-inf,-sqrt(2))U((2*sqrt(2))/2,+inf)
(-3,1)U(3,7)U{1,-3,7,3}
(5/3,8)U{5/3,8}
(-inf,-(7/3))
```

#### Example Demonstration 2 (trigonometry)
```python
from mathai import *
def nested_func(eq_node):
    eq_node = fraction(eq_node)
    eq_node = simplify(eq_node)
    eq_node = trig1(eq_node)
    eq_node = trig0(eq_node)
    return eq_node
for item in ["(cosec(x)-cot(x))^2=(1-cos(x))/(1+cos(x))", "cos(x)/(1+sin(x)) + (1+sin(x))/cos(x) = 2*sec(x)",\
             "tan(x)/(1-cot(x)) + cot(x)/(1-tan(x)) = 1 + sec(x)*cosec(x)", "(1+sec(x))/sec(x) = sin(x)^2/(1-cos(x))",\
             "(cos(x)-sin(x)+1)/(cos(x)+sin(x)-1) = cosec(x)+cot(x)"]:
  eq = logic0(dowhile(parse(item), nested_func))
  print(eq)
```
#### Output

```
true
true
true
true
true
```

#### Example Demonstration 3 (integration)
```python
from mathai import *

eq = simplify(parse("integrate(2*x/(x^2+1),x)"))
eq = integrate_const(eq)
eq = integrate_fraction(eq)
print(simplify(fraction(simplify(eq))))

eq = simplify(parse("integrate(sin(cos(x))*sin(x),x)"))
eq = integrate_subs(eq)
eq = integrate_const(eq)
eq = integrate_formula(eq)
eq = integrate_clean(eq)
print(simplify(eq))

eq = simplify(parse("integrate(x*sqrt(x+2),x)"))
eq = integrate_subs(eq)
eq = integrate_const(eq)
eq = integrate_formula(eq)
eq = expand(eq)
eq = integrate_const(eq)
eq = integrate_summation(eq)
eq = simplify(eq)
eq = integrate_const(eq)
eq = integrate_formula(eq)
eq = integrate_clean(eq)
print(simplify(fraction(simplify(eq))))

eq = simplify(parse("integrate(x/(e^(x^2)),x)"))
eq = integrate_subs(eq)
eq = integrate_const(eq)
eq = integrate_formula(eq)
eq = simplify(eq)
eq = integrate_formula(eq)
eq = integrate_clean(eq)
print(simplify(eq))

eq = fraction(trig0(trig1(simplify(parse("integrate(sin(x)^4,x)")))))
eq = integrate_const(eq)
eq = integrate_summation(eq)
eq = integrate_formula(eq)
eq = integrate_const(eq)
eq = integrate_formula(eq)
print(factor0(simplify(fraction(simplify(eq)))))
```
#### Output

```
log(abs((1+(x^2))))
cos(cos(x))
((6*((2+x)^(5/2)))-(20*((2+x)^(3/2))))/15
-((e^-(x^2))/2)
-(((8*sin((2*x)))-(12*x)-sin((4*x)))/32)
```

#### Example Demonstration 4 (derivation of hydrogen atom's ground state energy in electron volts using the variational principle in quantum physics)
```python
from mathai import *;
def auto_integration(eq):
    for _ in range(3):
        eq=dowhile(integrate_subs(eq),lambda x:integrate_summation(integrate_const(integrate_formula(simplify(expand(x))))));
        out=integrate_clean(copy.deepcopy(eq));
        if "f_integrate" not in str_form(out):return dowhile(out,lambda x:simplify(fraction(x)));
        eq=integrate_byparts(eq);
    return eq;
z,k,m,e1,hbar=map(lambda s:simplify(parse(s)),["1","8987551787","9109383701*10^(-40)","1602176634*10^(-28)","1054571817*10^(-43)"]);
pi,euler,r=tree_form("s_pi"),tree_form("s_e"),parse("r");a0=hbar**2/(k*e1**2*m);psi=((z**3/(pi*a0**3)).fx("sqrt"))*euler**(-(z/a0)*r);
laplace_psi=diff(r**2*diff(psi,r.name),r.name)/r**2;V=-(k*z*e1**2)/r;Hpsi=-hbar**2/(2*m)*laplace_psi+V*psi;
norm=lambda f:simplify(
    limit3(limit2(expand(TreeNode("f_limitpinf",[auto_integration(TreeNode("f_integrate",[f*parse("4")*pi*r**2,r])),r]))))
    -limit1(TreeNode("f_limit",[auto_integration(TreeNode("f_integrate",[f*parse("4")*pi*r**2,r])),r]))
);
print(compute(norm(psi*Hpsi)/(norm(psi**2)*e1)));
```
#### Output

```
-13.605693122882867
```

#### Example Demonstration 5 (boolean algebra)
```python
from mathai import *
print(logic_n(simplify(parse("~(p<->q)<->(~p<->q)"))))
print(logic_n(simplify(parse("(p->q)<->(~q->~p)"))))
```
#### Output

```
true
true
```

#### Example Demonstration 6 (limits)
```python
from mathai import *
limits = ["(e^(tan(x)) - 1 - tan(x)) / x^2", "sin(x)/x", "(1-cos(x))/x^2", "(sin(x)-x)/sin(x)^3"]
for q in limits:
    q = fraction(simplify(TreeNode("f_limit",[parse(q),parse("x")])))
    q = limit1(q)
    print(q)
```
#### Output

```
1/2
1
1/2
-(1/6)
```