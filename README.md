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
    "abs(x)>=0",
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
i=1
for item in question_list_from_lecture:
  eq = simplify(parse(item))
  eq = dowhile(eq, absolute)
  eq = simplify(eq)
  eq = dowhile(eq, fraction)
  eq = simplify(eq)
  eq = factor1(eq)
  eq = prepare(eq)
  eq = simplify(eq)
  eq = factor2(eq)
  c = wavycurvy(eq & domain(eq)).fix()
  print(f"QUESTION {i}. {item}\nANSWER {i}. {c}\n")
  i += 1
```
#### Output

```
QUESTION 1. abs(x)>=0
ANSWER 1. (-inf,+inf)

QUESTION 2. 2*x/(2*x^2 + 5*x + 2) > 1/(x + 1)
ANSWER 2. (-2,-1)U(-(2/3),-(1/2))

QUESTION 3. (x + 2)*(x + 3)/((x - 2)*(x - 3)) <= 1
ANSWER 3. (-inf,0)U(2,3)U{0}

QUESTION 4. (5*x - 1) < (x + 1)^2 & (x + 1)^2 < 7*x - 3
ANSWER 4. (2,4)

QUESTION 5. (2*x - 1)/(2*x^3 + 3*x^2 + x) > 0
ANSWER 5. (-inf,-1)U(-(1/2),0)U(1/2,+inf)

QUESTION 6. abs(x + 5)*x + 2*abs(x + 7) - 2 = 0
ANSWER 6. {-(3/2)-(sqrt(57)/2),-4,-3}

QUESTION 7. x*abs(x) - 5*abs(x + 2) + 6 = 0
ANSWER 7. {-1,(5/2)-(sqrt(89)/2),(5/2)+(sqrt(41)/2)}

QUESTION 8. x^2 - abs(x + 2) + x > 0
ANSWER 8. (-inf,-sqrt(2))U((2*sqrt(2))/2,+inf)

QUESTION 9. abs(abs(x - 2) - 3) <= 2
ANSWER 9. (-3,1)U(3,7)U{3,1,7,-3}

QUESTION 10. abs(3*x - 5) + abs(8 - x) = abs(3 + 2*x)
ANSWER 10. (5/3,8)U{8,5/3}

QUESTION 11. abs(x^2 + 5*x + 9) < abs(x^2 + 2*x + 2) + abs(3*x + 7)
ANSWER 11. (-inf,-(7/3))
```

#### Example Demonstration 2 (integration)
```python
from mathai import *
eq = parse("integrate((7^(7^(7^x)))*(7^(7^x))*(7^x),x)")
eq = integrate_subs(eq)
eq = integrate_const(eq)
eq = integrate_formula(eq)
eq = simplify(eq)
eq = integrate_clean(eq)
print(eq)
```
#### Output

```
(7^(7^(7^x)))/(log(7)^3)
```

#### Example Demonstration 3 (derivation of hydrogen atom's ground state energy in electron volts using the variational principle in quantum physics)
```python
from mathai import *
basic_int = lambda x: dowhile(x, lambda y: fraction(simplify(integrate_const(integrate_formula(integrate_summation(expand(y)))))))
algebra = lambda x: dowhile(x, lambda y: fraction(simplify(y)))
z =  simplify(parse("1"))
k =  simplify(parse("8987551787"))
m =  simplify(parse("9109383701 * 10^(-40)"))
e1=  simplify(parse("1602176634 * 10^(-28)"))
hbar=simplify(parse("1054571817 * 10^(-43)"))
pi = tree_form("s_pi")
euler = tree_form("s_e")
r = parse("r")
a0 = hbar**2 / (k*e1**2*m)
c2 = z/a0
c1 = (z**3 / (pi * a0**3)).fx("sqrt")
psi = c1 * euler**(-c2 * r)
psi2 = psi**2
laplace_psi = diff(r**2 * diff(psi, r.name), r.name)/r**2
psi2 = simplify(psi2)
integral_psi2 = TreeNode("f_integrate", [psi2 * parse("4")* pi * r**2, r])
integral_psi2 = simplify(integral_psi2)
integral_psi2 = integrate_subs(integral_psi2)
integral_psi2 = basic_int(integral_psi2)
integral_psi2 = integrate_byparts(integral_psi2)
integral_psi2 = basic_int(integral_psi2)
integral_psi2 = integrate_byparts(integral_psi2)
integral_psi2 = basic_int(integral_psi2)
integral_psi2 = integrate_clean(integral_psi2)
integral_psi2 = algebra(integral_psi2)
a = limit1(TreeNode("f_limit", [integral_psi2, r]))
b = limit3(TreeNode("f_limitpinf", [integral_psi2, r]))
integral_psi2 = simplify(b-a)
V = -(k * z * e1**2)/r
Hpsi = -hbar**2/(2*m) * laplace_psi + V*psi
psiHpsi = psi * Hpsi
integral_psiHpsi = TreeNode("f_integrate", [psiHpsi * parse("4")* pi * r**2, r])
integral_psiHpsi = simplify(integral_psiHpsi)
integral_psiHpsi = integrate_subs(integral_psiHpsi)
integral_psiHpsi = basic_int(integral_psiHpsi)
integral_psiHpsi = integrate_byparts(integral_psiHpsi)
integral_psiHpsi = basic_int(integral_psiHpsi)
integral_psiHpsi = integrate_byparts(integral_psiHpsi)
integral_psiHpsi = basic_int(integral_psiHpsi)
integral_psiHpsi = integrate_clean(integral_psiHpsi)
integral_psiHpsi = algebra(integral_psiHpsi)
a = limit1(TreeNode("f_limit", [integral_psiHpsi, r]))
b = limit3(limit2(expand(TreeNode("f_limitpinf", [integral_psiHpsi, r]))))
integral_psiHpsi = simplify(b-a)
result =  integral_psiHpsi / integral_psi2
print(compute(result /e1))
```
#### Output

```
-13.605693122882867
```

#### Example Demonstration 4 (boolean algebra)
```python
from mathai import *
eq = parse("(A-B)|(B-A)")
eq = set_sub(eq)
eq = solve_logically(eq)
print(eq)
eq = parse("(A|B)-(A&B)")
eq = set_sub(eq)
eq = solve_logically(eq)
print(eq)
```
#### Output

```
(~A&B)|(~B&A)
(~A&B)|(~B&A)
```

#### Example Demonstration 5 (limits approaching to a constant value)
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

#### Example Demonstration 6 (limits approaching to infinity)
```python
from mathai import *
eq= parse("limitpinf((3*x^2+x)/(2*x^2+5),x)")
eq = simplify(eq)
eq = limit4(eq)
eq = simplify(eq)
eq = limit3(eq)
eq = simplify(eq)
print(eq)
```
#### Output

```
3/2
```

#### Example Demonstration 7 (definite integration)
```python
from mathai import *
eq = parse("integrate(abs(sin(x)),x,-pi,pi/6)")
lst = [simplify, integrate_definite, integrate_const, integrate_formula, integrate_clean,\
       simplify, trig0, simplify, trig0, simplify]
for item in lst:
  print(eq)
  eq = item(eq)
print(eq)
```
#### Output

```
integrate(abs(sin(x)),x,(0-pi),(pi/6))
integrate(abs(sin(x)),x,-pi,(pi/6))
-subs(integrate(-sin(x),x),x,-pi)-subs(integrate(sin(x),x),x,0)+subs(integrate(-sin(x),x),x,0)+subs(integrate(sin(x),x),x,(pi/6))
-subs(integrate(sin(x),x),x,0)-subs(-integrate(sin(x),x),x,-pi)+subs(integrate(sin(x),x),x,(pi/6))+subs(-integrate(sin(x),x),x,0)
-subs(cos(x),x,-pi)-subs(-cos(x),x,0)+subs(-cos(x),x,(pi/6))+subs(--cos(x),x,0)
-cos((pi/6))-cos(-pi)--cos(0)--cos(0)
(2*cos(0))-cos((pi/6))-cos(-pi)
2-cos(pi)-(sqrt(3)/2)
2-cos(pi)-(sqrt(3)/2)
2--1-(sqrt(3)/2)
3-(sqrt(3)/2)
```

#### Example Demonstration 8 (linear equations) (general solution of linear equations in two variables)
```python
from mathai import *
eq= parse("a*x+b*y+c = 0 & d*x+f*y+g = 0")
eq = simplify(eq)
eq = linear_solve(eq, [parse(item) for item in "a b c d f g".split(" ")])
eq = factor1(eq)
eq = simplify(eq)
for item in eq.children:
  print(item)
```
#### Output

```
((((c*f)-(b*g))/((a*f)-(b*d)))+x)=0
((((a*g)-(c*d))/((a*f)-(b*d)))+y)=0
```

#### Example Demonstration 9 (expectation algebra)
```python
from mathai import *
eq = simplify(parse("covariance(A+B,C+D)=covariance(B,C)+covariance(B,D)+covariance(A,D)+covariance(A,C)"))
eq = logic0(simplify(expect(simplify(expand(expect(eq))))))
print(eq)
```
#### Output

```
true
```

### 55 NCERT questions solved by the library

```python
from mathai import *
title = "class 12 ncert maths part-II chapter 9 exercise 9.3, question "
set1 = ("dif(y,x) = sqrt(4-y^2); # 2", "dif(y,x)+y=1; # 3",  "x^5*dif(y,x)=-y^5; # 8")
set2 = ("dif(y,x)=(1+x^2)*(1+y^2); # 6", "x*(x^2 - 1)*dif(y,x)=1; # 12")
set3 = ("dif(y,x)=arcsin(x); # 9",)
set4 = ("sec(x)^2*tan(y)+sec(y)^2*tan(x)*dif(y,x)=0; # 4", )
set1, set2, set3, set4 = [tuple([x.replace("#", title) for x in item]) for item in [set1, set2, set3, set4]]
title = "class 12 ncert maths part-II chapter 7 exercise 7.2, question "
set5 = ("integrate(2*x/(1+x^2),x); # 1", "integrate(sqrt(a*x+b),x); # 6", "integrate(x*sqrt(x),x); # 7",
        "integrate(x*sqrt(1+2*x^2),x); # 8", "integrate(x/sqrt(x+4),x); # 11", "integrate(e^(2*x+3),x); # 16",
        "integrate(x/e^(x^2),x); # 17", "integrate(sqrt(sin(2*x))*cos(2*x),x); # 27", "integrate(sin(x)*sin(cos(x)),x); # 3")
set5  = tuple([x.replace("#", title) for x in set5])
title = "class 12 ncert maths part-II chapter 7 exercise 7.3, question "
set6 = ("integrate(sin(2*x+5)^2,x); # 1", "integrate(sin(3*x)*cos(4*x),x); # 2", "integrate(cos(2*x)*cos(4*x)*cos(6*x),x); # 3",
        "integrate(sin(2*x+1)^3,x); # 4", "integrate(sin(x)^3*cos(x)^3,x); # 5", "integrate(sin(x)*sin(2*x)*sin(3*x),x); # 6",
        "integrate(sin(4*x)*sin(8*x),x); # 7", "integrate(sin(x)^4,x); # 10", "integrate(cos(2*x)^4,x); # 11")
set6 = tuple([x.replace("#", title) for x in set6])
title = "class 12 ncert maths part-II chapter 7 exercise 7.5, question "
set7 = ("integrate(x/((x+1)*(x+2)),x); # 1", "integrate(1/(x^2-9),x); # 2", "integrate((3*x-1)/((x-1)*(x-2)*(x-3)),x); # 3",
        "integrate(x/((x-1)*(x-2)*(x-3)),x); # 4", "integrate(2*x/(x^2+3*x+2),x); # 5", "integrate((1-x^2)/(x*(1-2*x)),x); # 6",
        "integrate(x/((x-1)*(x^2+1)),x); # 7", "integrate(x/((x-1)^2*(x+2)),x); # 8", "integrate((2*x-3)/((x^2-1)*(2*x+3)),x); # 10",
        "integrate(5*x/((x+1)*(x^2-4)),x); # 11", "integrate(2/((1-x)*(1+x^2)),x); # 13")
set7 = tuple([x.replace("#", title) for x in set7])
title = "class 12 ncert maths part-II chapter 9 exercise 9.4, question "
set8 = ("(x^2+x*y)*dif(y,x) = (x^2 + y^2); # 1", "dif(y,x)=(x+y)/x; # 2", "(x-y)*dif(y,x)-(x+y)=0; # 3",
        "(x^2-y^2)+2*x*y*dif(y,x)=0; # 4")
set9 = ("x*dif(y,x)-y+x*sin(y/x)=0; # 8",)
set8, set9 = [tuple([x.replace("#", title) for x in item]) for item in [set8, set9]]
title = "class 12 ncert maths part-II chapter 9 exercise 9.5, question "
set10 = ("(x+y)*dif(y,x) = 1; # 10", )
set10 = tuple([x.replace("#", title) for x in set10])
title = "class 11 ncert maths chapter 5 exercise 5.1, question "
set11 = ("4*x+3 < 5*x+7; # 1", "x/2 >= (5*x-2)/3 - (7*x - 3)/5; # 20")
set11 = tuple([x.replace("#", title) for x in set11])
title = "class 10 ncert maths chapter 8 exercise 8.3, 4th question, section "
set12 = ("(cosec(x)-cot(x))^2 = (1-cos(x))/(1+cos(x)); # 1",  "cos(x)/(1+sin(x)) + (1+sin(x))/cos(x) = 2*sec(x); # 2",
         "tan(x)/(1-cot(x)) + cot(x)/(1-tan(x)) = 1 + sec(x)*cosec(x); # 3", "(1+sec(x))/sec(x) = sin(x)^2/(1-cos(x)); # 4",
         "(cos(x)-sin(x)+1)/(cos(x)+sin(x)-1) = cosec(x)+cot(x); # 5", "(sin(x)-2*sin(x)^3)/(2*cos(x)^3-cos(x))=tan(x); # 7",
         "(sin(x)+cosec(x))^2 + (cos(x)+sec(x))^2 = 7+tan(x)^2+cot(x)^2; # 8", "(cosec(x)-sin(x))*(sec(x)-cos(x)) = 1/(tan(x)+cot(x)); # 9",)
set12 = tuple([x.replace("#", title) for x in set12])
title = "class 11 ncert maths chapter 1 miscellaneous exercise, 6th question, section "
set13 = ("A<->((A&B)|(A-B)); # 1", "A|(B-A)<->(A|B); # 2")
set13 = tuple([x.replace("#", title) for x in set13])
set14 = ("x^2*dif(y,x)=x^2-2*y^2+x*y; # 5",)
set14 = tuple([x.replace("#", title) for x in set14])
count = 1
lst1 = [parse, simplify, trig0, fraction, ode_solve, integrate_const,
       integrate_formula, simplify, integrate_subs, trig4, simplify,
       integrate_formula, integrate_summation, fraction,
       simplify, integrate_const, simplify, integrate_fraction, expand, simplify,
       integrate_summation, integrate_const, integrate_formula, apart, integrate_const,
       integrate_summation, integrate_formula, integrate_const, integrate_fraction]
lst2 = [parse, simplify, trig0, fraction, ode_solve, integrate_const,
       integrate_formula, simplify, integrate_formula, fraction,
       simplify, integrate_const, simplify, integrate_fraction, integrate_formula,
       factor2, factor1, simplify, apart, simplify, integrate_summation, integrate_formula, integrate_const, integrate_formula,
       apart, integrate_summation, integrate_const, integrate_summation, integrate_formula]
lst3 = [parse, simplify, trig0, fraction, ode_solve, integrate_const,
       integrate_formula, simplify, integrate_byparts, trig4, simplify,
       integrate_formula, integrate_summation, fraction,
       simplify, integrate_const, simplify, integrate_fraction, integrate_formula]
lst4 = [parse, simplify, trig0, fraction, fraction, simplify, ode_solve, integrate_const,
       integrate_formula, simplify, integrate_subs, simplify, trig4,
       integrate_const, factor2, integrate_const, apart, integrate_summation, integrate_const,
       integrate_formula]
lst5 = [parse, simplify, trig0, fraction, integrate_const,
       integrate_formula, simplify, integrate_subs, simplify, integrate_const, simplify,
       integrate_formula, integrate_const, expand, integrate_summation, integrate_const,
       integrate_formula, fraction, simplify, factor0]
lst6 = [parse, simplify, trig1, fraction, fraction, integrate_const,
       integrate_formula, simplify, fraction, expand, simplify, integrate_summation, integrate_const,
       integrate_formula]
lst7 = [parse, simplify, factor2, simplify, apart, integrate_const,
       integrate_formula, simplify, integrate_summation, integrate_const,
       integrate_formula, fraction, integrate_const, integrate_formula, expand, simplify,
       integrate_summation, integrate_const, simplify, integrate_fraction]
lst8 = [parse, simplify, fraction, lambda x: dowhile(x, absolute), fraction, simplify, factor2, prepare, lambda x: wavycurvy(x).fix()]
lst9 = [parse, simplify, trig0, lambda x: dowhile(x, lambda y: simplify(expand(simplify(fraction(y))))), trig1, simplify, expand, simplify, logic0]
lst10 = [parse, set_sub, simplify, truth_gen, logic4]
question = {set4:lst4, set14: lst2, set9: lst1, set13: lst10, set12: lst9, set11: lst8, set10: lst3, set2: lst2, set8: lst1, set7: lst7, set6: lst6, set5: lst5, set3: lst3, set1: lst1}
def proof_prb(orig, eq):
  if not isinstance(eq, TreeNode):
    return True
  s = str_form(eq)
  t = str_form(orig)
  if "f_integrate" in t or "f_dif" in t or "f_try" in t or "f_subs" in t:
    return False
  return any("f_"+item in s for item in "and or not imply equiv eq lt le gt ge".split(" "))
for key in question.keys():
  lst = question[key]
  for eq in list(key):
    if ";" in eq:
      eq, source = eq.split(";")
      print(f"===========\nquestion {count}\n===========\n({source.upper()} )\nsolution with steps:\n")
    else:
      print(f"===========\nquestion {count}\n===========\nsolution with steps:\n")
    count += 1
    orig = parse(eq)
    old = None
    for func in lst+[lambda x: x]:
      if old != eq:
        print(eq)
      old = eq
      if not proof_prb(orig, eq) and "f_integrate" not in str_form(eq) and "f_dif" not in str_form(eq) and "f_try" in str_form(eq):
        eq = integrate_clean(eq)
        print(eq)
        break
      eq = func(eq)
      if not isinstance(eq, TreeNode) or (not proof_prb(orig, eq) and all("f_"+x not in str_form(eq) for x in "dif integrate subs try".split(" "))):
        print(eq)
        break
    if isinstance(eq, TreeNode) and (proof_prb(orig, eq) or any("f_"+x in str_form(eq) for x in "dif integrate subs try".split(" "))):
      if not proof_prb(orig, eq):
        eq = integrate_clean(eq)
      print(eq)
      if proof_prb(orig, eq) or any("f_"+x in str_form(eq) for x in "dif integrate subs try".split(" ")):
        raise Exception("solution failed")
    print()
```
#### Output

```
===========
question 1
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 9 EXERCISE 9.3, QUESTION  4 )
solution with steps:

sec(x)^2*tan(y)+sec(y)^2*tan(x)*dif(y,x)=0
((dif(y,x)*((sec(y)^2)*tan(x)))+((sec(x)^2)*tan(y)))=0
((dif(y,x)*(sec(y)^2)*tan(x))+((sec(x)^2)*tan(y)))=0
((((dif(y,x)*sin(x))/cos(x))/(cos(y)^2))+((sin(y)/cos(y))/(cos(x)^2)))=0
(((cos(x)*(cos(y)^2)*sin(y))+(cos(y)*dif(y,x)*(cos(x)^2)*sin(x)))/((cos(x)^3)*(cos(y)^3)))=0
((cos(x)*(cos(y)^2)*sin(y))+(cos(y)*dif(y,x)*(cos(x)^2)*sin(x)))=0
((integrate((-(1/cos(x))/sin(x)),x)+c1)+integrate((-(1/cos(y))/sin(y)),y))=0
(integrate(-(1/(cos(x)*sin(x))),x)+integrate(-(1/(cos(y)*sin(y))),y)+c1)=0
(try(integrate(-(1/(cos(y)*sin(y))),y),subs(integrate(-(1/((cos(arcsin(z))^2)*z)),z),z,sin(y)),subs(integrate((1/((sin(arccos(z))^2)*z)),z),z,cos(y)))+try(subs(integrate(-(1/((cos(arcsin(y))^2)*y)),y),y,sin(x)),integrate(-(1/(cos(x)*sin(x))),x),subs(integrate((1/((sin(arccos(y))^2)*y)),y),y,cos(x)))+c1)=0
(try(integrate(-(1/(cos(y)*sin(y))),y),subs(integrate(-(1/((1-(z^2))*z)),z),z,sin(y)),subs(integrate((1/((1-(z^2))*z)),z),z,cos(y)))+try(subs(integrate(-(1/((1-(y^2))*y)),y),y,sin(x)),integrate(-(1/(cos(x)*sin(x))),x),subs(integrate((1/((1-(y^2))*y)),y),y,cos(x)))+c1)=0
(try(-integrate((1/(cos(y)*sin(y))),y),subs(-integrate((1/((1-(z^2))*z)),z),z,sin(y)),subs(integrate((1/((1-(z^2))*z)),z),z,cos(y)))+try(subs(-integrate((1/((1-(y^2))*y)),y),y,sin(x)),-integrate((1/(cos(x)*sin(x))),x),subs(integrate((1/((1-(y^2))*y)),y),y,cos(x)))+c1)=0
(try(-integrate((1/(cos(y)*sin(y))),y),subs(-integrate(-(1/((-1+z)*(1+z)*z)),z),z,sin(y)),subs(integrate(-(1/((-1+z)*(1+z)*z)),z),z,cos(y)))+try(subs(-integrate(-(1/((-1+y)*(1+y)*y)),y),y,sin(x)),-integrate((1/(cos(x)*sin(x))),x),subs(integrate(-(1/((-1+y)*(1+y)*y)),y),y,cos(x)))+c1)=0
(try(-integrate((1/(cos(y)*sin(y))),y),subs(--integrate((1/((-1+z)*(1+z)*z)),z),z,sin(y)),subs(-integrate((1/((-1+z)*(1+z)*z)),z),z,cos(y)))+try(subs(--integrate((1/((-1+y)*(1+y)*y)),y),y,sin(x)),-integrate((1/(cos(x)*sin(x))),x),subs(-integrate((1/((-1+y)*(1+y)*y)),y),y,cos(x)))+c1)=0
(try(-integrate((1/(cos(y)*sin(y))),y),subs(integrate(((1/z)-(1/(2*(-1+z)))-(1/(2*(1+z)))),z),z,sin(y)),subs(-integrate(((1/z)-(1/(2*(-1+z)))-(1/(2*(1+z)))),z),z,cos(y)))+try(subs(integrate(((1/y)-(1/(2*(-1+y)))-(1/(2*(1+y)))),y),y,sin(x)),-integrate((1/(cos(x)*sin(x))),x),subs(-integrate(((1/y)-(1/(2*(-1+y)))-(1/(2*(1+y)))),y),y,cos(x)))+c1)=0
(try(-integrate((1/(cos(y)*sin(y))),y),subs(((integrate(-(1/(2*(-1+z))),z)+integrate(-(1/(2*(1+z))),z))+integrate((1/z),z)),z,sin(y)),subs(-(integrate((1/z),z)+integrate(-(1/(2*(-1+z))),z)+integrate(-(1/(2*(1+z))),z)),z,cos(y)))+try(subs(((integrate(-(1/(2*(-1+y))),y)+integrate(-(1/(2*(1+y))),y))+integrate((1/y),y)),y,sin(x)),-integrate((1/(cos(x)*sin(x))),x),subs(-(integrate((1/y),y)+integrate(-(1/(2*(-1+y))),y)+integrate(-(1/(2*(1+y))),y)),y,cos(x)))+c1)=0
(try(-integrate((1/(cos(y)*sin(y))),y),subs(((-(integrate((1/(-1+z)),z)/2)-(integrate((1/(1+z)),z)/2))+integrate((1/z),z)),z,sin(y)),subs(-(integrate((1/z),z)-(integrate((1/(-1+z)),z)/2)-(integrate((1/(1+z)),z)/2)),z,cos(y)))+try(subs(((-(integrate((1/(-1+y)),y)/2)-(integrate((1/(1+y)),y)/2))+integrate((1/y),y)),y,sin(x)),-integrate((1/(cos(x)*sin(x))),x),subs(-(integrate((1/y),y)-(integrate((1/(-1+y)),y)/2)-(integrate((1/(1+y)),y)/2)),y,cos(x)))+c1)=0
(try(-integrate((1/(cos(y)*sin(y))),y),subs(((-(log(abs((-1+z)))/2)-(log(abs((1+z)))/2))+log(abs(z))),z,sin(y)),subs(-(log(abs(z))-(log(abs((-1+z)))/2)-(log(abs((1+z)))/2)),z,cos(y)))+try(subs(((-(log(abs((-1+y)))/2)-(log(abs((1+y)))/2))+log(abs(y))),y,sin(x)),-integrate((1/(cos(x)*sin(x))),x),subs(-(log(abs(y))-(log(abs((-1+y)))/2)-(log(abs((1+y)))/2)),y,cos(x)))+c1)=0
(((-(log(abs((-1+sin(x))))/2)-(log(abs((1+sin(x))))/2))+log(abs(sin(x))))+((-(log(abs((-1+sin(y))))/2)-(log(abs((1+sin(y))))/2))+log(abs(sin(y))))+c1)=0

===========
question 2
===========
( CLASS 11 NCERT MATHS CHAPTER 1 MISCELLANEOUS EXERCISE, 6TH QUESTION, SECTION  5 )
solution with steps:

x^2*dif(y,x)=x^2-2*y^2+x*y
(dif(y,x)*(x^2))=((x*y)+((x^2)-(2*(y^2))))
((2*(y^2))+(dif(y,x)*(x^2))-(x*y)-(x^2))=0
try(subs(((integrate(-(1/x),x)+c1)+integrate(-(1/(-1+(2*(z^2)))),z)),z,(y/x)))=0
try(subs(((-integrate((1/x),x)+c1)-integrate((1/(-1+(2*(z^2)))),z)),z,(y/x)))=0
try(subs(((-log(abs(x))+c1)-integrate((1/(-1+(2*(z^2)))),z)),z,(y/x)))=0
try(subs((-integrate((1/(-1+(2*(z^2)))),z)-log(abs(x))+c1),z,(y/x)))=0
try(subs((-integrate((1/(2*((sqrt(2)/2)+z)*(-(sqrt(2)/2)+z))),z)-log(abs(x))+c1),z,(y/x)))=0
try(subs((-integrate((1/(2*((sqrt(2)/2)+z)*(-(sqrt(2)/2)+z))),z)-log(abs(x))+c1),z,(y/x)))=0
try(subs((-integrate((1/(2*((sqrt(2)/2)+z)*(-(sqrt(2)/2)+z))),z)-log(abs(x))+c1),z,(y/x)))=0
try(subs((-(integrate((1/(((sqrt(2)/2)+z)*(-(sqrt(2)/2)+z))),z)/2)-log(abs(x))+c1),z,(y/x)))=0
try(subs((-(integrate((((-1+((sqrt(2)*x)/2))/((sqrt(2)/2)+z))+(x/(-(sqrt(2)/2)+z))),z)/2)-log(abs(x))+c1),z,(y/x)))=0
try(subs((-((integrate(((-1+((sqrt(2)*x)/2))/((sqrt(2)/2)+z)),z)+integrate((x/(-(sqrt(2)/2)+z)),z))/2)-log(abs(x))+c1),z,(y/x)))=0
try(subs((-((((-1+((sqrt(2)*x)/2))*integrate((1/((sqrt(2)/2)+z)),z))+(integrate((1/(-(sqrt(2)/2)+z)),z)*x))/2)-log(abs(x))+c1),z,(y/x)))=0
try(subs((-((((-1+((sqrt(2)*x)/2))*log(abs(((sqrt(2)/2)+z))))+(log(abs((-(sqrt(2)/2)+z)))*x))/2)-log(abs(x))+c1),z,(y/x)))=0
(-((((-1+((sqrt(2)*x)/2))*log(abs(((sqrt(2)/2)+(y/x)))))+(log(abs(((y/x)-(sqrt(2)/2))))*x))/2)-log(abs(x))+c1)=0

===========
question 3
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 9 EXERCISE 9.4, QUESTION  8 )
solution with steps:

x*dif(y,x)-y+x*sin(y/x)=0
((sin((y/x))*x)+((dif(y,x)*x)-y))=0
((dif(y,x)*x)+(sin((y/x))*x)-y)=0
try(subs(((integrate(-(1/x),x)+c1)+integrate(-(1/sin(z)),z)),z,(y/x)))=0
try(subs(((-integrate((1/x),x)+c1)-integrate((1/sin(z)),z)),z,(y/x)))=0
try(subs(((-log(abs(x))+c1)-log(abs(tan((z/2))))),z,(y/x)))=0
((-log(abs(x))+c1)-log(abs(tan((y/(2*x))))))=0

===========
question 4
===========
( CLASS 11 NCERT MATHS CHAPTER 1 MISCELLANEOUS EXERCISE, 6TH QUESTION, SECTION  1 )
solution with steps:

A<->((A&B)|(A-B))
A<->((A&B)|(A-B))
A<->((~B&A)|(A&B))
true

===========
question 5
===========
( CLASS 11 NCERT MATHS CHAPTER 1 MISCELLANEOUS EXERCISE, 6TH QUESTION, SECTION  2 )
solution with steps:

A|(B-A)<->(A|B)
((B-A)|A)<->(A|B)
((~A&B)|A)<->(A|B)
true

===========
question 6
===========
( CLASS 10 NCERT MATHS CHAPTER 8 EXERCISE 8.3, 4TH QUESTION, SECTION  1 )
solution with steps:

(cosec(x)-cot(x))^2 = (1-cos(x))/(1+cos(x))
((cosec(x)-cot(x))^2)=((1-cos(x))/(1+cos(x)))
(-((1-cos(x))/(1+cos(x)))+((cosec(x)-cot(x))^2))=0
(-((1-cos(x))/(1+cos(x)))+(((1/sin(x))-(cos(x)/sin(x)))^2))=0
((cos(x)*(sin(x)^8))+((cos(x)^3)*(sin(x)^6))-(cos(x)*(sin(x)^6))-((cos(x)^2)*(sin(x)^6))-(sin(x)^8)+(sin(x)^6))=0
0=0
true

===========
question 7
===========
( CLASS 10 NCERT MATHS CHAPTER 8 EXERCISE 8.3, 4TH QUESTION, SECTION  2 )
solution with steps:

cos(x)/(1+sin(x)) + (1+sin(x))/cos(x) = 2*sec(x)
(((1+sin(x))/cos(x))+(cos(x)/(1+sin(x))))=(2*sec(x))
(((1+sin(x))/cos(x))+(cos(x)/(1+sin(x)))-(2*sec(x)))=0
(((1+sin(x))/cos(x))+(cos(x)/(1+sin(x)))-(2/cos(x)))=0
((cos(x)*(sin(x)^2))-cos(x)+(cos(x)^3))=0
0=0
true

===========
question 8
===========
( CLASS 10 NCERT MATHS CHAPTER 8 EXERCISE 8.3, 4TH QUESTION, SECTION  3 )
solution with steps:

tan(x)/(1-cot(x)) + cot(x)/(1-tan(x)) = 1 + sec(x)*cosec(x)
((cot(x)/(1-tan(x)))+(tan(x)/(1-cot(x))))=(1+(cosec(x)*sec(x)))
(-1+(cot(x)/(1-tan(x)))+(tan(x)/(1-cot(x)))-(cosec(x)*sec(x)))=0
(-1+((cos(x)/sin(x))/(1-(sin(x)/cos(x))))+((sin(x)/cos(x))/(1-(cos(x)/sin(x))))-(1/(cos(x)*sin(x))))=0
((2*cos(x)*(sin(x)^3))+(2*(cos(x)^3)*sin(x))-(2*cos(x)*sin(x))-(2*(cos(x)^2)*(sin(x)^2))-(cos(x)^4)-(sin(x)^4)+(cos(x)^2)+(sin(x)^2))=0
0=0
true

===========
question 9
===========
( CLASS 10 NCERT MATHS CHAPTER 8 EXERCISE 8.3, 4TH QUESTION, SECTION  4 )
solution with steps:

(1+sec(x))/sec(x) = sin(x)^2/(1-cos(x))
((1+sec(x))/sec(x))=((sin(x)^2)/(1-cos(x)))
(((1+sec(x))/sec(x))-((sin(x)^2)/(1-cos(x))))=0
(((1+(1/cos(x)))/(1/cos(x)))-((sin(x)^2)/(1-cos(x))))=0
(1-(cos(x)^2)-(sin(x)^2))=0
0=0
true

===========
question 10
===========
( CLASS 10 NCERT MATHS CHAPTER 8 EXERCISE 8.3, 4TH QUESTION, SECTION  5 )
solution with steps:

(cos(x)-sin(x)+1)/(cos(x)+sin(x)-1) = cosec(x)+cot(x)
((1+(cos(x)-sin(x)))/((cos(x)+sin(x))-1))=(cosec(x)+cot(x))
(((1+cos(x)-sin(x))/(-1+cos(x)+sin(x)))-cosec(x)-cot(x))=0
(((1+cos(x)-sin(x))/(-1+cos(x)+sin(x)))-(1/sin(x))-(cos(x)/sin(x)))=0
(-((cos(x)^2)*sin(x))-(sin(x)^3)+sin(x))=0
0=0
true

===========
question 11
===========
( CLASS 10 NCERT MATHS CHAPTER 8 EXERCISE 8.3, 4TH QUESTION, SECTION  7 )
solution with steps:

(sin(x)-2*sin(x)^3)/(2*cos(x)^3-cos(x))=tan(x)
((sin(x)-(2*(sin(x)^3)))/((2*(cos(x)^3))-cos(x)))=tan(x)
(((-(2*(sin(x)^3))+sin(x))/((2*(cos(x)^3))-cos(x)))-tan(x))=0
(((-(2*(sin(x)^3))+sin(x))/((2*(cos(x)^3))-cos(x)))-(sin(x)/cos(x)))=0
((2*cos(x)*sin(x))-(2*cos(x)*(sin(x)^3))-(2*(cos(x)^3)*sin(x)))=0
0=0
true

===========
question 12
===========
( CLASS 10 NCERT MATHS CHAPTER 8 EXERCISE 8.3, 4TH QUESTION, SECTION  8 )
solution with steps:

(sin(x)+cosec(x))^2 + (cos(x)+sec(x))^2 = 7+tan(x)^2+cot(x)^2
(((cos(x)+sec(x))^2)+((cosec(x)+sin(x))^2))=((7+(tan(x)^2))+(cot(x)^2))
(-7-(cot(x)^2)-(tan(x)^2)+((cos(x)+sec(x))^2)+((cosec(x)+sin(x))^2))=0
(-7-((cos(x)/sin(x))^2)-((sin(x)/cos(x))^2)+((cos(x)+(1/cos(x)))^2)+(((1/sin(x))+sin(x))^2))=0
(((cos(x)^2)*(sin(x)^4))+((cos(x)^4)*(sin(x)^2))+((cos(x)^4)*(sin(x)^6))+((cos(x)^6)*(sin(x)^4))-(3*(cos(x)^4)*(sin(x)^4))-((cos(x)^2)*(sin(x)^6))-((cos(x)^6)*(sin(x)^2)))=0
0=0
true

===========
question 13
===========
( CLASS 10 NCERT MATHS CHAPTER 8 EXERCISE 8.3, 4TH QUESTION, SECTION  9 )
solution with steps:

(cosec(x)-sin(x))*(sec(x)-cos(x)) = 1/(tan(x)+cot(x))
((cosec(x)-sin(x))*(sec(x)-cos(x)))=(1/(cot(x)+tan(x)))
(((cosec(x)-sin(x))*(-cos(x)+sec(x)))-(1/(cot(x)+tan(x))))=0
((((1/cos(x))-cos(x))*((1/sin(x))-sin(x)))-(1/((cos(x)/sin(x))+(sin(x)/cos(x)))))=0
((cos(x)*(sin(x)^3))+((cos(x)^3)*(sin(x)^5))+((cos(x)^3)*sin(x))+((cos(x)^5)*(sin(x)^3))-(3*(cos(x)^3)*(sin(x)^3))-(cos(x)*(sin(x)^5))-((cos(x)^5)*sin(x)))=0
0=0
true

===========
question 14
===========
( CLASS 11 NCERT MATHS CHAPTER 5 EXERCISE 5.1, QUESTION  1 )
solution with steps:

4*x+3 < 5*x+7
(3+(4*x))<(7+(5*x))
(-4-x)<0
(-4,+inf)

===========
question 15
===========
( CLASS 11 NCERT MATHS CHAPTER 5 EXERCISE 5.1, QUESTION  20 )
solution with steps:

x/2 >= (5*x-2)/3 - (7*x - 3)/5
(x/2)>=((((5*x)-2)/3)-(((7*x)-3)/5))
(((-3+(7*x))/5)+((2-(5*x))/3)+(x/2))>=0
((2+(7*x))/30)>=0
(~(((2+(7*x))/30)=0)&~(((2+(7*x))/30)<0))|(((2+(7*x))/30)=0)
(~((2+(7*x))=0)&~((2+(7*x))<0))|((2+(7*x))=0)
(-(2/7),+inf)U{-(2/7)}

===========
question 16
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 9 EXERCISE 9.5, QUESTION  10 )
solution with steps:

(x+y)*dif(y,x) = 1
((x+y)*dif(y,x))=1
(-1+((x+y)*dif(y,x)))=0
(((e^integrate(-1,y))*x)-integrate(((e^integrate(-1,y))*y),y)-c1)=0
(((e^-y)*x)-integrate(((e^-y)*y),y)-c1)=0
(((e^-y)*x)-((integrate((e^-y),y)*y)-integrate(integrate((e^-y),y),y))-c1)=0
(integrate(integrate((e^-y),y),y)+((e^-y)*x)-(integrate((e^-y),y)*y)-c1)=0
(integrate(-(e^-y),y)+((e^-y)*x)--((e^-y)*y)-c1)=0
(integrate(-(e^-y),y)+((e^-y)*x)+((e^-y)*y)-c1)=0
(((e^-y)*x)+((e^-y)*y)-integrate((e^-y),y)-c1)=0
(((e^-y)*x)+((e^-y)*y)--(e^-y)-c1)=0

===========
question 17
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 9 EXERCISE 9.3, QUESTION  6 )
solution with steps:

dif(y,x)=(1+x^2)*(1+y^2)
dif(y,x)=((1+(x^2))*(1+(y^2)))
(dif(y,x)-((1+(x^2))*(1+(y^2))))=0
(dif(y,x)+((-1-(x^2))*(1+(y^2))))=0
((integrate(-(1/(1+(y^2))),y)+c1)+integrate((1+(x^2)),x))=0
((-integrate((1/(1+(y^2))),y)+c1)+integrate((1+(x^2)),x))=0
(integrate((1+(x^2)),x)-integrate((1/(1+(y^2))),y)+c1)=0
(integrate((1+(x^2)),x)-arctan(y)+c1)=0
((integrate(1,x)+integrate((x^2),x))-arctan(y)+c1)=0
((((x^3)/3)+(1*x))-arctan(y)+c1)=0

===========
question 18
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 9 EXERCISE 9.3, QUESTION  12 )
solution with steps:

x*(x^2 - 1)*dif(y,x)=1
(dif(y,x)*(((x^2)-1)*x))=1
(-1+((-1+(x^2))*dif(y,x)*x))=0
((integrate(-1,y)+c1)+integrate((1/((-1+(x^2))*x)),x))=0
((-y+c1)+integrate((1/((-1+(x^2))*x)),x))=0
(integrate((1/((-1+(x^2))*x)),x)-y+c1)=0
(integrate((1/((-1+x)*(1+x)*x)),x)-y+c1)=0
(integrate(((1/x)-(1/(2*(-1+x)))-(1/(2*(1+x)))),x)-y+c1)=0
(((integrate(-(1/(2*(-1+x))),x)+integrate(-(1/(2*(1+x))),x))+integrate((1/x),x))-y+c1)=0
(((integrate(-(1/(2*(-1+x))),x)+integrate(-(1/(2*(1+x))),x))+log(abs(x)))-y+c1)=0
(((-(integrate((1/(-1+x)),x)/2)-(integrate((1/(1+x)),x)/2))+log(abs(x)))-y+c1)=0
(((-(log(abs((-1+x)))/2)-(log(abs((1+x)))/2))+log(abs(x)))-y+c1)=0

===========
question 19
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 9 EXERCISE 9.4, QUESTION  1 )
solution with steps:

(x^2+x*y)*dif(y,x) = (x^2 + y^2)
(((x*y)+(x^2))*dif(y,x))=((x^2)+(y^2))
((((x*y)+(x^2))*dif(y,x))-(x^2)-(y^2))=0
try(subs(((integrate(((1+z)/(-1+z)),z)+c1)+integrate((1/x),x)),z,(y/x)))=0
try(subs(((integrate(((1+z)/(-1+z)),z)+c1)+log(abs(x))),z,(y/x)))=0
try(subs((integrate(((1+z)/(-1+z)),z)+log(abs(x))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(integrate(((1+z)/(-1+z)),z),subs(integrate(((2+a)/a),a),a,(-1+z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(integrate(((1/(-1+z))+(z/(-1+z))),z),subs(integrate(((2/a)+(a/a)),a),a,(-1+z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(integrate(((1/(-1+z))+(z/(-1+z))),z),subs(integrate((1+(2/a)),a),a,(-1+z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try((integrate((1/(-1+z)),z)+integrate((z/(-1+z)),z)),subs((integrate(1,a)+integrate((2/a),a)),a,(-1+z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try((integrate((1/(-1+z)),z)+integrate((z/(-1+z)),z)),subs((integrate(1,a)+(2*integrate((1/a),a))),a,(-1+z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try((integrate((z/(-1+z)),z)+log(abs((-1+z)))),subs(((1*a)+(2*log(abs(a)))),a,(-1+z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try((integrate((z/(-1+z)),z)+log(abs((-1+z)))),subs(((2*log(abs(a)))+a),a,(-1+z)))+c1),z,(y/x)))=0
(((-1+(y/x))+(2*log(abs((-1+(y/x))))))+log(abs(x))+c1)=0

===========
question 20
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 9 EXERCISE 9.4, QUESTION  2 )
solution with steps:

dif(y,x)=(x+y)/x
dif(y,x)=((x+y)/x)
(dif(y,x)-((x+y)/x))=0
(((dif(y,x)*x)-x-y)/x)=0
try(subs(((integrate(-1,z)+c1)+integrate((1/x),x)),z,(y/x)))=0
try(subs(((-z+c1)+log(abs(x))),z,(y/x)))=0
((-(y/x)+c1)+log(abs(x)))=0

===========
question 21
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 9 EXERCISE 9.4, QUESTION  3 )
solution with steps:

(x-y)*dif(y,x)-(x+y)=0
((dif(y,x)*(x-y))-(x+y))=0
(((-y+x)*dif(y,x))-x-y)=0
try(subs(((integrate(((1-z)/(-1-(z^2))),z)+c1)+integrate((1/x),x)),z,(y/x)))=0
try(subs(((integrate(((1-z)/(-1-(z^2))),z)+c1)+log(abs(x))),z,(y/x)))=0
try(subs((integrate(((1-z)/(-1-(z^2))),z)+log(abs(x))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs(integrate((((1-sqrt(a))*(1/sqrt(a)))/(2*(-1-a))),a),a,(z^2)),integrate(((1-z)/(-1-(z^2))),z))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs((integrate((((1-sqrt(a))*(1/sqrt(a)))/(-1-a)),a)/2),a,(z^2)),integrate(((1-z)/(-1-(z^2))),z))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs((integrate((((1-sqrt(a))*(1/sqrt(a)))/(-1-a)),a)/2),a,(z^2)),integrate(((1-z)/(-1-(z^2))),z))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs((integrate((((1-sqrt(a))*(1/sqrt(a)))/(-1-a)),a)/2),a,(z^2)),((log(abs((-1-(z^2))))/2)-arctan(z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs((integrate((((1/sqrt(a))/(-1-a))-(1/(-1-a))),a)/2),a,(z*z)),((log(abs((-1-(z^2))))/2)-arctan(z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs((integrate((((1/sqrt(a))/(-1-a))-(1/(-1-a))),a)/2),a,(z^2)),((log(abs((-1-(z^2))))/2)-arctan(z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs(((integrate(((1/sqrt(a))/(-1-a)),a)+integrate(-(1/(-1-a)),a))/2),a,(z^2)),((log(abs((-1-(z^2))))/2)-arctan(z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs(((integrate(((1/sqrt(a))/(-1-a)),a)-integrate((1/(-1-a)),a))/2),a,(z^2)),((log(abs((-1-(z^2))))/2)-arctan(z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs(((integrate(((1/sqrt(a))/(-1-a)),a)+log(abs((-1-a))))/2),a,(z^2)),((log(abs((-1-(z^2))))/2)-arctan(z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs(((integrate(((1/sqrt(a))/(-1-a)),a)+log(abs((-1-a))))/2),a,(z^2)),((log(abs((-1-(z^2))))/2)-arctan(z)))+c1),z,(y/x)))=0
(((log(abs((-1-((y/x)^2))))/2)-arctan((y/x)))+log(abs(x))+c1)=0

===========
question 22
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 9 EXERCISE 9.4, QUESTION  4 )
solution with steps:

(x^2-y^2)+2*x*y*dif(y,x)=0
((dif(y,x)*((2*x)*y))+((x^2)-(y^2)))=0
((2*dif(y,x)*x*y)-(y^2)+(x^2))=0
try(subs(((integrate(-(1/(2*x)),x)+c1)+integrate((-(1/(1+(z^2)))*z),z)),z,(y/x)))=0
try(subs(((-(integrate((1/x),x)/2)+c1)+integrate((-(1/(1+(z^2)))*z),z)),z,(y/x)))=0
try(subs(((-(log(abs(x))/2)+c1)+integrate((-(1/(1+(z^2)))*z),z)),z,(y/x)))=0
try(subs((integrate(-(z/(1+(z^2))),z)-(log(abs(x))/2)+c1),z,(y/x)))=0
try(subs((-(log(abs(x))/2)+try(integrate(-(z/(1+(z^2))),z),subs(integrate(-(1/(2*(1+a))),a),a,(z^2)))+c1),z,(y/x)))=0
try(subs((((2*try(integrate(-(z/(1+(z^2))),z),subs(integrate(-(1/(2*(1+a))),a),a,(z^2))))+(2*c1)-log(abs(x)))/2),z,(y/x)))=0
try(subs((((2*try(-integrate((z/(1+(z^2))),z),subs(-(integrate((1/(1+a)),a)/2),a,(z^2))))+(2*c1)-log(abs(x)))/2),z,(y/x)))=0
try(subs((((2*try(-integrate((z/(1+(z^2))),z),subs(-(integrate((1/(1+a)),a)/2),a,(z^2))))+(2*c1)-log(abs(x)))/2),z,(y/x)))=0
try(subs((((2*try(-(log(abs((1+(z^2))))/2),subs(-(integrate((1/(1+a)),a)/2),a,(z^2))))+(2*c1)-log(abs(x)))/2),z,(y/x)))=0
try(subs((((2*try(-(log(abs((1+(z^2))))/2),subs(-(integrate((1/(1+a)),a)/2),a,(z^2))))/2)+((2*c1)/2)-(log(abs(x))/2)),z,(y/x)))=0
try(subs((-(log(abs(x))/2)+try(-(log(abs((1+(z^2))))/2),subs(-(integrate((1/(1+a)),a)/2),a,(z^2)))+c1),z,(y/x)))=0
try(subs((-(log(abs(x))/2)+try(-(log(abs((1+(z^2))))/2),subs(-(log(abs((1+a)))/2),a,(z^2)))+c1),z,(y/x)))=0
(-(log(abs((1+((y/x)^2))))/2)-(log(abs(x))/2)+c1)=0

===========
question 23
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  1 )
solution with steps:

integrate(x/((x+1)*(x+2)),x)
integrate((x/((1+x)*(2+x))),x)
integrate((x/((1+x)*(2+x))),x)
integrate(((1/(1+x))-(2/(2+x))),x)
integrate((1/(1+x)),x)+integrate(-(2/(2+x)),x)
integrate((1/(1+x)),x)-(2*integrate((1/(2+x)),x))
log(abs((1+x)))-(2*log(abs((2+x))))

===========
question 24
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  2 )
solution with steps:

integrate(1/(x^2-9),x)
integrate((1/((x^2)-9)),x)
integrate((1/((-3+x)*(3+x))),x)
integrate(((1/(6*(3+x)))-(1/(6*(-3+x)))),x)
integrate(((1/(6*(3+x)))-(1/(6*(-3+x)))),x)
integrate((1/(6*(3+x))),x)+integrate(-(1/(6*(-3+x))),x)
(integrate((1/(3+x)),x)/6)-(integrate((1/(-3+x)),x)/6)
(log(abs((3+x)))/6)-(log(abs((-3+x)))/6)

===========
question 25
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  3 )
solution with steps:

integrate((3*x-1)/((x-1)*(x-2)*(x-3)),x)
integrate((((3*x)-1)/(((x-1)*(x-2))*(x-3))),x)
integrate(((-1+(3*x))/((-1+x)*(-2+x)*(-3+x))),x)
integrate(((5/(-2+x))-(1/(-1+x))-(4/(-3+x))),x)
(integrate(-(1/(-1+x)),x)+integrate(-(4/(-3+x)),x))+integrate((5/(-2+x)),x)
(-integrate((1/(-1+x)),x)-(4*integrate((1/(-3+x)),x)))+(5*integrate((1/(-2+x)),x))
(-log(abs((-1+x)))-(4*log(abs((-3+x)))))+(5*log(abs((-2+x))))

===========
question 26
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  4 )
solution with steps:

integrate(x/((x-1)*(x-2)*(x-3)),x)
integrate((x/(((x-1)*(x-2))*(x-3))),x)
integrate((x/((-1+x)*(-2+x)*(-3+x))),x)
integrate(((2/(-2+x))-(1/(2*(-1+x)))-(3/(2*(-3+x)))),x)
integrate(((2/(-2+x))-(1/(2*(-1+x)))-(3/(2*(-3+x)))),x)
(integrate(-(1/(2*(-1+x))),x)+integrate(-(3/(2*(-3+x))),x))+integrate((2/(-2+x)),x)
(-(integrate((1/(-1+x)),x)/2)-((3*integrate((1/(-3+x)),x))/2))+(2*integrate((1/(-2+x)),x))
(-(log(abs((-1+x)))/2)-((3*log(abs((-3+x))))/2))+(2*log(abs((-2+x))))

===========
question 27
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  5 )
solution with steps:

integrate(2*x/(x^2+3*x+2),x)
integrate(((2*x)/(2+((3*x)+(x^2)))),x)
integrate(((2*x)/(2+(3*x)+(x^2))),x)
integrate(((2*x)/((1+x)*(2+x))),x)
integrate(((2/(1+x))-(4/(2+x))),x)
integrate((2/(1+x)),x)+integrate(-(4/(2+x)),x)
(2*integrate((1/(1+x)),x))-(4*integrate((1/(2+x)),x))
(2*log(abs((1+x))))-(4*log(abs((2+x))))

===========
question 28
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  6 )
solution with steps:

integrate((1-x^2)/(x*(1-2*x)),x)
integrate(((1-(x^2))/((1-(2*x))*x)),x)
integrate(((1-(x^2))/((1-(2*x))*x)),x)
integrate((((1-x)*(1+x))/((1-(2*x))*x)),x)
integrate((-(1/x)-(2/(1-(2*x)))),x)
integrate(-(1/x),x)+integrate(-(2/(1-(2*x))),x)
-integrate((1/x),x)-(2*integrate((1/(1-(2*x))),x))
-log(abs(x))--log(abs((1-(2*x))))

===========
question 29
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  7 )
solution with steps:

integrate(x/((x-1)*(x^2+1)),x)
integrate((x/((1+(x^2))*(x-1))),x)
integrate((x/((-1+x)*(1+(x^2)))),x)
integrate(((((x/2)-(1/2))/(1+(x^2)))-(1/(2*(-1+x)))),x)
integrate(((((x/2)-(1/2))/(1+(x^2)))-(1/(2*(-1+x)))),x)
integrate((((x/2)-(1/2))/(1+(x^2))),x)+integrate(-(1/(2*(-1+x))),x)
integrate((((x/2)-(1/2))/(1+(x^2))),x)-(integrate((1/(-1+x)),x)/2)
integrate((((x/2)-(1/2))/(1+(x^2))),x)-(log(abs((-1+x)))/2)
((2*integrate(((x/(2*(1+(x^2))))-(1/(2*(1+(x^2))))),x))-log(abs((-1+x))))/2
((2*integrate(((x/(2*(1+(x^2))))-(1/(2*(1+(x^2))))),x))/2)-(log(abs((-1+x)))/2)
integrate(((x/(2*(1+(x^2))))-(1/(2*(1+(x^2))))),x)-(log(abs((-1+x)))/2)
(integrate((x/(2*(1+(x^2)))),x)+integrate(-(1/(2*(1+(x^2)))),x))-(log(abs((-1+x)))/2)
((integrate((x/(1+(x^2))),x)/2)-(integrate((1/(1+(x^2))),x)/2))-(log(abs((-1+x)))/2)
(integrate((x/(1+(x^2))),x)/2)-(integrate((1/(1+(x^2))),x)/2)-(log(abs((-1+x)))/2)
(log(abs((1+(x^2))))/4)-(arctan(x)/2)-(log(abs((-1+x)))/2)

===========
question 30
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  8 )
solution with steps:

integrate(x/((x-1)^2*(x+2)),x)
integrate((x/((2+x)*((x-1)^2))),x)
integrate((x/((2+x)*((-1+x)^2))),x)
integrate(((2/(9*(2+x)))-(1/(3*((-1+x)^2)))-(2/(9*(-1+x)))),x)
integrate(((2/(9*(2+x)))-(1/(3*((-1+x)^2)))-(2/(9*(-1+x)))),x)
(integrate(-(1/(3*((-1+x)^2))),x)+integrate(-(2/(9*(-1+x))),x))+integrate((2/(9*(2+x))),x)
(-(integrate((1/((-1+x)^2)),x)/3)-((2*integrate((1/(-1+x)),x))/9))+((2/9)*integrate((1/(2+x)),x))
(-((2*log(abs((-1+x))))/9)--(1/(3*(-1+x))))+((2/9)*log(abs((2+x))))

===========
question 31
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  10 )
solution with steps:

integrate((2*x-3)/((x^2-1)*(2*x+3)),x)
integrate((((2*x)-3)/((3+(2*x))*((x^2)-1))),x)
integrate(((-3+(2*x))/((-1+(x^2))*(3+(2*x)))),x)
integrate(((-3+(2*x))/((-1+x)*(1+x)*(3+(2*x)))),x)
integrate(((1/(10*(-1+x)))+(24/(5*(3+(2*x))))-(5/(2*(1+x)))),x)
integrate(((1/(10*(-1+x)))+(24/(5*(3+(2*x))))-(5/(2*(1+x)))),x)
(integrate((24/(5*(3+(2*x)))),x)+integrate(-(5/(2*(1+x))),x))+integrate((1/(10*(-1+x))),x)
(((24/5)*integrate((1/(3+(2*x))),x))-((5*integrate((1/(1+x)),x))/2))+(integrate((1/(-1+x)),x)/10)
(((24/5)*(log(abs((3+(2*x))))/2))-((5*log(abs((1+x))))/2))+(log(abs((-1+x)))/10)

===========
question 32
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  11 )
solution with steps:

integrate(5*x/((x+1)*(x^2-4)),x)
integrate(((5*x)/((1+x)*((x^2)-4))),x)
integrate(((5*x)/((-4+(x^2))*(1+x))),x)
integrate(((5*x)/((-2+x)*(1+x)*(2+x))),x)
integrate(((5/(2*(2+x)))-(5/(3*(1+x)))-(5/(6*(-2+x)))),x)
integrate(((5/(2*(2+x)))-(5/(3*(1+x)))-(5/(6*(-2+x)))),x)
(integrate(-(5/(3*(1+x))),x)+integrate(-(5/(6*(-2+x))),x))+integrate((5/(2*(2+x))),x)
(-((5*integrate((1/(-2+x)),x))/6)-((5*integrate((1/(1+x)),x))/3))+((5/2)*integrate((1/(2+x)),x))
(-((5*log(abs((-2+x))))/6)-((5*log(abs((1+x))))/3))+((5/2)*log(abs((2+x))))

===========
question 33
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  13 )
solution with steps:

integrate(2/((1-x)*(1+x^2)),x)
integrate((2/((1+(x^2))*(1-x))),x)
integrate((2/((1-x)*(1+(x^2)))),x)
integrate((((-1-x)/(1+(x^2)))-(1/(1-x))),x)
integrate(((-1-x)/(1+(x^2))),x)+integrate(-(1/(1-x)),x)
integrate(((-1-x)/(1+(x^2))),x)-integrate((1/(1-x)),x)
integrate(((-1-x)/(1+(x^2))),x)--log(abs((1-x)))
integrate(((-1-x)/(1+(x^2))),x)+log(abs((1-x)))
integrate((-(1/(1+(x^2)))-(x/(1+(x^2)))),x)+log(abs((1-x)))
integrate((-(1/(1+(x^2)))-(x/(1+(x^2)))),x)+log(abs((1-x)))
(integrate(-(1/(1+(x^2))),x)+integrate(-(x/(1+(x^2))),x))+log(abs((1-x)))
(-integrate((1/(1+(x^2))),x)-integrate((x/(1+(x^2))),x))+log(abs((1-x)))
log(abs((1-x)))-integrate((1/(1+(x^2))),x)-integrate((x/(1+(x^2))),x)
log(abs((1-x)))-arctan(x)-(log(abs((1+(x^2))))/2)

===========
question 34
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.3, QUESTION  1 )
solution with steps:

integrate(sin(2*x+5)^2,x)
integrate((sin((5+(2*x)))^2),x)
integrate(((1/2)-(cos((10+(4*x)))/2)),x)
integrate(((2-(2*cos((10+(4*x)))))/4),x)
integrate((2-(2*cos((10+(4*x))))),x)/4
(integrate(2,x)+integrate(-(2*cos((10+(4*x)))),x))/4
(integrate(2,x)-(2*integrate(cos((10+(4*x))),x)))/4
((2*x)-(sin((10+(4*x)))/2))/4

===========
question 35
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.3, QUESTION  2 )
solution with steps:

integrate(sin(3*x)*cos(4*x),x)
integrate((cos((4*x))*sin((3*x))),x)
integrate(((sin((7*x))/2)-(sin(x)/2)),x)
integrate((((2*sin((7*x)))-(2*sin(x)))/4),x)
integrate(((2*sin((7*x)))-(2*sin(x))),x)/4
(integrate((2*sin((7*x))),x)+integrate(-(2*sin(x)),x))/4
((2*integrate(sin((7*x)),x))-(2*integrate(sin(x),x)))/4
((2*cos(x))-((2*cos((7*x)))/7))/4

===========
question 36
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.3, QUESTION  3 )
solution with steps:

integrate(cos(2*x)*cos(4*x)*cos(6*x),x)
integrate((cos((6*x))*(cos((2*x))*cos((4*x)))),x)
integrate(((1/4)+(cos((12*x))/4)+(cos((4*x))/4)+(cos((8*x))/4)),x)
integrate(((64+(64*cos((12*x)))+(64*cos((4*x)))+(64*cos((8*x))))/256),x)
integrate((64+(64*cos((12*x)))+(64*cos((4*x)))+(64*cos((8*x)))),x)/256
(integrate(64,x)+integrate((64*cos((12*x))),x)+integrate((64*cos((4*x))),x)+integrate((64*cos((8*x))),x))/256
(integrate(64,x)+(64*integrate(cos((12*x)),x))+(64*integrate(cos((4*x)),x))+(64*integrate(cos((8*x)),x)))/256
(((16*sin((12*x)))/3)+(16*sin((4*x)))+(64*x)+(8*sin((8*x))))/256

===========
question 37
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.3, QUESTION  4 )
solution with steps:

integrate(sin(2*x+1)^3,x)
integrate((sin((1+(2*x)))^3),x)
integrate(((sin((1+(2*x)))/2)-(sin((-1-(2*x)))/4)-(sin((3+(6*x)))/4)),x)
integrate((((16*sin((1+(2*x))))-(8*sin((-1-(2*x))))-(8*sin((3+(6*x)))))/32),x)
integrate(((16*sin((1+(2*x))))-(8*sin((-1-(2*x))))-(8*sin((3+(6*x))))),x)/32
(integrate((16*sin((1+(2*x)))),x)+integrate(-(8*sin((-1-(2*x)))),x)+integrate(-(8*sin((3+(6*x)))),x))/32
((16*integrate(sin((1+(2*x))),x))-(8*integrate(sin((-1-(2*x))),x))-(8*integrate(sin((3+(6*x))),x)))/32
(((4*cos((3+(6*x))))/3)-(4*cos((-1-(2*x))))-(8*cos((1+(2*x)))))/32

===========
question 38
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.3, QUESTION  5 )
solution with steps:

integrate(sin(x)^3*cos(x)^3,x)
integrate(((cos(x)^3)*(sin(x)^3)),x)
integrate((((3*sin((2*x)))/32)-(sin((6*x))/32)),x)
integrate((((96*sin((2*x)))-(32*sin((6*x))))/1024),x)
integrate(((96*sin((2*x)))-(32*sin((6*x)))),x)/1024
(integrate((96*sin((2*x))),x)+integrate(-(32*sin((6*x))),x))/1024
((96*integrate(sin((2*x)),x))-(32*integrate(sin((6*x)),x)))/1024
(((16*cos((6*x)))/3)-(48*cos((2*x))))/1024

===========
question 39
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.3, QUESTION  6 )
solution with steps:

integrate(sin(x)*sin(2*x)*sin(3*x),x)
integrate(((sin((2*x))*sin(x))*sin((3*x))),x)
integrate(((sin((2*x))/4)+(sin((4*x))/4)-(sin((6*x))/4)),x)
integrate((((16*sin((2*x)))+(16*sin((4*x)))-(16*sin((6*x))))/64),x)
integrate(((16*sin((2*x)))+(16*sin((4*x)))-(16*sin((6*x)))),x)/64
(integrate((16*sin((2*x))),x)+integrate((16*sin((4*x))),x)+integrate(-(16*sin((6*x))),x))/64
((16*integrate(sin((2*x)),x))+(16*integrate(sin((4*x)),x))-(16*integrate(sin((6*x)),x)))/64
(((8*cos((6*x)))/3)-(4*cos((4*x)))-(8*cos((2*x))))/64

===========
question 40
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.3, QUESTION  7 )
solution with steps:

integrate(sin(4*x)*sin(8*x),x)
integrate((sin((4*x))*sin((8*x))),x)
integrate(((cos((4*x))/2)-(cos((12*x))/2)),x)
integrate((((2*cos((4*x)))-(2*cos((12*x))))/4),x)
integrate(((2*cos((4*x)))-(2*cos((12*x)))),x)/4
(integrate((2*cos((4*x))),x)+integrate(-(2*cos((12*x))),x))/4
((2*integrate(cos((4*x)),x))-(2*integrate(cos((12*x)),x)))/4
((sin((4*x))/2)-(sin((12*x))/6))/4

===========
question 41
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.3, QUESTION  10 )
solution with steps:

integrate(sin(x)^4,x)
integrate((sin(x)^4),x)
integrate(((3/8)+(cos((4*x))/8)-(cos((2*x))/2)),x)
integrate(((48+(16*cos((4*x)))-(64*cos((2*x))))/128),x)
integrate((48+(16*cos((4*x)))-(64*cos((2*x)))),x)/128
(integrate(48,x)+integrate((16*cos((4*x))),x)+integrate(-(64*cos((2*x))),x))/128
(integrate(48,x)+(16*integrate(cos((4*x)),x))-(64*integrate(cos((2*x)),x)))/128
((4*sin((4*x)))+(48*x)-(32*sin((2*x))))/128

===========
question 42
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.3, QUESTION  11 )
solution with steps:

integrate(cos(2*x)^4,x)
integrate((cos((2*x))^4),x)
integrate(((3/8)+(cos((4*x))/2)+(cos((8*x))/8)),x)
integrate(((48+(16*cos((8*x)))+(64*cos((4*x))))/128),x)
integrate((48+(16*cos((8*x)))+(64*cos((4*x)))),x)/128
(integrate(48,x)+integrate((16*cos((8*x))),x)+integrate((64*cos((4*x))),x))/128
(integrate(48,x)+(16*integrate(cos((8*x)),x))+(64*integrate(cos((4*x)),x)))/128
((16*sin((4*x)))+(2*sin((8*x)))+(48*x))/128

===========
question 43
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.2, QUESTION  1 )
solution with steps:

integrate(2*x/(1+x^2),x)
integrate(((2*x)/(1+(x^2))),x)
integrate(((2*x)/(1+(x^2))),x)
2*integrate((x/(1+(x^2))),x)
2*try(subs(integrate((1/(2*(1+y))),y),y,(x^2)),integrate((x/(1+(x^2))),x))
2*try(subs((integrate((1/(1+y)),y)/2),y,(x^2)),integrate((x/(1+(x^2))),x))
2*try(subs((log(abs((1+y)))/2),y,(x^2)),integrate((x/(1+(x^2))),x))
2*try(subs((log(abs((1+y)))/2),y,(x*x)),integrate((x/(1+(x^2))),x))
2*try(subs((log(abs((1+y)))/2),y,(x^2)),integrate((x/(1+(x^2))),x))
2*(log(abs((1+(x^2))))/2)

===========
question 44
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.2, QUESTION  6 )
solution with steps:

integrate(sqrt(a*x+b),x)
integrate(sqrt(((x*a)+b)),x)
(2*(((x*a)+b)^(3/2)))/(3*a)

===========
question 45
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.2, QUESTION  7 )
solution with steps:

integrate(x*sqrt(x),x)
integrate((sqrt(x)*x),x)
(2*(x^(5/2)))/5

===========
question 46
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.2, QUESTION  8 )
solution with steps:

integrate(x*sqrt(1+2*x^2),x)
integrate((sqrt((1+(2*(x^2))))*x),x)
try(subs(integrate((sqrt((1+(2*y)))/2),y),y,(x^2)),integrate((sqrt((1+(2*(x^2))))*x),x))
try(subs((integrate(sqrt((1+(2*y))),y)/2),y,(x^2)),integrate((sqrt((1+(2*(x^2))))*x),x))
try(subs(((((1+(2*y))^(3/2))/3)/2),y,(x^2)),integrate((sqrt((1+(2*(x^2))))*x),x))
try(subs((((1+(2*y))^(3/2))/6),y,(x*x)),integrate((sqrt((1+(2*x*x)))*x),x))
((1+(2*(x^2)))^(3/2))/6

===========
question 47
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.2, QUESTION  11 )
solution with steps:

integrate(x/sqrt(x+4),x)
integrate((x/sqrt((4+x))),x)
try(integrate(((1/sqrt((4+x)))*x),x),subs(integrate(((-4+y)*(1/sqrt(y))),y),y,(4+x)))
try(integrate(((1/sqrt((4+x)))*x),x),subs(integrate((((1/sqrt(y))*y)-(4*(1/sqrt(y)))),y),y,(4+x)))
try(integrate(((1/sqrt((4+x)))*x),x),subs((integrate(((1/sqrt(y))*y),y)+integrate(-(4*(1/sqrt(y))),y)),y,(4+x)))
try(integrate(((1/sqrt((4+x)))*x),x),subs((integrate(((1/sqrt(y))*y),y)-(4*integrate((1/sqrt(y)),y))),y,(4+x)))
try(integrate(((1/sqrt((4+x)))*x),x),subs((((2*(y^(3/2)))/3)-(4*sqrt((4*y)))),y,(4+x)))
try(integrate(((1/sqrt((4+x)))*x),x),subs((((2*(y^(3/2)))-sqrt((576*y)))/3),y,(4+x)))
try(integrate(((1/sqrt((4+x)))*x),x),subs(((2*(-sqrt((144*y))+(y^(3/2))))/3),y,(4+x)))
(2*(-sqrt((144*(4+x)))+((4+x)^(3/2))))/3

===========
question 48
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.2, QUESTION  16 )
solution with steps:

integrate(e^(2*x+3),x)
integrate((e^(3+(2*x))),x)
(e^(3+(2*x)))/2

===========
question 49
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.2, QUESTION  17 )
solution with steps:

integrate(x/e^(x^2),x)
integrate((x/(e^(x^2))),x)
try(subs(integrate(-(1/2),y),y,(e^-(x^2))),integrate(((e^-(x^2))*x),x),subs(integrate(((e^-y)/2),y),y,(x^2)))
try(subs(integrate(-(1/2),y),y,(e^-(x^2))),integrate(((e^-(x^2))*x),x),subs((integrate((e^-y),y)/2),y,(x^2)))
try(subs(-(y/2),y,(e^-(x^2))),integrate(((e^-(x^2))*x),x),subs((-(e^-y)/2),y,(x^2)))
try(subs(-(y/2),y,(e^-(x^2))),integrate(((e^-(x^2))*x),x),subs(-((e^-y)/2),y,(x*x)))
try(subs(-(y/2),y,(e^-(x^2))),integrate(((e^-(x^2))*x),x),subs(-((e^-y)/2),y,(x^2)))
try(subs(-(y/2),y,(e^-(x^2))),integrate(((e^-(x^2))*x),x),subs(-((e^-y)/2),y,(x^2)))
-((e^-(x^2))/2)

===========
question 50
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.2, QUESTION  27 )
solution with steps:

integrate(sqrt(sin(2*x))*cos(2*x),x)
integrate((cos((2*x))*sqrt(sin((2*x)))),x)
try(subs(integrate(((cos(y)*sqrt(sin(y)))/2),y),y,(2*x)),subs(integrate(-(((1/sqrt(sin(arccos(y))))*y)/2),y),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs(integrate((sqrt(y)/2),y),y,sin((2*x))))
try(subs(integrate(((cos(y)*sqrt(sin(y)))/2),y),y,(2*x)),subs(integrate(-(((1/sqrt(sin(arccos(y))))*y)/2),y),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs(integrate((sqrt(y)/2),y),y,sin((2*x))))
try(subs((integrate((cos(y)*sqrt(sin(y))),y)/2),y,(2*x)),subs(-(integrate(((1/sqrt(sin(arccos(y))))*y),y)/2),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs((integrate(sqrt(y),y)/2),y,sin((2*x))))
try(subs((integrate((cos(y)*sqrt(sin(y))),y)/2),y,(2*x)),subs(-(integrate(((1/sqrt(sin(arccos(y))))*y),y)/2),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs((integrate(sqrt(y),y)/2),y,sin((2*x))))
try(subs((integrate((cos(y)*sqrt(sin(y))),y)/2),y,(2*x)),subs(-(integrate(((1/sqrt(sin(arccos(y))))*y),y)/2),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs((((2*(y^(3/2)))/3)/2),y,sin((2*x))))
try(subs((integrate((cos(y)*sqrt(sin(y))),y)/2),y,(2*x)),subs(-(integrate(((1/sqrt(sin(arccos(y))))*y),y)/2),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs(((2*(y^(3/2)))/6),y,sin((2*x))))
try(subs((integrate((cos(y)*sqrt(sin(y))),y)/2),y,(2*x)),subs(-(integrate(((1/sqrt(sin(arccos(y))))*y),y)/2),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs(((y^(3/2))/3),y,sin((2*x))))
try(subs((integrate((cos(y)*sqrt(sin(y))),y)/2),y,(2*x)),subs(-(integrate(((1/sqrt(sin(arccos(y))))*y),y)/2),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs(((y^(3/2))/3),y,sin((2*x))))
(sin((2*x))^(3/2))/3

===========
question 51
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.2, QUESTION  3 )
solution with steps:

integrate(sin(x)*sin(cos(x)),x)
integrate((sin(cos(x))*sin(x)),x)
try(subs(integrate(-sin(y),y),y,cos(x)),subs(integrate(((sin(cos(arcsin(y)))*y)/cos(arcsin(y))),y),y,sin(x)),subs(integrate(-(sin(cos(arcsin(y)))/cos(cos(arcsin(y)))),y),y,sin(cos(x))),integrate((sin(cos(x))*sin(x)),x))
try(subs(-integrate(sin(y),y),y,cos(x)),subs(integrate(((sin(cos(arcsin(y)))*y)/cos(arcsin(y))),y),y,sin(x)),subs(-integrate((sin(cos(arcsin(y)))/cos(cos(arcsin(y)))),y),y,sin(cos(x))),integrate((sin(cos(x))*sin(x)),x))
try(subs(--cos(y),y,cos(x)),subs(integrate(((sin(cos(arcsin(y)))*y)/cos(arcsin(y))),y),y,sin(x)),subs(-integrate((sin(cos(arcsin(y)))/cos(cos(arcsin(y)))),y),y,sin(cos(x))),integrate((sin(cos(x))*sin(x)),x))
try(subs(--cos(y),y,cos(x)),subs(integrate(((sin(cos(arcsin(y)))*y)/cos(arcsin(y))),y),y,sin(x)),subs(-integrate((sin(cos(arcsin(y)))/cos(cos(arcsin(y)))),y),y,sin(cos(x))),integrate((sin(cos(x))*sin(x)),x))
cos(cos(x))

===========
question 52
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 9 EXERCISE 9.3, QUESTION  9 )
solution with steps:

dif(y,x)=arcsin(x)
dif(y,x)=arcsin(x)
(dif(y,x)-arcsin(x))=0
((integrate(-1,y)+c1)+integrate(arcsin(x),x))=0
((-y+c1)+integrate(arcsin(x),x))=0
(integrate(arcsin(x),x)-y+c1)=0
(-y+try(((arcsin(x)*integrate(1,x))-integrate(((1/sqrt((1-(x^2))))*integrate(1,x)),x)),(integrate(arcsin(x),x)-integrate(0,x)))+c1)=0
(-y+try(((arcsin(x)*(1*x))-integrate(((1/sqrt((1-(x^2))))*x),x)),(integrate(arcsin(x),x)-0))+c1)=0
(-y+try(((arcsin(x)*x)-integrate(((1/sqrt((1-(x^2))))*x),x)),integrate(arcsin(x),x))+c1)=0
(-y+try(((arcsin(x)*x)+sqrt((1-(x^2)))),integrate(arcsin(x),x))+c1)=0
(((arcsin(x)*x)+sqrt((1-(x^2))))-y+c1)=0

===========
question 53
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 9 EXERCISE 9.3, QUESTION  2 )
solution with steps:

dif(y,x) = sqrt(4-y^2)
dif(y,x)=sqrt((4-(y^2)))
(dif(y,x)-sqrt((4-(y^2))))=0
((integrate(1,x)+c1)+integrate(-(1/sqrt((4-(y^2)))),y))=0
((integrate(1,x)+c1)-integrate((1/sqrt((4-(y^2)))),y))=0
(((1*x)+c1)-integrate((1/sqrt((4-(y^2)))),y))=0
(-integrate((1/sqrt((4-(y^2)))),y)+x+c1)=0
(-try(subs(integrate((((1/sqrt((4-z)))*(1/sqrt(z)))/2),z),z,(y^2)),integrate((1/sqrt((4-(y^2)))),y))+x+c1)=0
(-try(subs((integrate(((1/sqrt((4-z)))*(1/sqrt(z))),z)/2),z,(y^2)),integrate((1/sqrt((4-(y^2)))),y))+x+c1)=0
(-try(subs((arcsin(((-2+z)/2))/2),z,(y^2)),arcsin((y/2)))+x+c1)=0
(-arcsin((y/2))+x+c1)=0

===========
question 54
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 9 EXERCISE 9.3, QUESTION  3 )
solution with steps:

dif(y,x)+y=1
(dif(y,x)+y)=1
(-1+dif(y,x)+y)=0
((integrate(1,x)+c1)+integrate(-(1/(1-y)),y))=0
((integrate(1,x)+c1)-integrate((1/(1-y)),y))=0
(((1*x)+c1)--log(abs((1-y))))=0

===========
question 55
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 9 EXERCISE 9.3, QUESTION  8 )
solution with steps:

x^5*dif(y,x)=-y^5
(dif(y,x)*(x^5))=((0-y)^5)
((dif(y,x)*(x^5))-(-y^5))=0
((integrate((-(1/(x^4))/x),x)+c1)+integrate((-(1/(y^4))/y),y))=0
(integrate(-(1/(x^5)),x)+integrate(-(1/(y^5)),y)+c1)=0
(-integrate((1/(x^5)),x)-integrate((1/(y^5)),y)+c1)=0
(-integrate((1/(x^5)),x)-integrate((1/(y^5)),y)+c1)=0
(--(1/(4*(x^4)))--(1/(4*(y^4)))+c1)=0
```