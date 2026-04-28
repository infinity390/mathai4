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

#### Example Demonstration 1 (derivation of hydrogen atom's ground state energy in electron volts using the variational principle in quantum physics)
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

#### Example Demonstration 2 (boolean algebra)
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

#### Example Demonstration 3 (limits approaching to a constant value)
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

#### Example Demonstration 4 (limits approaching to infinity)
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

#### Example Demonstration 5 (linear equations) (general solution of linear equations in two variables)
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

#### Example Demonstration 6 (expectation algebra)
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

### Questions solved using god() function

#### Code

```python
from mathai import *
lst = [
"(x+y)*dif(y,x)=1", "abs(abs(x-2)-3)<=2", "abs(x)>=0", "dif(y,x)=arcsin(x)", "(5*x-1)<(x+1)^2&(x+1)^2<7*x-3",
"integrate(sin(x)^6+cos(x)^6+3*sin(x)^2*cos(x)^2,x)", "integrate((x^4+x^2+1)/(x^2-x+1),x)", "integrate(1/(sin(x)^2*cos(x)^2),x)",
"integrate(x/sqrt(x+4),x)", "integrate(1/(1+sin(x)),x)", "integrate(sin(x)/(1+cos(x))^2,x)", "integrate(sin(x)^4,x)",
"integrate(abs(x+1),x,-4,10)", "integrate(abs(sin(x)),x,-pi,pi/6)", "integrate(2*x/(1+x^2),x)", "integrate(sin(2*x+5)^2,x)",
"integrate(sqrt(a*x+b),x)", "integrate(x*sqrt(x),x)","integrate(x*sqrt(1+2*x^2),x)", "integrate(e^(2*x+3),x)",
"integrate(x/e^(x^2),x)","integrate(sqrt(sin(2*x))*cos(2*x),x)", "integrate(sin(x)*sin(cos(x)),x)", 
"integrate(sin(3*x)*cos(4*x),x)","integrate(cos(2*x)*cos(4*x)*cos(6*x),x)", "integrate(sin(2*x+1)^3,x)",
"integrate(sin(x)^3*cos(x)^3,x)", "integrate(sin(x)*sin(2*x)*sin(3*x),x)","integrate(sin(4*x)*sin(8*x),x)", 
"integrate(cos(2*x)^4,x)", "integrate(x/((x+1)*(x+2)),x)","integrate(1/(x^2-9),x)", "integrate((3*x-1)/((x-1)*(x-2)*(x-3)),x)",
"integrate(x/((x-1)*(x-2)*(x-3)),x)", "integrate(2*x/(x^2+3*x+2),x)","integrate((1-x^2)/(x*(1-2*x)),x)", 
"integrate(x/((x-1)^2*(x+2)),x)","integrate((2*x-3)/((x^2-1)*(2*x+3)),x)","integrate(5*x/((x+1)*(x^2-4)),x)", 
"(A-B)|(B-A) <-> (A|B)-(A&B)", "(cosec(x)-cot(x))^2=(1-cos(x))/(1+cos(x))", "(x+2)*(x+3)/((x-2)*(x-3))<=1", 
"cos(x)/(1+sin(x))+(1+sin(x))/cos(x)=2*sec(x)", "tan(x)/(1-cot(x))+cot(x)/(1-tan(x))=1+sec(x)*cosec(x)",
"(1+sec(x))/sec(x)=sin(x)^2/(1-cos(x))", "(cos(x)-sin(x)+1)/(cos(x)+sin(x)-1)=cosec(x)+cot(x)","integrate(x/((x-1)*(x^2+1)),x)",
"(sin(x)-2*sin(x)^3)/(2*cos(x)^3-cos(x))=tan(x)", "(sin(x)+cosec(x))^2+(cos(x)+sec(x))^2=7+tan(x)^2+cot(x)^2",
"(cosec(x)-sin(x))*(sec(x)-cos(x))=1/(tan(x)+cot(x))", "2*x/(2*x^2+5*x+2)>1/(x+1)", "integrate(2/((1-x)*(1+x^2)),x)",
"abs(x+5)*x+2*abs(x+7)-2=0", "x*abs(x)-5*abs(x+2)+6=0", "x^2-abs(x+2)+x>0", "abs(3*x-5)+abs(8-x)=abs(3+2*x)",
"abs(x^2+5*x+9)<abs(x^2+2*x+2)+abs(3*x+7)", "dif(y,x)=sqrt(4-y^2)", "dif(y,x)+y=1", "x^5*dif(y,x)=-y^5",
"dif(y,x)=(1+x^2)*(1+y^2)", "x*(x^2-1)*dif(y,x)=1", "(x^2+x*y)*dif(y,x)=(x^2+y^2)", "dif(y,x)=(x+y)/x",
"(x-y)*dif(y,x)-(x+y)=0", "(x^2-y^2)+2*x*y*dif(y,x)=0", "x*dif(y,x)-y+x*sin(y/x)=0", "integrate((7^(7^(7^x)))*(7^(7^x))*(7^x),x)"
]
for s in lst:
    god(s)
```

#### Output

```
? (x+y)*dif(y,x)=1
thinking...
(((e^-y)*x)-integrate(((e^-y)*y),y)-c1)=0
(((e^y)*x)-integrate(((e^y)*y),y)-c1)=0
(((e^y)*x)-subs(integrate(log(z),z),z,(e^y))-c1)=0
(((e^-y)*x)-try(subs(integrate(((e^z)*z),z),z,-y),subs(integrate(log(z),z),z,(e^-y)))-c1)=0
(((e^-y)*x)-try(subs(((integrate((e^z),z)*z)-integrate(integrate((e^z),z),z)),z,-y),subs(try(((integrate(1,z)*log(z))-integrate((integrate(1,z)/z),z)),(integrate(log(z),z)-integrate(0,z))),z,(e^-y)))-c1)=0
(((e^-y)*x)-try(subs((((e^z)*z)-(e^z)),z,-y),subs(try(((log(z)*z)-z),integrate(log(z),z)),z,(e^-y)))-c1)=0
=> (((e^-y)*x)+((e^-y)*y)-c1+(e^-y))=0

? abs(abs(x-2)-3)<=2
thinking...
(-2+abs((-3+abs((-2+x)))))<=0
(((-2+x)<0)&((((-3-(-2+x))<0)&(((-2-(-1-x))=0)|((-2-(-1-x))<0)))|(((~((-3-(-2+x))=0)&~((-3-(-2+x))<0))|((-3-(-2+x))=0))&(((-2+(-3-(-2+x)))=0)|((-2+(-3-(-2+x)))<0)))))|(((((-3+(-2+x))<0)&(((-2-(-5+x))=0)|((-2-(-5+x))<0)))|(((~((-3+(-2+x))=0)&~((-3+(-2+x))<0))|((-3+(-2+x))=0))&(((-2+(-3+(-2+x)))=0)|((-2+(-3+(-2+x)))<0))))&((~((-2+x)=0)&~((-2+x)<0))|((-2+x)=0)))
(((-2+x)<0)&((((-1-x)<0)&(((-1+x)=0)|((-1+x)<0)))|(((~((-1-x)=0)&~((-1-x)<0))|((-1-x)=0))&(((-3-x)=0)|((-3-x)<0)))))|(((((-5+x)<0)&(((3-x)=0)|((3-x)<0)))|(((~((-5+x)=0)&~((-5+x)<0))|((-5+x)=0))&(((-7+x)=0)|((-7+x)<0))))&((~((-2+x)=0)&~((-2+x)<0))|((-2+x)=0)))
(-3,1)U(3,7)U{-3,1,3,7}
=> (-3,1)U(3,7)U{-3,1,3,7}

? abs(x)>=0
thinking...
abs(x)>=0
((x<0)&((~(-x=0)&~(-x<0))|(-x=0)))|(((~(x=0)&~(x<0))|(x=0))&((~(x=0)&~(x<0))|(x=0)))
((x<0)&((~(x=0)&~(x>0))|(x=0)))|(((~(x=0)&~(x<0))|(x=0))&((~(x=0)&~(x<0))|(x=0)))
((x<0)&((~(~(x=0)&~(x<0))&~(x=0))|(x=0)))|(((~(x=0)&~(x<0))|(x=0))&((~(x=0)&~(x<0))|(x=0)))
(-inf,+inf)
=> (-inf,+inf)

? dif(y,x)=arcsin(x)
thinking...
(integrate(arcsin(x),x)-y+c1)=0
(-y+try(((arcsin(x)*integrate(1,x))-integrate(((1/sqrt((1-(x^2))))*integrate(1,x)),x)),(integrate(arcsin(x),x)-integrate(0,x)))+c1)=0
(-y+try(((arcsin(x)*x)+sqrt((1-(x^2)))),integrate(arcsin(x),x))+c1)=0
=> ((arcsin(x)*x)-y+sqrt((1-(x^2)))+c1)=0

? (5*x-1)<(x+1)^2&(x+1)^2<7*x-3
thinking...
((-1+(5*x)+((-1-x)*(1+x)))<0)&((3-(7*x)+((1+x)^2))<0)
(((-1+x)*(-4+x))<0)&(((-2+x)*(1-x))<0)
((-2+(3*x)-(x^2))<0)&((4-(5*x)+(x^2))<0)
(2,4)
=> (2,4)

? integrate(sin(x)^6+cos(x)^6+3*sin(x)^2*cos(x)^2,x)
thinking...
integrate((cos(x)^6),x)+integrate((sin(x)^6),x)+(3*integrate(((cos(x)^2)*(sin(x)^2)),x))
integrate((cos(x)*cos(x)*cos(x)*cos(x)*cos(x)*cos(x)),x)+integrate((sin(x)*sin(x)*sin(x)*sin(x)*sin(x)*sin(x)),x)+(3*integrate((cos(x)*cos(x)*sin(x)*sin(x)),x))
(3*try(subs(integrate(((cos(sqrt(y))*sin(sqrt(y)))/2),y),y,(sin(x)^2)),subs(integrate(-((y^2)*sin(arccos(y))),y),y,cos(x)),subs(integrate(-((cos(sqrt(y))*sin(sqrt(y)))/2),y),y,(cos(x)^2)),subs(integrate((cos(arcsin(y))*(y^2)),y),y,sin(x))))+try(subs(integrate((sin((y^(1/6)))/(6*cos((y^(1/6))))),y),y,(sin(x)^6)),subs(integrate(((y^6)/cos(arcsin(y))),y),y,sin(x)),subs(integrate(((sin(sqrt(y))^5)/(2*cos(sqrt(y)))),y),y,(sin(x)^2)))+try(subs(integrate(-(cos((y^(1/6)))/(6*sin((y^(1/6))))),y),y,(cos(x)^6)),subs(integrate(-((y^6)/sin(arccos(y))),y),y,cos(x)),subs(integrate(-((cos(sqrt(y))^5)/(2*sin(sqrt(y)))),y),y,(cos(x)^2)))
(3*try(subs((integrate((cos(sqrt(y))*sin(sqrt(y))),y)/2),y,(sin(x)^2)),subs(-integrate(((y^2)*sin(arccos(y))),y),y,cos(x)),subs(-(integrate((cos(sqrt(y))*sin(sqrt(y))),y)/2),y,(cos(x)^2)),subs(integrate((cos(arcsin(y))*(y^2)),y),y,sin(x))))+try(subs((integrate((sin((y^(1/6)))/cos((y^(1/6)))),y)/6),y,(sin(x)^6)),subs(integrate(((y^6)/cos(arcsin(y))),y),y,sin(x)),subs((integrate(((sin(sqrt(y))^5)/cos(sqrt(y))),y)/2),y,(sin(x)^2)))+try(subs(-(integrate((cos((y^(1/6)))/sin((y^(1/6)))),y)/6),y,(cos(x)^6)),subs(-integrate(((y^6)/sin(arccos(y))),y),y,cos(x)),subs(-(integrate(((cos(sqrt(y))^5)/sin(sqrt(y))),y)/2),y,(cos(x)^2)))
integrate(((5/16)+(cos((6*x))/32)+((15*cos((2*x)))/32)+((3*cos((4*x)))/16)),x)+integrate(((5/16)+((3*cos((4*x)))/16)-(cos((6*x))/32)-((15*cos((2*x)))/32)),x)+(3*integrate(((1/8)-(cos((4*x))/8)),x))
((3*sin((4*x)))/32)+((5*x)/8)+(3*((x/8)-(sin((4*x))/32)))
=> x

? integrate((x^4+x^2+1)/(x^2-x+1),x)
thinking...
integrate(((1+(x^2)+(x^4))/(1-x+(x^2))),x)
integrate((1+(x^2)+x),x)
((2*(x^3))+(3*(x^2))+(6*x))/6
=> ((2*(x^3))+(3*(x^2))+(6*x))/6

? integrate(1/(sin(x)^2*cos(x)^2),x)
thinking...
integrate((1/((cos(x)^2)*(sin(x)^2))),x)
integrate(((1/(cos(x)^2))+(1/(sin(x)^2))),x)
-cot(x)+tan(x)
=> (-(cos(x)^2)+(sin(x)^2))/(cos(x)*sin(x))

? integrate(x/sqrt(x+4),x)
thinking...
integrate(((1/sqrt((4+x)))*x),x)
try(subs(integrate(((-4+y)*(1/sqrt(y))),y),y,(4+x)),subs(integrate(((2*(4-(1/(y^2))))/(y^2)),y),y,(1/sqrt((4+x)))))
try(subs(integrate(((-4+y)*(1/sqrt(y))),y),y,(4+x)),subs((2*integrate(((-1+(4*(y^2)))/(y^4)),y)),y,(1/sqrt((4+x)))))
try(subs(integrate(((-4+y)*(1/sqrt(y))),y),y,(4+x)),subs((2*integrate(((4-(1/(y^2)))/(y^2)),y)),y,(1/sqrt((4+x)))))
try(subs(integrate((((1/sqrt(y))*y)-(4*(1/sqrt(y)))),y),y,(4+x)),subs((2*integrate(((4/(y^2))-(1/(y^4))),y)),y,(1/sqrt((4+x)))))
try(subs(((2*(-sqrt((144*y))+(y^(3/2))))/3),y,(4+x)),subs((2*integrate(((-1+(4*(y^2)))/(y^4)),y)),y,(1/sqrt((4+x)))))
=> (2*(-sqrt((144*(4+x)))+((4+x)^(3/2))))/3

? integrate(1/(1+sin(x)),x)
thinking...
integrate((1/(1+sin(x))),x)
integrate(((1-sin(x))/(cos(x)^2)),x)
integrate(((1/(cos(x)^2))-(sin(x)/(cos(x)^2))),x)
-integrate((sin(x)/(cos(x)^2)),x)+tan(x)
-try(subs(integrate((cos((1/sqrt(y)))/2),y),y,(1/(cos(x)^2))),subs(integrate(-(1/(y^2)),y),y,cos(x)),subs(integrate((y/(cos(arcsin(y))^3)),y),y,sin(x)),subs(integrate(-(1/(2*(cos(sqrt(y))^3))),y),y,(cos(x)^2)))+tan(x)
-try(subs((integrate(cos((1/sqrt(y))),y)/2),y,(1/(cos(x)^2))),subs((1/y),y,cos(x)),subs(integrate((y/(cos(arcsin(y))^3)),y),y,sin(x)),subs(-(integrate((1/(cos(sqrt(y))^3)),y)/2),y,(cos(x)^2)))+tan(x)
=> (-1+sin(x))/cos(x)

? integrate(sin(x)/(1+cos(x))^2,x)
thinking...
integrate((sin(x)/((1+cos(x))^2)),x)
integrate((((1-cos(x))^2)/(sin(x)^3)),x)
integrate((((1/(sin(x)^3))-(cos(x)/(sin(x)^3)))+(-(cos(x)/(sin(x)^3))--((cos(x)^2)/(sin(x)^3)))),x)
integrate((1/(sin(x)^3)),x)+integrate(((cos(x)^2)/(sin(x)^3)),x)-(2*integrate((cos(x)/(sin(x)^3)),x))
-(2*try(subs(integrate((1/(y^3)),y),y,sin(x)),subs(integrate(-(sin((y^-(1/3)))/3),y),y,(1/(sin(x)^3))),subs(integrate(-(y/(sin(arccos(y))^4)),y),y,cos(x))))+try(subs(integrate((cos(arcsin(y))/(y^3)),y),y,sin(x)),subs(integrate(-((cos((y^-(1/3)))*sin((y^-(1/3))))/3),y),y,(1/(sin(x)^3))),subs(integrate(-((y^2)/(sin(arccos(y))^4)),y),y,cos(x)),subs(integrate(-(cos(sqrt(y))/(2*(sin(sqrt(y))^4))),y),y,(cos(x)^2)))+try(subs(integrate(-(sin((y^-(1/3)))/(3*cos((y^-(1/3))))),y),y,(1/(sin(x)^3))),subs(integrate((1/(cos(arcsin(y))*(y^3))),y),y,sin(x)))
-(2*try(subs(-(1/(2*(y^2))),y,sin(x)),subs(-(integrate(sin((y^-(1/3))),y)/3),y,(1/(sin(x)^3))),subs(-integrate((y/(sin(arccos(y))^4)),y),y,cos(x))))+try(subs(integrate((cos(arcsin(y))/(y^3)),y),y,sin(x)),subs(-(integrate((cos((y^-(1/3)))*sin((y^-(1/3)))),y)/3),y,(1/(sin(x)^3))),subs(-integrate(((y^2)/(sin(arccos(y))^4)),y),y,cos(x)),subs(-(integrate((cos(sqrt(y))/(sin(sqrt(y))^4)),y)/2),y,(cos(x)^2)))+try(subs(-(integrate((sin((y^-(1/3)))/cos((y^-(1/3)))),y)/3),y,(1/(sin(x)^3))),subs(integrate((1/(cos(arcsin(y))*(y^3))),y),y,sin(x)))
integrate((sin(x)/((3/2)+(cos((2*x))/2)+(2*cos(x)))),x)
try(subs(integrate(-(1/((1+y)^2)),y),y,cos(x)),subs(integrate(-(1/(y^2)),y),y,(1+cos(x))),subs(integrate(((1/sqrt(y))/2),y),y,(1/((1+cos(x))^2))),subs(integrate((y/(cos(arcsin(y))*((1+cos(arcsin(y)))^2))),y),y,sin(x)))
try(subs((1/(1+y)),y,cos(x)),subs((1/y),y,(1+cos(x))),subs(sqrt(y),y,(1/((1+cos(x))^2))),subs(integrate((y/(cos(arcsin(y))*((1+cos(arcsin(y)))^2))),y),y,sin(x)))
=> 1/(1+cos(x))

? integrate(sin(x)^4,x)
thinking...
integrate((sin(x)^4),x)
integrate((sin(x)*sin(x)*sin(x)*sin(x)),x)
try(subs(integrate(((y^4)/cos(arcsin(y))),y),y,sin(x)),subs(integrate((sin((y^(1/4)))/(4*cos((y^(1/4))))),y),y,(sin(x)^4)),subs(integrate(((sin(sqrt(y))^3)/(2*cos(sqrt(y)))),y),y,(sin(x)^2)))
try(subs(integrate(((y^4)/cos(arcsin(y))),y),y,sin(x)),subs((integrate((sin((y^(1/4)))/cos((y^(1/4)))),y)/4),y,(sin(x)^4)),subs((integrate(((sin(sqrt(y))^3)/cos(sqrt(y))),y)/2),y,(sin(x)^2)))
integrate(((3/8)+(cos((4*x))/8)-(cos((2*x))/2)),x)
((3*x)/8)+(sin((4*x))/32)-(sin((2*x))/4)
=> ((32*sin((4*x)))+(384*x)-(256*sin((2*x))))/1024

? integrate(abs(x+1),x,-4,10)
thinking...
-subs((((2*x)+(x^2))/2),x,-1)-subs(((-(2*x)-(x^2))/2),x,-4)+subs((((2*x)+(x^2))/2),x,10)+subs(((-(2*x)-(x^2))/2),x,-1)
=> 65

? integrate(abs(sin(x)),x,-pi,pi/6)
thinking...
-subs(cos(x),x,-pi)-subs(-cos(x),x,0)+subs(cos(x),x,0)+subs(-cos(x),x,(pi/6))
=> (6-sqrt(3))/2

? integrate(2*x/(1+x^2),x)
thinking...
log(abs((1+(x^2))))
=> log(abs((1+(x^2))))

? integrate(sin(2*x+5)^2,x)
thinking...
integrate((sin((5+(2*x)))^2),x)
integrate((sin((5+(2*x)))*sin((5+(2*x)))),x)
try(subs(integrate(((sin((5+(2*((arcsin(y)/2)-(5/2)))))^2)/(2*cos((5+(2*((arcsin(y)/2)-(5/2))))))),y),y,sin((5+(2*x)))),subs(integrate((sin((5+(2*((sqrt(y)/2)-(5/2)))))/(4*cos((5+(2*((sqrt(y)/2)-(5/2))))))),y),y,(sin((5+(2*x)))^2)),subs(integrate(((sin((5+(2*((y/2)-(5/2)))))^2)/2),y),y,(5+(2*x))))
try(subs((integrate(((sin(arcsin(y))^2)/cos(arcsin(y))),y)/2),y,sin((5+(2*x)))),subs((integrate((sin(sqrt(y))/cos(sqrt(y))),y)/4),y,(sin((5+(2*x)))^2)),subs((integrate((sin(y)^2),y)/2),y,(5+(2*x))))
integrate(((1/2)-(cos((10+(4*x)))/2)),x)
(x/2)-(sin((10+(4*x)))/8)
=> ((8*x)-(2*sin((10+(4*x)))))/16

? integrate(sqrt(a*x+b),x)
thinking...
(2*(((x*a)+b)^(3/2)))/(3*a)
=> (2*(((x*a)+b)^(3/2)))/(3*a)

? integrate(x*sqrt(x),x)
thinking...
(2*(x^(5/2)))/5
=> (2*(x^(5/2)))/5

? integrate(x*sqrt(1+2*x^2),x)
thinking...
sqrt((1+(2*(x^2))))/2
=> sqrt((1+(2*(x^2))))/2

? integrate(e^(2*x+3),x)
thinking...
(e^(3+(2*x)))/2
=> (e^(3+(2*x)))/2

? integrate(x/e^(x^2),x)
thinking...
integrate(((e^-(x^2))*x),x)
integrate(((e^(x^2))*x),x)
integrate(((e^(x*x))*x),x)
try(subs(integrate((1/2),y),y,(e^(x^2))),subs(integrate(((e^y)/2),y),y,(x^2)))
try(subs((y/2),y,(e^(x^2))),subs(((e^y)/2),y,(x^2)))
=> (e^(x^2))/2

? integrate(sqrt(sin(2*x))*cos(2*x),x)
thinking...
integrate((cos((2*x))*sqrt(sin((2*x)))),x)
integrate((cos((2*x))*(1/sqrt(sin((2*x))))),x)
try(subs(integrate(((cos(y)*(1/sqrt(sin(y))))/2),y),y,(2*x)),subs(integrate(-(((sin(arccos(y))^-(3/2))*y)/2),y),y,cos((2*x))),subs(integrate(((1/sqrt(y))/2),y),y,sin((2*x))),subs(integrate(-sin((1/(y^2))),y),y,(1/sqrt(sin((2*x))))))
try(subs((integrate((cos(y)*(1/sqrt(sin(y)))),y)/2),y,(2*x)),subs(-(integrate(((sin(arccos(y))^-(3/2))*y),y)/2),y,cos((2*x))),subs(sqrt(y),y,sin((2*x))),subs(-integrate(sin((1/(y^2))),y),y,(1/sqrt(sin((2*x))))))
=> sqrt(sin((2*x)))

? integrate(sin(x)*sin(cos(x)),x)
thinking...
integrate((sin(cos(x))*sin(x)),x)
try(subs(integrate(((sin(cos(arcsin(y)))*y)/cos(arcsin(y))),y),y,sin(x)),subs(integrate(-sin(y),y),y,cos(x)),subs(integrate(-(sin(cos(arcsin(y)))/cos(cos(arcsin(y)))),y),y,sin(cos(x))))
try(subs(integrate(((sin(cos(arcsin(y)))*y)/cos(arcsin(y))),y),y,sin(x)),subs(cos(y),y,cos(x)),subs(-integrate((sin(cos(arcsin(y)))/cos(cos(arcsin(y)))),y),y,sin(cos(x))))
=> cos(cos(x))

? integrate(sin(3*x)*cos(4*x),x)
thinking...
integrate((cos((4*x))*sin((3*x))),x)
try(subs(integrate(-((sin(((3*arccos(y))/4))*y)/(4*sin(arccos(y)))),y),y,cos((4*x))),subs(integrate(((cos(((4*arcsin(y))/3))*y)/(3*cos(arcsin(y)))),y),y,sin((3*x))),subs(integrate(((cos(((4*y)/3))*sin(y))/3),y),y,(3*x)))
try(subs(-(integrate(((sin(((3*arccos(y))/4))*y)/sin(arccos(y))),y)/4),y,cos((4*x))),subs((integrate(((cos(((4*arcsin(y))/3))*y)/cos(arcsin(y))),y)/3),y,sin((3*x))),subs((integrate((cos(((4*y)/3))*sin(y)),y)/3),y,(3*x)))
integrate(((sin((7*x))/2)-(sin(x)/2)),x)
(cos(x)/2)-(cos((7*x))/14)
=> ((14*cos(x))-(2*cos((7*x))))/28

? integrate(cos(2*x)*cos(4*x)*cos(6*x),x)
thinking...
integrate((cos((2*x))*cos((4*x))*cos((6*x))),x)
try(subs(integrate(-((cos((2*arccos(y)))*cos((3*arccos(y)))*y)/(2*sin(arccos(y)))),y),y,cos((2*x))),subs(integrate(-((cos((arccos(y)/2))*cos(((3*arccos(y))/2))*y)/(4*sin(arccos(y)))),y),y,cos((4*x))),subs(integrate(((cos(((2*y)/3))*cos((y/3))*cos(y))/6),y),y,(6*x)),subs(integrate(-((cos((arccos(y)/3))*cos(((2*arccos(y))/3))*y)/(6*sin(arccos(y)))),y),y,cos((6*x))))
try(subs(-(integrate(((cos((2*arccos(y)))*cos((3*arccos(y)))*y)/sin(arccos(y))),y)/2),y,cos((2*x))),subs(-(integrate(((cos((arccos(y)/2))*cos(((3*arccos(y))/2))*y)/sin(arccos(y))),y)/4),y,cos((4*x))),subs((integrate((cos(((2*y)/3))*cos((y/3))*cos(y)),y)/6),y,(6*x)),subs(-(integrate(((cos((arccos(y)/3))*cos(((2*arccos(y))/3))*y)/sin(arccos(y))),y)/6),y,cos((6*x))))
integrate(((1/4)+(cos((12*x))/4)+(cos((4*x))/4)+(cos((8*x))/4)),x)
(sin((12*x))/48)+(sin((4*x))/16)+(sin((8*x))/32)+(x/4)
=> ((2048*sin((12*x)))+(24576*x)+(3072*sin((8*x)))+(6144*sin((4*x))))/98304

? integrate(sin(2*x+1)^3,x)
thinking...
integrate((sin((1+(2*x)))^3),x)
integrate((sin((1+(2*x)))*sin((1+(2*x)))*sin((1+(2*x)))),x)
try(subs(integrate(((sin((1+(2*((arcsin(y)/2)-(1/2)))))^3)/(2*cos((1+(2*((arcsin(y)/2)-(1/2))))))),y),y,sin((1+(2*x)))),subs(integrate(((sin((1+(2*((y/2)-(1/2)))))^3)/2),y),y,(1+(2*x))),subs(integrate((sin((1+(2*(((y^(1/3))/2)-(1/2)))))/(6*cos((1+(2*(((y^(1/3))/2)-(1/2))))))),y),y,(sin((1+(2*x)))^3)))
try(subs((integrate(((sin(arcsin(y))^3)/cos(arcsin(y))),y)/2),y,sin((1+(2*x)))),subs((integrate((sin(y)^3),y)/2),y,(1+(2*x))),subs((integrate((sin((y^(1/3)))/cos((y^(1/3)))),y)/6),y,(sin((1+(2*x)))^3)))
integrate(((sin((1+(2*x)))/2)-(sin((-1-(2*x)))/4)-(sin((3+(6*x)))/4)),x)
(cos((3+(6*x)))/24)-(cos((-1-(2*x)))/8)-(cos((1+(2*x)))/4)
=> ((32*cos((3+(6*x))))-(192*cos((1+(2*x))))-(96*cos((-1-(2*x)))))/768

? integrate(sin(x)^3*cos(x)^3,x)
thinking...
integrate(((cos(x)^3)*(sin(x)^3)),x)
integrate((cos(x)*cos(x)*cos(x)*sin(x)*sin(x)*sin(x)),x)
try(subs(integrate(-((sin(arccos(y))^2)*(y^3)),y),y,cos(x)),subs(integrate(((cos(arcsin(y))^2)*(y^3)),y),y,sin(x)),subs(integrate((((cos((y^(1/3)))^2)*sin((y^(1/3))))/3),y),y,(sin(x)^3)),subs(integrate(-((cos((y^(1/3)))*(sin((y^(1/3)))^2))/3),y),y,(cos(x)^3)))
try(subs(-integrate(((sin(arccos(y))^2)*(y^3)),y),y,cos(x)),subs(integrate(((cos(arcsin(y))^2)*(y^3)),y),y,sin(x)),subs((integrate(((cos((y^(1/3)))^2)*sin((y^(1/3)))),y)/3),y,(sin(x)^3)),subs(-(integrate((cos((y^(1/3)))*(sin((y^(1/3)))^2)),y)/3),y,(cos(x)^3)))
integrate((((3*sin((2*x)))/32)-(sin((6*x))/32)),x)
(cos((6*x))/192)-((3*cos((2*x)))/64)
=> ((64*cos((6*x)))-(576*cos((2*x))))/12288

? integrate(sin(x)*sin(2*x)*sin(3*x),x)
thinking...
integrate((sin((2*x))*sin((3*x))*sin(x)),x)
try(subs(integrate(((sin((2*arcsin(y)))*sin((3*arcsin(y)))*y)/cos(arcsin(y))),y),y,sin(x)),subs(integrate(((sin(((2*y)/3))*sin((y/3))*sin(y))/3),y),y,(3*x)),subs(integrate(((sin((arcsin(y)/2))*sin(((3*arcsin(y))/2))*y)/(2*cos(arcsin(y)))),y),y,sin((2*x))),subs(integrate(((sin((arcsin(y)/3))*sin(((2*arcsin(y))/3))*y)/(3*cos(arcsin(y)))),y),y,sin((3*x))))
try(subs(integrate(((sin((2*arcsin(y)))*sin((3*arcsin(y)))*y)/cos(arcsin(y))),y),y,sin(x)),subs((integrate((sin(((2*y)/3))*sin((y/3))*sin(y)),y)/3),y,(3*x)),subs((integrate(((sin((arcsin(y)/2))*sin(((3*arcsin(y))/2))*y)/cos(arcsin(y))),y)/2),y,sin((2*x))),subs((integrate(((sin((arcsin(y)/3))*sin(((2*arcsin(y))/3))*y)/cos(arcsin(y))),y)/3),y,sin((3*x))))
integrate(((sin((2*x))/4)+(sin((4*x))/4)-(sin((6*x))/4)),x)
(cos((6*x))/24)-(cos((2*x))/8)-(cos((4*x))/16)
=> ((128*cos((6*x)))-(192*cos((4*x)))-(384*cos((2*x))))/3072

? integrate(sin(4*x)*sin(8*x),x)
thinking...
integrate((sin((4*x))*sin((8*x))),x)
try(subs(integrate(((sin((2*arcsin(y)))*y)/(4*cos(arcsin(y)))),y),y,sin((4*x))),subs(integrate(((sin((arcsin(y)/2))*y)/(8*cos(arcsin(y)))),y),y,sin((8*x))),subs(integrate(((sin((y/2))*sin(y))/8),y),y,(8*x)))
try(subs((integrate(((sin((2*arcsin(y)))*y)/cos(arcsin(y))),y)/4),y,sin((4*x))),subs((integrate(((sin((arcsin(y)/2))*y)/cos(arcsin(y))),y)/8),y,sin((8*x))),subs((integrate((sin((y/2))*sin(y)),y)/8),y,(8*x)))
integrate(((cos((4*x))/2)-(cos((12*x))/2)),x)
(sin((4*x))/8)-(sin((12*x))/24)
=> ((24*sin((4*x)))-(8*sin((12*x))))/192

? integrate(cos(2*x)^4,x)
thinking...
integrate((cos((2*x))^4),x)
integrate((cos((2*x))*cos((2*x))*cos((2*x))*cos((2*x))),x)
try(subs(integrate(((cos(y)^4)/2),y),y,(2*x)),subs(integrate(-((y^4)/(2*sin(arccos(y)))),y),y,cos((2*x))),subs(integrate(-(cos((y^(1/4)))/(8*sin((y^(1/4))))),y),y,(cos((2*x))^4)))
try(subs((integrate((cos(y)^4),y)/2),y,(2*x)),subs(-(integrate(((y^4)/sin(arccos(y))),y)/2),y,cos((2*x))),subs(-(integrate((cos((y^(1/4)))/sin((y^(1/4)))),y)/8),y,(cos((2*x))^4)))
integrate(((3/8)+(cos((4*x))/2)+(cos((8*x))/8)),x)
((3*x)/8)+(sin((4*x))/8)+(sin((8*x))/64)
=> ((1536*x)+(512*sin((4*x)))+(64*sin((8*x))))/4096

? integrate(x/((x+1)*(x+2)),x)
thinking...
(log(abs((2+(3*x)+(x^2))))/2)-((3*integrate((1/(2+(3*x)+(x^2))),x))/2)
(log(abs(((1+x)*(2+x))))/2)-((3*integrate((1/((1+x)*(2+x))),x))/2)
(log(abs(((1+x)*(2+x))))/2)-((3*integrate(((1/(2+x))-(1/(1+x))),x))/2)
(log(abs(((1+x)*(2+x))))/2)+((3*(log(abs((1+x)))-log(abs((2+x)))))/2)
=> ((2*log(abs((2+(3*x)+(x^2)))))+(6*log(abs((1+x))))-(6*log(abs((2+x)))))/4

? integrate(1/(x^2-9),x)
thinking...
integrate((1/((-3+x)*(3+x))),x)
integrate(((1/(6*(3+x)))-(1/(6*(-3+x)))),x)
(log(abs((3+x)))/6)-(log(abs((-3+x)))/6)
=> ((6*log(abs((3+x))))-(6*log(abs((-3+x)))))/36

? integrate((3*x-1)/((x-1)*(x-2)*(x-3)),x)
thinking...
integrate(((-1+(3*x))/((-1+x)*(-2+x)*(-3+x))),x)
integrate(((5/(-2+x))-(1/(-1+x))-(4/(-3+x))),x)
(5*log(abs((-2+x))))-log(abs((-1+x)))-(4*log(abs((-3+x))))
=> (5*log(abs((-2+x))))-log(abs((-1+x)))-(4*log(abs((-3+x))))

? integrate(x/((x-1)*(x-2)*(x-3)),x)
thinking...
integrate((x/((-1+x)*(-2+x)*(-3+x))),x)
integrate(((2/(-2+x))-(1/(2*(-1+x)))-(3/(2*(-3+x)))),x)
(2*log(abs((-2+x))))-(log(abs((-1+x)))/2)-((3*log(abs((-3+x))))/2)
=> ((8*log(abs((-2+x))))-(2*log(abs((-1+x))))-(6*log(abs((-3+x)))))/4

? integrate(2*x/(x^2+3*x+2),x)
thinking...
2*((log(abs((2+(3*x)+(x^2))))/2)-((3*integrate((1/(2+(3*x)+(x^2))),x))/2))
2*((log(abs(((1+x)*(2+x))))/2)-((3*integrate((1/((1+x)*(2+x))),x))/2))
2*((log(abs(((1+x)*(2+x))))/2)-((3*integrate(((1/(2+x))-(1/(1+x))),x))/2))
2*((log(abs(((1+x)*(2+x))))/2)+((3*(log(abs((1+x)))-log(abs((2+x)))))/2))
=> ((2*log(abs((2+(3*x)+(x^2)))))+(6*log(abs((1+x))))-(6*log(abs((2+x)))))/2

? integrate((1-x^2)/(x*(1-2*x)),x)
thinking...
integrate((((1-x)*(1+x))/((1-(2*x))*x)),x)
integrate((-(1/x)-(2/(1-(2*x)))),x)
log(abs((1-(2*x))))-log(abs(x))
=> log(abs((1-(2*x))))-log(abs(x))

? integrate(x/((x-1)^2*(x+2)),x)
thinking...
integrate((x/((2+x)*((-1+x)^2))),x)
integrate(((2/(9*(2+x)))-(1/(3*((-1+x)^2)))-(2/(9*(-1+x)))),x)
(1/(3*(-1+x)))+((2*log(abs((2+x))))/9)-((2*log(abs((-1+x))))/9)
=> (81+(54*log(abs((-1+x))))+(54*log(abs((2+x)))*x)-(54*log(abs((-1+x)))*x)-(54*log(abs((2+x)))))/(243*(-1+x))

? integrate((2*x-3)/((x^2-1)*(2*x+3)),x)
thinking...
integrate(((-3+(2*x))/((-1+x)*(1+x)*(3+(2*x)))),x)
integrate(((1/(10*(-1+x)))+(24/(5*(3+(2*x))))-(5/(2*(1+x)))),x)
(log(abs((-1+x)))/10)+((12*log(abs((3+(2*x)))))/5)-((5*log(abs((1+x))))/2)
=> ((10*log(abs((-1+x))))+(240*log(abs((3+(2*x)))))-(250*log(abs((1+x)))))/100

? integrate(5*x/((x+1)*(x^2-4)),x)
thinking...
5*integrate((x/((-2+x)*(1+x)*(2+x))),x)
5*integrate(((1/(2*(2+x)))-(1/(3*(1+x)))-(1/(6*(-2+x)))),x)
5*((log(abs((2+x)))/2)-(log(abs((-2+x)))/6)-(log(abs((1+x)))/3))
=> (5*((18*log(abs((2+x))))-(12*log(abs((1+x))))-(6*log(abs((-2+x))))))/36

? (A-B)|(B-A) <-> (A|B)-(A&B)
thinking...
=> true

? (cosec(x)-cot(x))^2=(1-cos(x))/(1+cos(x))
thinking...
(-((1-cos(x))/(1+cos(x)))+((cosec(x)-cot(x))^2))=0
(-((1-cos(x))/(1+cos(x)))+(((1/sin(x))-(cos(x)/sin(x)))^2))=0
((cos(x)*(sin(x)^8))+((cos(x)^3)*(sin(x)^6))-(cos(x)*(sin(x)^6))-((cos(x)^2)*(sin(x)^6))-(sin(x)^8)+(sin(x)^6))=0
0=0
true
=> true

? (x+2)*(x+3)/((x-2)*(x-3))<=1
thinking...
(-1+(((2+x)*(3+x))/((-2+x)*(-3+x))))<=0
((-1+(((2+x)*(3+x))/((-2+x)*(-3+x))))=0)|((-1+(((2+x)*(3+x))/((-2+x)*(-3+x))))<0)
(x=0)|((x/((-2+x)*(-3+x)))<0)
(-inf,0)U(2,3)U{0}
=> (-inf,0)U(2,3)U{0}

? cos(x)/(1+sin(x))+(1+sin(x))/cos(x)=2*sec(x)
thinking...
(((1+sin(x))/cos(x))+(cos(x)/(1+sin(x)))-(2*sec(x)))=0
(((1+sin(x))/cos(x))+(cos(x)/(1+sin(x)))-(2/cos(x)))=0
((cos(x)*(sin(x)^2))-cos(x)+(cos(x)^3))=0
0=0
true
=> true

? tan(x)/(1-cot(x))+cot(x)/(1-tan(x))=1+sec(x)*cosec(x)
thinking...
(-1+(cot(x)/(1-tan(x)))+(tan(x)/(1-cot(x)))-(cosec(x)*sec(x)))=0
(-1+((cos(x)/sin(x))/(1-(sin(x)/cos(x))))+((sin(x)/cos(x))/(1-(cos(x)/sin(x))))-(1/(cos(x)*sin(x))))=0
((2*cos(x)*(sin(x)^3))+(2*(cos(x)^3)*sin(x))-(2*cos(x)*sin(x))-(2*(cos(x)^2)*(sin(x)^2))-(cos(x)^4)-(sin(x)^4)+(cos(x)^2)+(sin(x)^2))=0
0=0
true
=> true

? (1+sec(x))/sec(x)=sin(x)^2/(1-cos(x))
thinking...
(((1+sec(x))/sec(x))-((sin(x)^2)/(1-cos(x))))=0
(((1+(1/cos(x)))/(1/cos(x)))-((sin(x)^2)/(1-cos(x))))=0
(1-(cos(x)^2)-(sin(x)^2))=0
0=0
true
=> true

? (cos(x)-sin(x)+1)/(cos(x)+sin(x)-1)=cosec(x)+cot(x)
thinking...
(((1+cos(x)-sin(x))/(-1+cos(x)+sin(x)))-cosec(x)-cot(x))=0
(((1+cos(x)-sin(x))/(-1+cos(x)+sin(x)))-(1/sin(x))-(cos(x)/sin(x)))=0
(-((cos(x)^2)*sin(x))-(sin(x)^3)+sin(x))=0
0=0
true
=> true

? integrate(x/((x-1)*(x^2+1)),x)
thinking...
integrate((x/((-1+x)*(1+(x^2)))),x)
integrate(((((x/2)-(1/2))/(1+(x^2)))-(1/(2*(-1+x)))),x)
(log(abs((1+(x^2))))/4)-(arctan(x)/2)-(log(abs((-1+x)))/2)
=> ((4*log(abs((1+(x^2)))))-(8*arctan(x))-(8*log(abs((-1+x)))))/16

? (sin(x)-2*sin(x)^3)/(2*cos(x)^3-cos(x))=tan(x)
thinking...
(((-(2*(sin(x)^3))+sin(x))/((2*(cos(x)^3))-cos(x)))-tan(x))=0
(((-(2*(sin(x)^3))+sin(x))/((2*(cos(x)^3))-cos(x)))-(sin(x)/cos(x)))=0
((2*cos(x)*sin(x))-(2*cos(x)*(sin(x)^3))-(2*(cos(x)^3)*sin(x)))=0
0=0
true
=> true

? (sin(x)+cosec(x))^2+(cos(x)+sec(x))^2=7+tan(x)^2+cot(x)^2
thinking...
(-7-(cot(x)^2)-(tan(x)^2)+((cos(x)+sec(x))^2)+((cosec(x)+sin(x))^2))=0
(-7-((cos(x)/sin(x))^2)-((sin(x)/cos(x))^2)+((cos(x)+(1/cos(x)))^2)+(((1/sin(x))+sin(x))^2))=0
(((cos(x)^2)*(sin(x)^4))+((cos(x)^4)*(sin(x)^2))+((cos(x)^4)*(sin(x)^6))+((cos(x)^6)*(sin(x)^4))-(3*(cos(x)^4)*(sin(x)^4))-((cos(x)^2)*(sin(x)^6))-((cos(x)^6)*(sin(x)^2)))=0
0=0
true
=> true

? (cosec(x)-sin(x))*(sec(x)-cos(x))=1/(tan(x)+cot(x))
thinking...
(((cosec(x)-sin(x))*(-cos(x)+sec(x)))-(1/(cot(x)+tan(x))))=0
((((1/cos(x))-cos(x))*((1/sin(x))-sin(x)))-(1/((cos(x)/sin(x))+(sin(x)/cos(x)))))=0
((cos(x)*(sin(x)^3))+((cos(x)^3)*(sin(x)^5))+((cos(x)^3)*sin(x))+((cos(x)^5)*(sin(x)^3))-(3*(cos(x)^3)*(sin(x)^3))-(cos(x)*(sin(x)^5))-((cos(x)^5)*sin(x)))=0
0=0
true
=> true

? 2*x/(2*x^2+5*x+2)>1/(x+1)
thinking...
(((2*x)/(2+(2*(x^2))+(5*x)))-(1/(1+x)))>0
~((((2*x)/(2+(2*(x^2))+(5*x)))-(1/(1+x)))=0)&~((((2*x)/(2+(2*(x^2))+(5*x)))-(1/(1+x)))<0)
~((-2-(3*x))=0)&~(((-2-(3*x))/((1+x)*(2+(2*(x^2))+(5*x))))<0)
~((-2-(3*x))=0)&~(((-2-(3*x))/((1+x)*(2+x)*((1/2)+x)))<0)
(-2,-1)U(-(2/3),-(1/2))U{-(1/2),-2,-1}
=> (-2,-1)U(-(2/3),-(1/2))U{-(1/2),-2,-1}

? integrate(2/((1-x)*(1+x^2)),x)
thinking...
2*integrate((1/((1-x)*(1+(x^2)))),x)
2*integrate((((-(1/2)-(x/2))/(1+(x^2)))-(1/(2*(1-x)))),x)
2*((log(abs((1-x)))/2)-(arctan(x)/2)-(log(abs((1+(x^2))))/4))
=> ((8*log(abs((1-x))))-(4*log(abs((1+(x^2)))))-(8*arctan(x)))/8

? abs(x+5)*x+2*abs(x+7)-2=0
thinking...
(-2+(2*abs((7+x)))+(abs((5+x))*x))=0
(((5+x)<0)&((((-2+(2*(7+x))+(-(5+x)*x))=0)&((~((7+x)=0)&~((7+x)<0))|((7+x)=0)))|(((-2+(2*-(7+x))+(-(5+x)*x))=0)&((7+x)<0))))|(((((-2+(2*(7+x))+((5+x)*x))=0)&((~((7+x)=0)&~((7+x)<0))|((7+x)=0)))|(((-2+(2*-(7+x))+((5+x)*x))=0)&((7+x)<0)))&((~((5+x)=0)&~((5+x)<0))|((5+x)=0)))
(((5+x)<0)&((((-2+(2*(-7-x))+((-5-x)*x))=0)&((7+x)<0))|(((-2+(2*(7+x))+((-5-x)*x))=0)&((~((7+x)=0)&~((7+x)<0))|((7+x)=0)))))|(((((-2+(2*(-7-x))+((5+x)*x))=0)&((7+x)<0))|(((-2+(2*(7+x))+((5+x)*x))=0)&((~((7+x)=0)&~((7+x)<0))|((7+x)=0))))&((~((5+x)=0)&~((5+x)<0))|((5+x)=0)))
(((5+x)<0)&((((-2+(2*(-7-x))-((5+x)*x))=0)&((7+x)<0))|((((((-3-sqrt(57))/2)-x)*(((3-sqrt(57))/2)+x))=0)&((~((7+x)=0)&~((7+x)<0))|((7+x)=0)))))|((((((3+x)*(4+x))=0)&((~((7+x)=0)&~((7+x)<0))|((7+x)=0)))|((((((3-sqrt(73))/2)+x)*(((3+sqrt(73))/2)+x))=0)&((7+x)<0)))&((~((5+x)=0)&~((5+x)<0))|((5+x)=0)))
(((5+x)<0)&((((-16-(7*x)-(x^2))=0)&((7+x)<0))|(((12-(3*x)-(x^2))=0)&((~((7+x)=0)&~((7+x)<0))|((7+x)=0)))))|(((((-16+(3*x)+(x^2))=0)&((7+x)<0))|(((12+(7*x)+(x^2))=0)&((~((7+x)=0)&~((7+x)<0))|((7+x)=0))))&((~((5+x)=0)&~((5+x)<0))|((5+x)=0)))
(((5+x)<0)&((((-16-(7*x)-(x^2))=0)&((7+x)<0))|((((((-3-sqrt(57))/2)-x)*(((3-sqrt(57))/2)+x))=0)&((~((7+x)=0)&~((7+x)<0))|((7+x)=0)))))|((((((3+x)*(4+x))=0)&((~((7+x)=0)&~((7+x)<0))|((7+x)=0)))|((((((3-sqrt(73))/2)+x)*(((3+sqrt(73))/2)+x))=0)&((7+x)<0)))&((~((5+x)=0)&~((5+x)<0))|((5+x)=0)))
{-(3/2)-(sqrt(57)/2),-3,-4}
=> {-(3/2)-(sqrt(57)/2),-3,-4}

? x*abs(x)-5*abs(x+2)+6=0
thinking...
(6+(abs(x)*x)-(5*abs((2+x))))=0
((x<0)&((((6+(-x*x)-(5*(-2-x)))=0)&((2+x)<0))|(((6+(-x*x)-(5*(2+x)))=0)&((~((2+x)=0)&~((2+x)<0))|((2+x)=0)))))|(((((6+(x*x)-(5*(-2-x)))=0)&((2+x)<0))|(((6+(x*x)-(5*(2+x)))=0)&((~((2+x)=0)&~((2+x)<0))|((2+x)=0))))&((~(x=0)&~(x<0))|(x=0)))
((x<0)&((((6+(5*(-2-x))-(x^2))=0)&((~((2+x)=0)&~((2+x)<0))|((2+x)=0)))|(((6+(5*(2+x))-(x^2))=0)&((2+x)<0))))|(((((6+(5*(-2-x))+(x^2))=0)&((~((2+x)=0)&~((2+x)<0))|((2+x)=0)))|(((6+(5*(2+x))+(x^2))=0)&((2+x)<0)))&((~(x=0)&~(x<0))|(x=0)))
((x<0)&(((((-1-x)*(4+x))=0)&((~((2+x)=0)&~((2+x)<0))|((2+x)=0)))|((((((-5-sqrt(89))/2)+x)*(((5-sqrt(89))/2)-x))=0)&((2+x)<0))))|(((((6+(5*(2+x))+(x^2))=0)&((2+x)<0))|((((((-5-sqrt(41))/2)+x)*(((-5+sqrt(41))/2)+x))=0)&((~((2+x)=0)&~((2+x)<0))|((2+x)=0))))&((~(x=0)&~(x<0))|(x=0)))
((x<0)&((((-4-(5*x)-(x^2))=0)&((~((2+x)=0)&~((2+x)<0))|((2+x)=0)))|(((16+(5*x)-(x^2))=0)&((2+x)<0))))|(((((-4-(5*x)+(x^2))=0)&((~((2+x)=0)&~((2+x)<0))|((2+x)=0)))|(((16+(5*x)+(x^2))=0)&((2+x)<0)))&((~(x=0)&~(x<0))|(x=0)))
((x<0)&(((((-1-x)*(4+x))=0)&((~((2+x)=0)&~((2+x)<0))|((2+x)=0)))|((((((-5-sqrt(89))/2)+x)*(((5-sqrt(89))/2)-x))=0)&((2+x)<0))))|(((((16+(5*x)+(x^2))=0)&((2+x)<0))|((((((-5-sqrt(41))/2)+x)*(((-5+sqrt(41))/2)+x))=0)&((~((2+x)=0)&~((2+x)<0))|((2+x)=0))))&((~(x=0)&~(x<0))|(x=0)))
{-1,(5/2)-(sqrt(89)/2),(5/2)+(sqrt(41)/2)}
=> {-1,(5/2)-(sqrt(89)/2),(5/2)+(sqrt(41)/2)}

? x^2-abs(x+2)+x>0
thinking...
(-abs((2+x))+(x^2)+x)>0
((~((-(-2-x)+(x^2)+x)=0)&~((-(-2-x)+(x^2)+x)<0))&((2+x)<0))|((~((-(2+x)+(x^2)+x)=0)&~((-(2+x)+(x^2)+x)<0))&((~((2+x)=0)&~((2+x)<0))|((2+x)=0)))
(((2+x)<0)&~((2+(2*x)+(x^2))=0)&~((2+(2*x)+(x^2))<0))|(~((-2+(x^2))=0)&~((-2+(x^2))<0)&((~((2+x)=0)&~((2+x)<0))|((2+x)=0)))
(((2+x)<0)&~((2+(2*x)+(x^2))=0)&~((2+(2*x)+(x^2))<0))|(~(((((2*sqrt(2))/2)+x)*(-sqrt(2)+x))=0)&~(((((2*sqrt(2))/2)+x)*(-sqrt(2)+x))<0)&((~((2+x)=0)&~((2+x)<0))|((2+x)=0)))
(-inf,-sqrt(2))U((2*sqrt(2))/2,+inf)
=> (-inf,-sqrt(2))U((2*sqrt(2))/2,+inf)

? abs(3*x-5)+abs(8-x)=abs(3+2*x)
thinking...
(abs((-5+(3*x)))+abs((8-x))-abs((3+(2*x))))=0
(((3+(2*x))<0)&((((8-x)<0)&(((((-5+(3*x))-(-3-(2*x))-(8-x))=0)&((~((-5+(3*x))=0)&~((-5+(3*x))<0))|((-5+(3*x))=0)))|(((-(-3-(2*x))-(-5+(3*x))-(8-x))=0)&((-5+(3*x))<0))))|((((((-5+(3*x))+(8-x)-(-3-(2*x)))=0)&((~((-5+(3*x))=0)&~((-5+(3*x))<0))|((-5+(3*x))=0)))|((((8-x)-(-3-(2*x))-(-5+(3*x)))=0)&((-5+(3*x))<0)))&((~((8-x)=0)&~((8-x)<0))|((8-x)=0)))))|(((((8-x)<0)&(((((-5+(3*x))-(3+(2*x))-(8-x))=0)&((~((-5+(3*x))=0)&~((-5+(3*x))<0))|((-5+(3*x))=0)))|(((-(-5+(3*x))-(3+(2*x))-(8-x))=0)&((-5+(3*x))<0))))|((((((-5+(3*x))+(8-x)-(3+(2*x)))=0)&((~((-5+(3*x))=0)&~((-5+(3*x))<0))|((-5+(3*x))=0)))|((((8-x)-(-5+(3*x))-(3+(2*x)))=0)&((-5+(3*x))<0)))&((~((8-x)=0)&~((8-x)<0))|((8-x)=0))))&((~((3+(2*x))=0)&~((3+(2*x))<0))|((3+(2*x))=0)))
(((3+(2*x))<0)&((((8-x)<0)&(((0=0)&((-5+(3*x))<0))|(((-10+(6*x))=0)&((~((-5+(3*x))=0)&~((-5+(3*x))<0))|((-5+(3*x))=0)))))|(((((16-(2*x))=0)&((-5+(3*x))<0))|(((6+(4*x))=0)&((~((-5+(3*x))=0)&~((-5+(3*x))<0))|((-5+(3*x))=0))))&((~((8-x)=0)&~((8-x)<0))|((8-x)=0)))))|(((((8-x)<0)&((((-16+(2*x))=0)&((~((-5+(3*x))=0)&~((-5+(3*x))<0))|((-5+(3*x))=0)))|(((-6-(4*x))=0)&((-5+(3*x))<0))))|((((0=0)&((~((-5+(3*x))=0)&~((-5+(3*x))<0))|((-5+(3*x))=0)))|(((10-(6*x))=0)&((-5+(3*x))<0)))&((~((8-x)=0)&~((8-x)<0))|((8-x)=0))))&((~((3+(2*x))=0)&~((3+(2*x))<0))|((3+(2*x))=0)))
(((3+(2*x))<0)&((((8-x)<0)&((((-10+(6*x))=0)&((~((-5+(3*x))=0)&~((-5+(3*x))<0))|((-5+(3*x))=0)))|(((-5+(3*x))<0)&true)))|(((((16-(2*x))=0)&((-5+(3*x))<0))|(((6+(4*x))=0)&((~((-5+(3*x))=0)&~((-5+(3*x))<0))|((-5+(3*x))=0))))&((~((8-x)=0)&~((8-x)<0))|((8-x)=0)))))|(((((8-x)<0)&((((-16+(2*x))=0)&((~((-5+(3*x))=0)&~((-5+(3*x))<0))|((-5+(3*x))=0)))|(((-6-(4*x))=0)&((-5+(3*x))<0))))|(((((10-(6*x))=0)&((-5+(3*x))<0))|(((~((-5+(3*x))=0)&~((-5+(3*x))<0))|((-5+(3*x))=0))&true))&((~((8-x)=0)&~((8-x)<0))|((8-x)=0))))&((~((3+(2*x))=0)&~((3+(2*x))<0))|((3+(2*x))=0)))
(5/3,8)U{5/3,8}
=> (5/3,8)U{5/3,8}

? abs(x^2+5*x+9)<abs(x^2+2*x+2)+abs(3*x+7)
thinking...
(abs((9+(5*x)+(x^2)))-abs((2+(2*x)+(x^2)))-abs((7+(3*x))))<0
(((7+(3*x))<0)&((((2+(2*x)+(x^2))<0)&((((9+(5*x)+(x^2))<0)&((-(-2-(2*x)-(x^2))-(-7-(3*x))-(9+(5*x)+(x^2)))<0))|((((9+(5*x)+(x^2))-(-2-(2*x)-(x^2))-(-7-(3*x)))<0)&((~((9+(5*x)+(x^2))=0)&~((9+(5*x)+(x^2))<0))|((9+(5*x)+(x^2))=0)))))|(((((9+(5*x)+(x^2))<0)&((-(-7-(3*x))-(2+(2*x)+(x^2))-(9+(5*x)+(x^2)))<0))|((((9+(5*x)+(x^2))-(-7-(3*x))-(2+(2*x)+(x^2)))<0)&((~((9+(5*x)+(x^2))=0)&~((9+(5*x)+(x^2))<0))|((9+(5*x)+(x^2))=0))))&((~((2+(2*x)+(x^2))=0)&~((2+(2*x)+(x^2))<0))|((2+(2*x)+(x^2))=0)))))|(((((2+(2*x)+(x^2))<0)&((((9+(5*x)+(x^2))<0)&((-(-2-(2*x)-(x^2))-(7+(3*x))-(9+(5*x)+(x^2)))<0))|((((9+(5*x)+(x^2))-(-2-(2*x)-(x^2))-(7+(3*x)))<0)&((~((9+(5*x)+(x^2))=0)&~((9+(5*x)+(x^2))<0))|((9+(5*x)+(x^2))=0)))))|(((((9+(5*x)+(x^2))<0)&((-(2+(2*x)+(x^2))-(7+(3*x))-(9+(5*x)+(x^2)))<0))|((((9+(5*x)+(x^2))-(2+(2*x)+(x^2))-(7+(3*x)))<0)&((~((9+(5*x)+(x^2))=0)&~((9+(5*x)+(x^2))<0))|((9+(5*x)+(x^2))=0))))&((~((2+(2*x)+(x^2))=0)&~((2+(2*x)+(x^2))<0))|((2+(2*x)+(x^2))=0))))&((~((7+(3*x))=0)&~((7+(3*x))<0))|((7+(3*x))=0)))
(((7+(3*x))<0)&((((2+(2*x)+(x^2))<0)&(((0<0)&((9+(5*x)+(x^2))<0))|(((18+(10*x)+(2*(x^2)))<0)&((~((9+(5*x)+(x^2))=0)&~((9+(5*x)+(x^2))<0))|((9+(5*x)+(x^2))=0)))))|(((((-4-(2*(x^2))-(4*x))<0)&((9+(5*x)+(x^2))<0))|(((14+(6*x))<0)&((~((9+(5*x)+(x^2))=0)&~((9+(5*x)+(x^2))<0))|((9+(5*x)+(x^2))=0))))&((~((2+(2*x)+(x^2))=0)&~((2+(2*x)+(x^2))<0))|((2+(2*x)+(x^2))=0)))))|(((((2+(2*x)+(x^2))<0)&((((-14-(6*x))<0)&((9+(5*x)+(x^2))<0))|(((4+(2*(x^2))+(4*x))<0)&((~((9+(5*x)+(x^2))=0)&~((9+(5*x)+(x^2))<0))|((9+(5*x)+(x^2))=0)))))|((((0<0)&((~((9+(5*x)+(x^2))=0)&~((9+(5*x)+(x^2))<0))|((9+(5*x)+(x^2))=0)))|(((-18-(10*x)-(2*(x^2)))<0)&((9+(5*x)+(x^2))<0)))&((~((2+(2*x)+(x^2))=0)&~((2+(2*x)+(x^2))<0))|((2+(2*x)+(x^2))=0))))&((~((7+(3*x))=0)&~((7+(3*x))<0))|((7+(3*x))=0)))
(((7+(3*x))<0)&((((2+(2*x)+(x^2))<0)&((((18+(10*x)+(2*(x^2)))<0)&((~((9+(5*x)+(x^2))=0)&~((9+(5*x)+(x^2))<0))|((9+(5*x)+(x^2))=0)))|(((9+(5*x)+(x^2))<0)&false)))|(((((-4-(2*(x^2))-(4*x))<0)&((9+(5*x)+(x^2))<0))|(((14+(6*x))<0)&((~((9+(5*x)+(x^2))=0)&~((9+(5*x)+(x^2))<0))|((9+(5*x)+(x^2))=0))))&((~((2+(2*x)+(x^2))=0)&~((2+(2*x)+(x^2))<0))|((2+(2*x)+(x^2))=0)))))|(((((2+(2*x)+(x^2))<0)&((((-14-(6*x))<0)&((9+(5*x)+(x^2))<0))|(((4+(2*(x^2))+(4*x))<0)&((~((9+(5*x)+(x^2))=0)&~((9+(5*x)+(x^2))<0))|((9+(5*x)+(x^2))=0)))))|(((((-18-(10*x)-(2*(x^2)))<0)&((9+(5*x)+(x^2))<0))|(((~((9+(5*x)+(x^2))=0)&~((9+(5*x)+(x^2))<0))|((9+(5*x)+(x^2))=0))&false))&((~((2+(2*x)+(x^2))=0)&~((2+(2*x)+(x^2))<0))|((2+(2*x)+(x^2))=0))))&((~((7+(3*x))=0)&~((7+(3*x))<0))|((7+(3*x))=0)))
(-inf,-(7/3))
=> (-inf,-(7/3))

? dif(y,x)=sqrt(4-y^2)
thinking...
(-arcsin((y/2))+x+c1)=0
=> (-arcsin((y/2))+x+c1)=0

? dif(y,x)+y=1
thinking...
(log(abs((1-y)))+x+c1)=0
=> (log(abs((1-y)))+x+c1)=0

? x^5*dif(y,x)=-y^5
thinking...
((4*(x^4)*(y^4)*c1)+(x^4)+(y^4))=0
=> ((4*(x^4)*(y^4)*c1)+(x^4)+(y^4))=0

? dif(y,x)=(1+x^2)*(1+y^2)
thinking...
((3*x)+(3*c1)-(3*arctan(y))+(x^3))=0
=> ((3*x)+(3*c1)-(3*arctan(y))+(x^3))=0

? x*(x^2-1)*dif(y,x)=1
thinking...
(integrate((1/((-1+x)*(1+x)*x)),x)-y+c1)=0
(integrate(((1/x)-(1/(2*(-1+x)))-(1/(2*(1+x)))),x)-y+c1)=0
(log(abs(x))-(log(abs((-1+x)))/2)-(log(abs((1+x)))/2)-y+c1)=0
=> ((4*log(abs(x)))+(4*c1)-(2*log(abs((-1+x))))-(2*log(abs((1+x))))-(4*y))=0

? (x^2+x*y)*dif(y,x)=(x^2+y^2)
thinking...
try(subs((integrate(((1+z)/(-1+z)),z)+log(abs(x))+c1),z,(y/x)))=0
try(subs((integrate(((1/(-1+z))+(z/(-1+z))),z)+log(abs(x))+c1),z,(y/x)))=0
try(subs((integrate((z/(-1+z)),z)+log(abs((-1+z)))+log(abs(x))+c1),z,(y/x)))=0
try(subs((log(abs((-1+z)))+log(abs(x))+try(subs(integrate(((1+a)/a),a),a,(-1+z)),subs(integrate(((-1-(1/a))/a),a),a,(1/(-1+z))))+c1),z,(y/x)))=0
try(subs((log(abs((-1+z)))+log(abs(x))+try(subs(integrate(((1+a)/a),a),a,(-1+z)),subs((-(log(abs((a^2)))/2)-integrate((1/(a^2)),a)),a,(1/(-1+z))))+c1),z,(y/x)))=0
try(subs((integrate(((1/(-1+z))+(z/(-1+z))),z)+log(abs(x))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs(integrate(((-2-(1/a))/a),a),a,(1/(-1+z))),subs(integrate(((2+a)/a),a),a,(-1+z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs(integrate((-(1/(a^2))-(2/a)),a),a,(1/(-1+z))),subs(integrate(((2/a)+(a/a)),a),a,(-1+z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs(((1-(2*log(abs(a))*a))/a),a,(1/(-1+z))),subs(((2*log(abs(a)))+a),a,(-1+z)))+c1),z,(y/x)))=0
=> ((log(abs(x))*x)+(x*c1)-(2*log(abs((x/(-x+y))))*x)-x+y)=0

? dif(y,x)=(x+y)/x
thinking...
try(subs((log(abs(x))-z+c1),z,(y/x)))=0
=> ((log(abs(x))*x)+(x*c1)-y)=0

? (x-y)*dif(y,x)-(x+y)=0
thinking...
try(subs(((log(abs((-1-(z^2))))/2)+log(abs(x))-arctan(z)+c1),z,(y/x)))=0
=> (log(abs(((-(x^2)-(y^2))/(x^2))))+(2*log(abs(x)))+(2*c1)-(2*arctan((y/x))))=0

? (x^2-y^2)+2*x*y*dif(y,x)=0
thinking...
try(subs((((2*c1)-log(abs((1+(z^2))))-log(abs(x)))/2),z,(y/x)))=0
=> ((2*c1)-log(abs((1+((y/x)^2))))-log(abs(x)))=0

? x*dif(y,x)-y+x*sin(y/x)=0
thinking...
try(subs((-log(abs(tan((z/2))))-log(abs(x))+c1),z,(y/x)))=0
=> (-log(abs((sin((y/(2*x)))/cos((y/(2*x))))))-log(abs(x))+c1)=0

? integrate((7^(7^(7^x)))*(7^(7^x))*(7^x),x)
thinking...
integrate((7^((7^(7^x))+(7^x)+x)),x)
integrate((7^(-(7^(7^x))-(7^x)-x)),x)
subs(integrate(((7^(-((2*log((log(y)/log(7))))/log(7))-((2*log(y))/log(7))-y))/(log(7)^2)),y),y,(7^(7^x)))
subs((integrate((7^((-(2*log(7)*log((log(y)/log(7))))-(2*log(7)*log(y))-((log(7)^2)*y))/(log(7)^2))),y)/(log(7)^2)),y,(7^(7^x)))
subs(integrate(((7^y)/(log(7)^2)),y),y,(7^(7^x)))
subs(((7^y)/(log(7)^3)),y,(7^(7^x)))
=> (7^(7^(7^x)))/(log(7)^3)
```