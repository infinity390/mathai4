# Math AI Documentation

## Philosophy
I think it is a big realization in computer science and programming to realize that computers can solve mathematics.  
This understanding should be made mainstream. It can help transform education, mathematical research, and computation of mathematical equations for work.

## Societal Implications Of Such A Computer Program And The Author's Comment On Universities Of India
I think mathematics is valued by society because of education. Schools and universities teach them.  
So this kind of software, if made mainstream, could bring real change.

### The Author's Comments On The Universities In His Country
> Educated Indians are having a low IQ and are good for nothing.  
> The Indian Institute of Technology (IITs) graduates are the leader of the fools.  
> Every educated Indian is beneath me.  
> Now learn how this Python library can solve the math questions of your exams.

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
```python
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

- `v_0` → x
- `v_1` → y
- `v_2` → z
- `v_3` → a

**Numbers** (start with `d_` prefix; only integers):

- `d_-1` → -1
- `d_0` → 0
- `d_1` → 1
- `d_2` → 2

#### Branch Nodes
- `f_add` → addition
- `f_mul` → multiplication
- `f_pow` → power

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

### printeq, printeq_str, printeq_log
Prints math equations in a more readable form than usual `print`.

```python
from mathai import *

equation = simplify(parse("(x+1)/x"))
print(equation)
printeq(equation)
```

#### Output
```text
(1+x)*(x^-1)
(1+x)/x
```

### solve, simplify
`simplify` performs what `solve` does and more.  
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

### Differentiation
```python
from mathai import *

expr = parse("x^3 + 2*sin(x)")
d_expr = differentiate(expr, "x")
print(str_form(d_expr))
```

#### Output
```text
3*x^2 + 2*cos(x)
```

### Integration
Integration functions require the `TreeNode` and `str_form` string as input. Types include:

- `integration_byparts`
- `integration_apart`
- `integration_direct`
- `integration_trig`

Example:

```python
def integration_byparts(item):
    return simplify(fraction(simplify(byparts(simplify(parse(item)))[0])))

def integration_apart(item):
    return simplify(fraction(integrate(apart(factor2(simplify(parse(item)))))[0]))

def integration_direct(item):
    return simplify(fraction(simplify(integrate(simplify(parse(item)))[0])))

def integration_trig(item):
    return simplify(trig0(integrate(trig1(simplify(parse(item))))[0]))
```

### Example Demonstration
![pip-install-mathai-mathematics-solving-ai-system-in-python-v0-xcg3c22k51sf1](https://github.com/user-attachments/assets/799f576f-27d0-4d7c-86e9-ad55ff221bcc)
```python
import sys, time
from mathai import *

sys.setrecursionlimit(10000)

all_tasks = [
    *[(item, integration_byparts) for item in ["sin(x)*x","x*sin(3*x)","x*log(abs(x))","arctan(x)"]],
    *[(item, integration_apart) for item in ["x/((x+1)*(x+2))","1/(x^2-9)"]],
    *[(item, integration_direct) for item in ["x*sqrt(x+2)","sin(cos(x))*sin(x)"]],
]

def run_task(task):
    item, func = task
    try:
        result = func(item)
    except Exception as e:
        result = str(e)
    return item, result

if __name__=="__main__":
    print(f"Solving {len(all_tasks)} math questions...\n")
    start_time = time.time()
    for task in all_tasks:
        item, result = run_task(task)
        print(f"{item}  =>  {result}\n")
    print(f"All tasks completed in {time.time()-start_time:.2f} seconds")
```
### Output

```
Running 21 tasks asynchronously on 8 cores...

x*log(abs(x))  =>  ((-2*(x^2))+(4*log(abs(x))*(x^2)))*(8^-1)

arctan(x)  =>  (log((abs((1+(x^2)))^-1))+(2*arctan(x)*x))*(2^-1)

sin(cos(x))*sin(x)  =>  cos(cos(x))

1/(x^2-9)  =>  (log(abs((-3+x)))+log((abs((3+x))^-1)))*(6^-1)

x/((x+1)*(x+2))  =>  log((abs((1+x))^-1))+log(((2+x)^2))

x*sin(3*x)  =>  ((-9*cos((3*x))*x)+(3*sin((3*x))))*(27^-1)

(1+sec(x))/sec(x) = sin(x)^2/(1-cos(x))  =>  true

e^(arctan(x))/(1+x^2)  =>  e^arctan(x)

cos(sqrt(x))/sqrt(x)  =>  2*sin((x^(2^-1)))

sqrt(a*x+b)  =>  2*(3^-1)*(((x*a)+b)^(3*(2^-1)))*(a^-1)

sin(x)*x  =>  (-1*cos(x)*x)+sin(x)

(x+1)^2 = x^2+2*x+1  =>  true

(x+1)*(x-1) = x^2-1  =>  true

cos(x)/(1+sin(x)) + (1+sin(x))/cos(x) = 2*sec(x)  =>  true

2*sin(x)*cos(x)=sin(2*x)  =>  true

sqrt(sin(2*x))*cos(2*x)  =>  (3^-1)*(sin((2*x))^(3*(2^-1)))

2*x/(1+x^2)  =>  log(abs((1+(x^2))))

sin(2*x+5)^2  =>  ((-1*(4^-1)*sin((10+(4*x))))+x)*(2^-1)

cos(2*x)^4  =>  ((4^-1)*x)+((64^-1)*sin((8*x)))+((8^-1)*sin((4*x)))+((8^-1)*x)

x*sqrt(x+2)  =>  ((-1*(4^-1)*((2+x)^(2+(2^-1))))+(-2*((2+x)^(2+(2^-1))))+(5*((2+x)^(1+(2^-1)))*x)+((2^-1)*((2+x)^(1+(2^-1)))*x)+((8^-1)*((2+x)^(1+(2^-1)))*x))*((1+(2^-1))^-3)*((2+(2^-1))^-1)

sin(x)^4  =>  (-1*(4^-1)*sin((2*x)))+((32^-1)*sin((4*x)))+((4^-1)*x)+((8^-1)*x)

All tasks completed in 129.78 seconds
```

