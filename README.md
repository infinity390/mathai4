<h1>Math AI Documentation</h1>

<h2>Philosophy</h2>
<p>
I think it is a big realization in computer science and programming to realize that computers can solve mathematics.
This understanding should be made mainstream. It can help transform education, mathematical research, 
and computation of mathematical equations for work.
</p>

<h2>Societal Implications Of Such A Computer Program And The Author's Comment On Universities Of India</h2>
<p>
I think mathematics is valued by society because of education. Schools and universities teach them. 
So this kind of software, if made mainstream, could bring real change.
</p>

<h3>The Author's Comments On The Universities In His Country</h3>
<blockquote>Educated Indians are having a low IQ and are good for nothing.</blockquote>
<blockquote>The Indian Institute of Technology (IITs) graduates are the leader of the fools.</blockquote>
<blockquote>Every educated Indian is beneath me.</blockquote>
<blockquote>Now learn how this Python library can solve the math questions of your exams.</blockquote>

<h2>The Summary Of How Computer "Solves" Math</h2>
<p>
Math equations are a tree data structure (<code>TreeNode</code> class).  
We can manipulate the math equations using various algorithms (functions provided by the <code>mathai</code> library).  
We first parse the math equation strings to get the tree data structure (<code>parse</code> function in <code>mathai</code>).
</p>

<h2>The Library</h2>
<p>Import the library by doing:</p>
<pre><code class="language-python">from mathai import *</code></pre>

<h3>str_form</h3>
<p>It is the string representation of a <code>TreeNode</code> math equation.</p>

<h4>Example</h4>
<pre><code class="language-python">(cos(x)^2)+(sin(x)^2)</code></pre>

<p>is the same as:</p>
<pre><code class="language-python">f_add
 f_pow
  f_cos
   v_0
  d_2
 f_pow
  f_sin
   v_0
  d_2
</code></pre>

<h4>Leaf Nodes</h4>
<h5>Variables</h5>
<ul>
  <li><code>v_0</code> → x</li>
  <li><code>v_1</code> → y</li>
  <li><code>v_2</code> → z</li>
  <li><code>v_3</code> → a</li>
</ul>

<h5>Numbers</h5>
<p>Start with a <code>d_</code> prefix. Only integers are allowed.</p>
<ul>
  <li><code>d_-1</code> → -1</li>
  <li><code>d_0</code> → 0</li>
  <li><code>d_1</code> → 1</li>
  <li><code>d_2</code> → 2</li>
</ul>

<h4>Branch Nodes</h4>
<ul>
  <li><code>f_add</code> → addition</li>
  <li><code>f_mul</code> → multiplication</li>
  <li><code>f_pow</code> → power</li>
</ul>

<h3>parse</h3>
<p>Takes in a math equation string and outputs a <code>TreeNode</code> object.</p>
<pre><code class="language-python">from mathai import *
equation = parse("sin(x)^2+cos(x)^2")
print(equation)
</code></pre>

<h4>Output</h4>
<pre><code class="language-python">(cos(x)^2)+(sin(x)^2)</code></pre>

<h3>printeq, printeq_str, printeq_log</h3>
<p>Prints math equations in a more readable form than usual <code>print</code>.</p>
<pre><code class="language-python">from mathai import *
equation = simplify(parse("(x+1)/x"))
print(equation)
printeq(equation)
</code></pre>

<h4>Output</h4>
<pre><code class="language-python">(1+x)*(x^-1)
(1+x)/x
</code></pre>

<h3>solve, simplify</h3>
<p>
<code>simplify</code> performs what <code>solve</code> does, because it calls it internally and does more.  
It simplifies and cleans up a given math equation.
</p>
<pre><code class="language-python">from mathai import *
equation = simplify(parse("(x+x+x+x-1-1-1-1)*(4*x-4)*sin(sin(x+x+x)*sin(3*x))"))
printeq(equation)
</code></pre>

<h4>Output</h4>
<pre><code class="language-python">((-4+(4*x))^2)*sin((sin((3*x))^2))</code></pre>

<h2>demonstration</h2>
<pre><code class="language-python">
import sys, os, time
from mathai import *

sys.setrecursionlimit(10000)

def integration_byparts(item):
    return simplify(fraction(simplify(byparts(simplify(parse(item)))[0])))

def integration_apart(item):
    return simplify(fraction(integrate(apart(factor2(simplify(parse(item)))))[0]))

def integration_direct(item):
    return simplify(fraction(simplify(integrate(simplify(parse(item)))[0])))

def integration_trig(item):
    return simplify(trig0(integrate(trig1(simplify(parse(item))))[0]))

def algebra(item):
    return logic0(simplify(expand(simplify(parse(item)))))

def trig_basic(item):
    return logic0(simplify(expand(trig3(simplify(parse(item))))))

def trig_advanced(item):
    return logic0(simplify(
        trig0(
            trig1(
                trig4(
                    simplify(
                        fraction(
                            trig0(
                                simplify(parse(item))
                            )
                        )
                    )
                )
            )
        )
    ))

all_tasks = [
    *[(item, trig_advanced) for item in [
        "cos(x)/(1+sin(x)) + (1+sin(x))/cos(x) = 2*sec(x)",
        "(1+sec(x))/sec(x) = sin(x)^2/(1-cos(x))"]],
    
    *[(item, integration_byparts) for item in [
        "sin(x)*x",
        "x*sin(3*x)",
        "x*log(abs(x))",
        "arctan(x)"]],
    
    *[(item, integration_apart) for item in [
        "x/((x+1)*(x+2))",
        "1/(x^2-9)"]],
    
    *[(item, integration_direct) for item in [
        "x*sqrt(x+2)",
        "sin(cos(x))*sin(x)",
        "2*x/(1+x^2)",
        "sqrt(a*x+b)",
        "cos(sqrt(x))/sqrt(x)",
        "e^(arctan(x))/(1+x^2)",
        "sqrt(sin(2*x))*cos(2*x)"]],
    
    *[(item, integration_trig) for item in [
        "sin(2*x+5)^2",
        "sin(x)^4",
        "cos(2*x)^4"]],
    
    *[(item, algebra) for item in [
        "(x+1)^2 = x^2+2*x+1",
        "(x+1)*(x-1) = x^2-1"]],
    
    *[(item, trig_basic) for item in [
        "2*sin(x)*cos(x)=sin(2*x)"]],
]

def run_task(task):
    item, func = task
    try:
        result = func(item)
    except Exception as e:
        result = str(e)
    return item, result

if __name__ == "__main__":
    print(f"Solving {len(all_tasks)} math questions...\n")
    start_time = time.time()
    for task in all_tasks:
        item, result = run_task(task)
        print(f"{item}  =>  {result}\n")
    print(f"All tasks completed in {time.time()-start_time:.2f} seconds")
</code></pre>
