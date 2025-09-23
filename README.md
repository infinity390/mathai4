new version of math solving Ai

pip install lark-parser
the only library requirement, for the parser.py file

```
>>> 1+1
1+1
>>> simplify
2
>>> (x+1)^2 = x^2+2*x+1
((1+x)^2)=(1+((2*x)+(x^2)))
>>> simplify
(-(1+(2*x)+(x^2))+((1+x)^2))=0
>>> expand
0=0
>>> sin(x)^4
sin(x)^4
>>> trig1
(cos(0)/4)+(cos(0)/8)+(cos((4*x))/8)-((cos(0)*cos((2*x)))/2)
>>> trig0
(1/4)+(1/8)+(cos((4*x))/8)-(cos((2*x))/2)
>>> integrate x
the given question is to integrate (1/4)+(1/8)+(cos((4*x))/8)-(cos((2*x))/2) wrt to x
by integration over sums -(cos((2*x))/2), 1/4, 1/8, cos((4*x))/8
  extracted the constant -(1/2), now integrating the equation cos((2*x)) only
  result is -(sin((2*x))/4)
  extracted the constant 1/8, now integrating the equation cos((4*x)) only
  result is sin((4*x))/32
result is (sin((4*x))/32)+(x/4)+(x/8)-(sin((2*x))/4)

(sin((4*x))/32)+(x/4)+(x/8)-(sin((2*x))/4)
>>> fraction
((128*sin((4*x)))+(1536*x)-(1024*sin((2*x))))/4096
>>> simplify
((12*x)-(8*sin((2*x)))+sin((4*x)))/32
>>>
```

sample mathematics done
