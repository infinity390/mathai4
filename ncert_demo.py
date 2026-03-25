!pip install mathai==0.9.4
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

"""
Collecting mathai==0.9.4
  Downloading mathai-0.9.4-py3-none-any.whl.metadata (7.7 kB)
Collecting lark-parser (from mathai==0.9.4)
  Downloading lark_parser-0.12.0-py2.py3-none-any.whl.metadata (1.7 kB)
Downloading mathai-0.9.4-py3-none-any.whl (46 kB)
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 46.4/46.4 kB 1.7 MB/s eta 0:00:00
Downloading lark_parser-0.12.0-py2.py3-none-any.whl (103 kB)
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 103.5/103.5 kB 4.0 MB/s eta 0:00:00
Installing collected packages: lark-parser, mathai
Successfully installed lark-parser-0.12.0 mathai-0.9.4
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
(try(integrate(-(1/(cos(x)*sin(x))),x),subs(integrate(-(1/((cos(arcsin(y))^2)*y)),y),y,sin(x)),subs(integrate((1/((sin(arccos(y))^2)*y)),y),y,cos(x)))+try(subs(integrate((1/((sin(arccos(z))^2)*z)),z),z,cos(y)),integrate(-(1/(cos(y)*sin(y))),y),subs(integrate(-(1/((cos(arcsin(z))^2)*z)),z),z,sin(y)))+c1)=0
(try(integrate(-(1/(cos(x)*sin(x))),x),subs(integrate(-(1/((1-(y^2))*y)),y),y,sin(x)),subs(integrate((1/((1-(y^2))*y)),y),y,cos(x)))+try(subs(integrate((1/((1-(z^2))*z)),z),z,cos(y)),integrate(-(1/(cos(y)*sin(y))),y),subs(integrate(-(1/((1-(z^2))*z)),z),z,sin(y)))+c1)=0
(try(-integrate((1/(cos(x)*sin(x))),x),subs(-integrate((1/((1-(y^2))*y)),y),y,sin(x)),subs(integrate((1/((1-(y^2))*y)),y),y,cos(x)))+try(subs(integrate((1/((1-(z^2))*z)),z),z,cos(y)),-integrate((1/(cos(y)*sin(y))),y),subs(-integrate((1/((1-(z^2))*z)),z),z,sin(y)))+c1)=0
(try(-integrate((1/(cos(x)*sin(x))),x),subs(-integrate(-(1/((-1+y)*(1+y)*y)),y),y,sin(x)),subs(integrate(-(1/((-1+y)*(1+y)*y)),y),y,cos(x)))+try(subs(integrate(-(1/((-1+z)*(1+z)*z)),z),z,cos(y)),-integrate((1/(cos(y)*sin(y))),y),subs(-integrate(-(1/((-1+z)*(1+z)*z)),z),z,sin(y)))+c1)=0
(try(-integrate((1/(cos(x)*sin(x))),x),subs(--integrate((1/((-1+y)*(1+y)*y)),y),y,sin(x)),subs(-integrate((1/((-1+y)*(1+y)*y)),y),y,cos(x)))+try(subs(-integrate((1/((-1+z)*(1+z)*z)),z),z,cos(y)),-integrate((1/(cos(y)*sin(y))),y),subs(--integrate((1/((-1+z)*(1+z)*z)),z),z,sin(y)))+c1)=0
(try(-integrate((1/(cos(x)*sin(x))),x),subs(integrate(((1/(2*(-1+y)))+(1/(2*(1+y)))-(1/y)),y),y,sin(x)),subs(-integrate(((1/(2*(-1+y)))+(1/(2*(1+y)))-(1/y)),y),y,cos(x)))+try(subs(-integrate(((1/(2*(-1+z)))+(1/(2*(1+z)))-(1/z)),z),z,cos(y)),-integrate((1/(cos(y)*sin(y))),y),subs(integrate(((1/(2*(-1+z)))+(1/(2*(1+z)))-(1/z)),z),z,sin(y)))+c1)=0
(try(-integrate((1/(cos(x)*sin(x))),x),subs(((integrate((1/(2*(-1+y))),y)+integrate(-(1/y),y))+integrate((1/(2*(1+y))),y)),y,sin(x)),subs(-(integrate((1/(2*(-1+y))),y)+integrate((1/(2*(1+y))),y)+integrate(-(1/y),y)),y,cos(x)))+try(subs(-(integrate((1/(2*(-1+z))),z)+integrate((1/(2*(1+z))),z)+integrate(-(1/z),z)),z,cos(y)),-integrate((1/(cos(y)*sin(y))),y),subs(((integrate((1/(2*(-1+z))),z)+integrate(-(1/z),z))+integrate((1/(2*(1+z))),z)),z,sin(y)))+c1)=0
(try(-integrate((1/(cos(x)*sin(x))),x),subs((((integrate((1/(-1+y)),y)/2)-integrate((1/y),y))+(integrate((1/(1+y)),y)/2)),y,sin(x)),subs(-((integrate((1/(-1+y)),y)/2)+(integrate((1/(1+y)),y)/2)-integrate((1/y),y)),y,cos(x)))+try(subs(-((integrate((1/(-1+z)),z)/2)+(integrate((1/(1+z)),z)/2)-integrate((1/z),z)),z,cos(y)),-integrate((1/(cos(y)*sin(y))),y),subs((((integrate((1/(-1+z)),z)/2)-integrate((1/z),z))+(integrate((1/(1+z)),z)/2)),z,sin(y)))+c1)=0
(try(-integrate((1/(cos(x)*sin(x))),x),subs((((log(abs((-1+y)))/2)-log(abs(y)))+(log(abs((1+y)))/2)),y,sin(x)),subs(-((log(abs((-1+y)))/2)+(log(abs((1+y)))/2)-log(abs(y))),y,cos(x)))+try(subs(-((log(abs((-1+z)))/2)+(log(abs((1+z)))/2)-log(abs(z))),z,cos(y)),-integrate((1/(cos(y)*sin(y))),y),subs((((log(abs((-1+z)))/2)-log(abs(z)))+(log(abs((1+z)))/2)),z,sin(y)))+c1)=0
((((log(abs((-1+sin(x))))/2)-log(abs(sin(x))))+(log(abs((1+sin(x))))/2))-((log(abs((-1+cos(y))))/2)+(log(abs((1+cos(y))))/2)-log(abs(cos(y))))+c1)=0

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
try(subs((-(integrate((((1/sqrt(2))/(-(sqrt(2)/2)+z))-((1/sqrt(2))/((sqrt(2)/2)+z))),z)/2)-log(abs(x))+c1),z,(y/x)))=0
try(subs((-((integrate(((1/sqrt(2))/(-(sqrt(2)/2)+z)),z)+integrate(-((1/sqrt(2))/((sqrt(2)/2)+z)),z))/2)-log(abs(x))+c1),z,(y/x)))=0
try(subs((-(((2*(1/sqrt(8))*integrate((1/(-(sqrt(2)/2)+z)),z))-(2*(1/sqrt(8))*integrate((1/((sqrt(2)/2)+z)),z)))/2)-log(abs(x))+c1),z,(y/x)))=0
try(subs((-(((2*(1/sqrt(8))*log(abs((-(sqrt(2)/2)+z))))-(2*(1/sqrt(8))*log(abs(((sqrt(2)/2)+z)))))/2)-log(abs(x))+c1),z,(y/x)))=0
(-(((2*(1/sqrt(8))*log(abs(((y/x)-(sqrt(2)/2)))))-(2*(1/sqrt(8))*log(abs(((sqrt(2)/2)+(y/x))))))/2)-log(abs(x))+c1)=0

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
(integrate(((1/(2*(-1+x)))+(1/(2*(1+x)))-(1/x)),x)-y+c1)=0
(((integrate((1/(2*(-1+x))),x)+integrate(-(1/x),x))+integrate((1/(2*(1+x))),x))-y+c1)=0
((((integrate((1/(-1+x)),x)/2)-integrate((1/x),x))+(integrate((1/(1+x)),x)/2))-y+c1)=0
((((log(abs((-1+x)))/2)-log(abs(x)))+(log(abs((1+x)))/2))-y+c1)=0

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
try(subs((log(abs(x))+try(subs(integrate(((2+a)/a),a),a,(-1+z)),integrate(((1+z)/(-1+z)),z))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs(integrate(((2/a)+(a/a)),a),a,(-1+z)),integrate(((1/(-1+z))+(z/(-1+z))),z))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs(integrate((1+(2/a)),a),a,(-1+z)),integrate(((1/(-1+z))+(z/(-1+z))),z))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs((integrate(1,a)+integrate((2/a),a)),a,(-1+z)),(integrate((1/(-1+z)),z)+integrate((z/(-1+z)),z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs((integrate(1,a)+(2*integrate((1/a),a))),a,(-1+z)),(integrate((1/(-1+z)),z)+integrate((z/(-1+z)),z)))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs(((1*a)+(2*log(abs(a)))),a,(-1+z)),(integrate((z/(-1+z)),z)+log(abs((-1+z)))))+c1),z,(y/x)))=0
try(subs((log(abs(x))+try(subs(((2*log(abs(a)))+a),a,(-1+z)),(integrate((z/(-1+z)),z)+log(abs((-1+z)))))+c1),z,(y/x)))=0
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
integrate(((2/(2+x))-(1/(1+x))),x)
integrate((2/(2+x)),x)+integrate(-(1/(1+x)),x)
(2*integrate((1/(2+x)),x))-integrate((1/(1+x)),x)
(2*log(abs((2+x))))-log(abs((1+x)))

===========
question 24
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  2 )
solution with steps:

integrate(1/(x^2-9),x)
integrate((1/((x^2)-9)),x)
integrate((1/((-3+x)*(3+x))),x)
integrate(((1/(6*(-3+x)))-(1/(6*(3+x)))),x)
integrate(((1/(6*(-3+x)))-(1/(6*(3+x)))),x)
integrate((1/(6*(-3+x))),x)+integrate(-(1/(6*(3+x))),x)
(integrate((1/(-3+x)),x)/6)-(integrate((1/(3+x)),x)/6)
(log(abs((-3+x)))/6)-(log(abs((3+x)))/6)

===========
question 25
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  3 )
solution with steps:

integrate((3*x-1)/((x-1)*(x-2)*(x-3)),x)
integrate((((3*x)-1)/(((x-1)*(x-2))*(x-3))),x)
integrate(((-1+(3*x))/((-1+x)*(-2+x)*(-3+x))),x)
integrate(((1/(-1+x))+(4/(-3+x))-(5/(-2+x))),x)
(integrate((4/(-3+x)),x)+integrate(-(5/(-2+x)),x))+integrate((1/(-1+x)),x)
((4*integrate((1/(-3+x)),x))-(5*integrate((1/(-2+x)),x)))+integrate((1/(-1+x)),x)
((4*log(abs((-3+x))))-(5*log(abs((-2+x)))))+log(abs((-1+x)))

===========
question 26
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  4 )
solution with steps:

integrate(x/((x-1)*(x-2)*(x-3)),x)
integrate((x/(((x-1)*(x-2))*(x-3))),x)
integrate((x/((-1+x)*(-2+x)*(-3+x))),x)
integrate(((1/(2*(-1+x)))+(3/(2*(-3+x)))-(2/(-2+x))),x)
integrate(((1/(2*(-1+x)))+(3/(2*(-3+x)))-(2/(-2+x))),x)
(integrate((3/(2*(-3+x))),x)+integrate(-(2/(-2+x)),x))+integrate((1/(2*(-1+x))),x)
(((3/2)*integrate((1/(-3+x)),x))-(2*integrate((1/(-2+x)),x)))+(integrate((1/(-1+x)),x)/2)
(((3/2)*log(abs((-3+x))))-(2*log(abs((-2+x)))))+(log(abs((-1+x)))/2)

===========
question 27
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  5 )
solution with steps:

integrate(2*x/(x^2+3*x+2),x)
integrate(((2*x)/(2+((3*x)+(x^2)))),x)
integrate(((2*x)/(2+(3*x)+(x^2))),x)
integrate(((2*x)/((1+x)*(2+x))),x)
integrate(((4/(2+x))-(2/(1+x))),x)
integrate((4/(2+x)),x)+integrate(-(2/(1+x)),x)
(4*integrate((1/(2+x)),x))-(2*integrate((1/(1+x)),x))
(4*log(abs((2+x))))-(2*log(abs((1+x))))

===========
question 28
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  6 )
solution with steps:

integrate((1-x^2)/(x*(1-2*x)),x)
integrate(((1-(x^2))/((1-(2*x))*x)),x)
integrate(((1-(x^2))/((1-(2*x))*x)),x)
integrate((((1-x)*(1+x))/((1-(2*x))*x)),x)
integrate(((1/x)+(2/(1-(2*x)))),x)
integrate((1/x),x)+integrate((2/(1-(2*x))),x)
integrate((1/x),x)+(2*integrate((1/(1-(2*x))),x))
log(abs(x))+(2*-(log(abs((1-(2*x))))/2))

===========
question 29
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  7 )
solution with steps:

integrate(x/((x-1)*(x^2+1)),x)
integrate((x/((1+(x^2))*(x-1))),x)
integrate((x/((-1+x)*(1+(x^2)))),x)
integrate(((1/(2*(-1+x)))+(((1/2)-(x/2))/(1+(x^2)))),x)
integrate(((1/(2*(-1+x)))+(((1/2)-(x/2))/(1+(x^2)))),x)
integrate((1/(2*(-1+x))),x)+integrate((((1/2)-(x/2))/(1+(x^2))),x)
(integrate((1/(-1+x)),x)/2)+integrate((((1/2)-(x/2))/(1+(x^2))),x)
(log(abs((-1+x)))/2)+integrate((((1/2)-(x/2))/(1+(x^2))),x)
(log(abs((-1+x)))+(2*integrate(((1/(2*(1+(x^2))))-(x/(2*(1+(x^2))))),x)))/2
(log(abs((-1+x)))/2)+((2*integrate(((1/(2*(1+(x^2))))-(x/(2*(1+(x^2))))),x))/2)
(log(abs((-1+x)))/2)+integrate(((1/(2*(1+(x^2))))-(x/(2*(1+(x^2))))),x)
(integrate((1/(2*(1+(x^2)))),x)+integrate(-(x/(2*(1+(x^2)))),x))+(log(abs((-1+x)))/2)
((integrate((1/(1+(x^2))),x)/2)-(integrate((x/(1+(x^2))),x)/2))+(log(abs((-1+x)))/2)
(integrate((1/(1+(x^2))),x)/2)+(log(abs((-1+x)))/2)-(integrate((x/(1+(x^2))),x)/2)
(arctan(x)/2)+(log(abs((-1+x)))/2)-(log(abs((1+(x^2))))/4)

===========
question 30
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  8 )
solution with steps:

integrate(x/((x-1)^2*(x+2)),x)
integrate((x/((2+x)*((x-1)^2))),x)
integrate((x/((2+x)*((-1+x)^2))),x)
integrate(((1/(3*((-1+x)^2)))+(2/(9*(-1+x)))-(2/(9*(2+x)))),x)
integrate(((1/(3*((-1+x)^2)))+(2/(9*(-1+x)))-(2/(9*(2+x)))),x)
(integrate((2/(9*(-1+x))),x)+integrate(-(2/(9*(2+x))),x))+integrate((1/(3*((-1+x)^2))),x)
(((2/9)*integrate((1/(-1+x)),x))-((2*integrate((1/(2+x)),x))/9))+(integrate((1/((-1+x)^2)),x)/3)
(((2/9)*log(abs((-1+x))))-((2*log(abs((2+x))))/9))+(-(1/(-1+x))/3)

===========
question 31
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  10 )
solution with steps:

integrate((2*x-3)/((x^2-1)*(2*x+3)),x)
integrate((((2*x)-3)/((3+(2*x))*((x^2)-1))),x)
integrate(((-3+(2*x))/((-1+(x^2))*(3+(2*x)))),x)
integrate(((-3+(2*x))/((-1+x)*(1+x)*(3+(2*x)))),x)
integrate(((5/(2*(1+x)))-(1/(10*(-1+x)))-(24/(5*(3+(2*x))))),x)
integrate(((5/(2*(1+x)))-(1/(10*(-1+x)))-(24/(5*(3+(2*x))))),x)
(integrate(-(1/(10*(-1+x))),x)+integrate(-(24/(5*(3+(2*x)))),x))+integrate((5/(2*(1+x))),x)
(-(integrate((1/(-1+x)),x)/10)-((24*integrate((1/(3+(2*x))),x))/5))+((5/2)*integrate((1/(1+x)),x))
(-(log(abs((-1+x)))/10)-((12*log(abs((3+(2*x)))))/5))+((5/2)*log(abs((1+x))))

===========
question 32
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  11 )
solution with steps:

integrate(5*x/((x+1)*(x^2-4)),x)
integrate(((5*x)/((1+x)*((x^2)-4))),x)
integrate(((5*x)/((-4+(x^2))*(1+x))),x)
integrate(((5*x)/((-2+x)*(1+x)*(2+x))),x)
integrate(((5/(3*(1+x)))+(5/(6*(-2+x)))-(5/(2*(2+x)))),x)
integrate(((5/(3*(1+x)))+(5/(6*(-2+x)))-(5/(2*(2+x)))),x)
(integrate((5/(3*(1+x))),x)+integrate(-(5/(2*(2+x))),x))+integrate((5/(6*(-2+x))),x)
(((5/3)*integrate((1/(1+x)),x))-((5*integrate((1/(2+x)),x))/2))+((5/6)*integrate((1/(-2+x)),x))
(((5/3)*log(abs((1+x))))-((5*log(abs((2+x))))/2))+((5/6)*log(abs((-2+x))))

===========
question 33
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.5, QUESTION  13 )
solution with steps:

integrate(2/((1-x)*(1+x^2)),x)
integrate((2/((1+(x^2))*(1-x))),x)
integrate((2/((1-x)*(1+(x^2)))),x)
integrate(((1/(1-x))+((1+x)/(1+(x^2)))),x)
integrate((1/(1-x)),x)+integrate(((1+x)/(1+(x^2))),x)
integrate(((1+x)/(1+(x^2))),x)-log(abs((1-x)))
integrate(((1/(1+(x^2)))+(x/(1+(x^2)))),x)-log(abs((1-x)))
integrate(((1/(1+(x^2)))+(x/(1+(x^2)))),x)-log(abs((1-x)))
(integrate((1/(1+(x^2))),x)+integrate((x/(1+(x^2))),x))-log(abs((1-x)))
integrate((1/(1+(x^2))),x)+integrate((x/(1+(x^2))),x)-log(abs((1-x)))
arctan(x)+(log(abs((1+(x^2))))/2)-log(abs((1-x)))

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
2*try(integrate((x/(1+(x^2))),x),subs(integrate((1/(2*(1+y))),y),y,(x^2)))
2*try(integrate((x/(1+(x^2))),x),subs((integrate((1/(1+y)),y)/2),y,(x^2)))
2*try(integrate((x/(1+(x^2))),x),subs((log(abs((1+y)))/2),y,(x^2)))
2*try(integrate((x/(1+(x^2))),x),subs((log(abs((1+y)))/2),y,(x*x)))
2*try(integrate((x/(1+(x^2))),x),subs((log(abs((1+y)))/2),y,(x^2)))
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
try(subs(integrate(((-4+y)*(1/sqrt(y))),y),y,(4+x)),integrate(((1/sqrt((4+x)))*x),x))
try(subs(integrate((((1/sqrt(y))*y)-(4*(1/sqrt(y)))),y),y,(4+x)),integrate(((1/sqrt((4+x)))*x),x))
try(subs((integrate(((1/sqrt(y))*y),y)+integrate(-(4*(1/sqrt(y))),y)),y,(4+x)),integrate(((1/sqrt((4+x)))*x),x))
try(subs((integrate(((1/sqrt(y))*y),y)-(4*integrate((1/sqrt(y)),y))),y,(4+x)),integrate(((1/sqrt((4+x)))*x),x))
try(subs((((2*(y^(3/2)))/3)-(4*sqrt((4*y)))),y,(4+x)),integrate(((1/sqrt((4+x)))*x),x))
try(subs((((2*(y^(3/2)))-sqrt((576*y)))/3),y,(4+x)),integrate(((1/sqrt((4+x)))*x),x))
try(subs(((2*(-sqrt((144*y))+(y^(3/2))))/3),y,(4+x)),integrate(((1/sqrt((4+x)))*x),x))
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
try(subs(integrate(((e^-y)/2),y),y,(x^2)),integrate(((e^-(x^2))*x),x))
try(subs((integrate((e^-y),y)/2),y,(x^2)),integrate(((e^-(x^2))*x),x))
try(subs((-(e^-y)/2),y,(x^2)),integrate(((e^-(x^2))*x),x))
try(subs(-((e^-y)/2),y,(x*x)),integrate(((e^-(x^2))*x),x))
try(subs(-((e^-y)/2),y,(x^2)),integrate(((e^-(x^2))*x),x))
try(subs(-((e^-y)/2),y,(x^2)),integrate(((e^-(x^2))*x),x))
-((e^-(x^2))/2)

===========
question 50
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.2, QUESTION  27 )
solution with steps:

integrate(sqrt(sin(2*x))*cos(2*x),x)
integrate((cos((2*x))*sqrt(sin((2*x)))),x)
try(subs(integrate(-(((1/sqrt(sin(arccos(y))))*y)/2),y),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs(integrate(((cos(y)*sqrt(sin(y)))/2),y),y,(2*x)),subs(integrate((sqrt(y)/2),y),y,sin((2*x))))
try(subs(integrate(-(((1/sqrt(sin(arccos(y))))*y)/2),y),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs(integrate(((cos(y)*sqrt(sin(y)))/2),y),y,(2*x)),subs(integrate((sqrt(y)/2),y),y,sin((2*x))))
try(subs(-(integrate(((1/sqrt(sin(arccos(y))))*y),y)/2),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs((integrate((cos(y)*sqrt(sin(y))),y)/2),y,(2*x)),subs((integrate(sqrt(y),y)/2),y,sin((2*x))))
try(subs(-(integrate(((1/sqrt(sin(arccos(y))))*y),y)/2),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs((integrate((cos(y)*sqrt(sin(y))),y)/2),y,(2*x)),subs((integrate(sqrt(y),y)/2),y,sin((2*x))))
try(subs(-(integrate(((1/sqrt(sin(arccos(y))))*y),y)/2),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs((integrate((cos(y)*sqrt(sin(y))),y)/2),y,(2*x)),subs((((2*(y^(3/2)))/3)/2),y,sin((2*x))))
try(subs(-(integrate(((1/sqrt(sin(arccos(y))))*y),y)/2),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs((integrate((cos(y)*sqrt(sin(y))),y)/2),y,(2*x)),subs(((2*(y^(3/2)))/6),y,sin((2*x))))
try(subs(-(integrate(((1/sqrt(sin(arccos(y))))*y),y)/2),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs((integrate((cos(y)*sqrt(sin(y))),y)/2),y,(2*x)),subs(((y^(3/2))/3),y,sin((2*x))))
try(subs(-(integrate(((1/sqrt(sin(arccos(y))))*y),y)/2),y,cos((2*x))),integrate((cos((2*x))*sqrt(sin((2*x)))),x),subs((integrate((cos(y)*sqrt(sin(y))),y)/2),y,(2*x)),subs(((y^(3/2))/3),y,sin((2*x))))
(sin((2*x))^(3/2))/3

===========
question 51
===========
( CLASS 12 NCERT MATHS PART-II CHAPTER 7 EXERCISE 7.2, QUESTION  3 )
solution with steps:

integrate(sin(x)*sin(cos(x)),x)
integrate((sin(cos(x))*sin(x)),x)
try(subs(integrate(((sin(cos(arcsin(y)))*y)/cos(arcsin(y))),y),y,sin(x)),subs(integrate(-sin(y),y),y,cos(x)),subs(integrate(-(sin(cos(arcsin(y)))/cos(cos(arcsin(y)))),y),y,sin(cos(x))),integrate((sin(cos(x))*sin(x)),x))
try(subs(integrate(((sin(cos(arcsin(y)))*y)/cos(arcsin(y))),y),y,sin(x)),subs(-integrate(sin(y),y),y,cos(x)),subs(-integrate((sin(cos(arcsin(y)))/cos(cos(arcsin(y)))),y),y,sin(cos(x))),integrate((sin(cos(x))*sin(x)),x))
try(subs(integrate(((sin(cos(arcsin(y)))*y)/cos(arcsin(y))),y),y,sin(x)),subs(--cos(y),y,cos(x)),subs(-integrate((sin(cos(arcsin(y)))/cos(cos(arcsin(y)))),y),y,sin(cos(x))),integrate((sin(cos(x))*sin(x)),x))
try(subs(integrate(((sin(cos(arcsin(y)))*y)/cos(arcsin(y))),y),y,sin(x)),subs(--cos(y),y,cos(x)),subs(-integrate((sin(cos(arcsin(y)))/cos(cos(arcsin(y)))),y),y,sin(cos(x))),integrate((sin(cos(x))*sin(x)),x))
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
(-try(integrate((1/sqrt((4-(y^2)))),y),subs(integrate((((1/sqrt((4-z)))*(1/sqrt(z)))/2),z),z,(y^2)))+x+c1)=0
(-try(integrate((1/sqrt((4-(y^2)))),y),subs((integrate(((1/sqrt((4-z)))*(1/sqrt(z))),z)/2),z,(y^2)))+x+c1)=0
(-try(arcsin((y/2)),subs((arcsin(((-2+z)/2))/2),z,(y^2)))+x+c1)=0
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
"""
