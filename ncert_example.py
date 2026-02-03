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
set9 = ("x*dif(y,x)-y+x*sin(y/x)=0; # 8", "x^2*dif(y,x)=x^2-2*y^2+x*y; # 5")
set8, set9 = [tuple([x.replace("#", title) for x in item]) for item in [set8, set9]]
title = "class 12 ncert maths part-II chapter 9 exercise 9.5, question "
set10 = ("(x+y)*dif(y,x) = 1; # 10", )
set10 = tuple([x.replace("#", title) for x in set10])
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
       factor2, factor1, simplify, apart, integrate_summation, integrate_formula, integrate_const, integrate_formula]
lst3 = [parse, simplify, trig0, fraction, ode_solve, integrate_const,
       integrate_formula, simplify, integrate_byparts, trig4, simplify,
       integrate_formula, integrate_summation, fraction,
       simplify, integrate_const, simplify, integrate_fraction, integrate_formula]
lst4 = [parse, simplify, trig0, fraction, fraction, simplify, ode_solve, integrate_const,
       integrate_formula, simplify, integrate_subs, simplify, trig4,
       integrate_const, factor2, apart, integrate_summation, integrate_const,
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
question = {set10: lst3, set9: lst2, set2: lst2, set8: lst1, set7: lst7, set6: lst6, set5: lst5, set4:lst4, set3: lst3, set1: lst1}
for key in question.keys():
  lst = question[key]
  for eq in list(key):
    if ";" in eq:
      eq, source = eq.split(";")
      print(f"===========\nquestion {count}\n===========\n({source.upper()} )\nsolution with steps:\n")
    else:
      print(f"===========\nquestion {count}\n===========\nsolution with steps:\n")
    count += 1
    old = None
    for func in lst+[lambda x: x]:
      if old != eq:
        print(eq)
      old = eq
      if "f_integrate" not in str_form(eq) and "f_dif" not in str_form(eq) and "f_try" in str_form(eq):
        eq = integrate_clean(eq)
        print(eq)
        break
      eq = func(eq)
      if all("f_"+x not in str_form(eq) for x in "dif integrate subs try".split(" ")):
        print(eq)
        break
    if any("f_"+x in str_form(eq) for x in "dif integrate subs try".split(" ")):
      eq = integrate_clean(eq)
      print(eq)
      if any("f_"+x in str_form(eq) for x in "dif integrate subs try".split(" ")):
        raise Exception("solution failed")
    print()
