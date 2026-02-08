import Base
import Parser
import Simplify
import Expand
import Trig

main :: IO ()
main = do
  print (dowhile (trig1(parse "sin(2*x)^12")) (\x ->trig0( simplify (expand x "*"))))
