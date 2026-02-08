import Base
import Parser
import Simplify
import Expand
import Trig

main :: IO ()
main = do
  print (  simplify ( trig0 ( simplify (parse "cos(pi/6)^4") )  ) )
