import Base
import Parser
import Simplify
import Expand
import Trig
import Diff
main :: IO ()
main = print $ simplify $ diff (parse "x^x") (parse "x")
