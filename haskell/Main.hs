import Base
import Parser
import Simplify
import Expand
import Trig
import Diff
import Fraction
import Limit
main :: IO ()
main = print $ simplify $ limit1 (parse "limit((1-cos(x))/x^2,x)")