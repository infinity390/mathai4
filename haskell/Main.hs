import Base
import Parser

main :: IO ()
main = do
  print (flattenTree (parse "[x*sin(x), 1]+[x,y]"))
