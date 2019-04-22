module Main where

import Ast
import Semantics
import JVM

program = AssignmentNode "result" (AddExprNode (NumExprNode "42") (NumExprNode "42"))

main :: IO ()
main = putStrLn (show (interpret program))

