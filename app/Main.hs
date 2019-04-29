module Main where

import Ast
import Semantics
import JVM
import Data.ByteString
import Codegen

--program = AssignmentNode "result" (AddExprNode (NumExprNode "42") (NumExprNode "42"))
program = CompositeNode (AssignmentNode "result" (NumExprNode "300")) (PrintNode (VarExprNode "result"))
--program = PrintNode (NumExprNode "42")

bytes = getClassBytes (generateClassFile program)

main :: IO ()
main = Data.ByteString.writeFile "Main.class" bytes