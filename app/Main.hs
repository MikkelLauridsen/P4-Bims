module Main where

import Ast
import JVM
import Data.ByteString
import Codegen

-- Factorial of 5
program = CompositeNode 
    (CompositeNode 
        (AssignmentNode 
            "x" (NumExprNode "1"))
        (AssignmentNode 
            "n" (NumExprNode "10"))) 
    (WhileNode 
        (GreaterThanNode 
            (VarExprNode "n") 
            (NumExprNode "1")) 
        (CompositeNode 
            (AssignmentNode 
                "x"  
                (MultExprNode 
                    (VarExprNode "n") 
                    (VarExprNode "x"))) 
            (CompositeNode
                (AssignmentNode 
                    "n" 
                    (SubExprNode 
                        (VarExprNode "n") 
                        (NumExprNode "1")))
                (PrintNode (VarExprNode "x")))))

bytes = getClassBytes (generateClassFile program)

main :: IO ()
main = Data.ByteString.writeFile "Main.class" bytes