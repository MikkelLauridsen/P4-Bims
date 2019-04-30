module Main where

import Ast
import Semantics
import JVM
import Data.ByteString
import Codegen

--program = AssignmentNode "result" (AddExprNode (NumExprNode "42") (NumExprNode "42"))
{-program = CompositeNode 
    (CompositeNode 
        (AssignmentNode 
            "result"
            (MultExprNode
                (SubExprNode
                    (AddExprNode 
                        (NumExprNode "42") 
                        (NumExprNode "42"))
                    (NumExprNode "10")
                )
                (NumExprNode "3")
            )
        )
        (PrintNode (VarExprNode "result"))
    )
    (WhileNode 
        (ComparisonNode (NumExprNode "42") (NumExprNode "42"))
        (PrintNode (NumExprNode "42"))
    )
    -}
program = 
    WhileNode 
        (ComparisonNode (NumExprNode "42") (NumExprNode "42"))
        (SkipNode)
--program = PrintNode (NumExprNode "42")

bytes = getClassBytes (generateClassFile program)

main :: IO ()
main = Data.ByteString.writeFile "Main.class" bytes