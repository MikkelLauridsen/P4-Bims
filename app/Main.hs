module Main where

import Ast
import JVM
import Data.ByteString
import Codegen
import Control.Exception

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
main = do
    e <- try (Data.ByteString.writeFile "Main.class" bytes) :: IO (Either IOException ())
    case e of
        (Right e) -> Prelude.putStrLn "Generated Main.class successfully"
        (Left e)  -> Prelude.putStrLn "Failed to create Main.class"