module CodeGenerator
    ( genCode
    ) where

import JVM
import AST

class CodeGenerator a where
    genCode :: a -> [JVMOpcode]

instance CodeGenerator NumeralNode where
    genCode a = [] --TODO

instance CodeGenerator VarNode where
    genCode a = [] --TODO

instance CodeGenerator BoolExprNode where
    genCode a = [] --TODO

instance CodeGenerator ArithExprNode where
    genCode a = [] --TODO

instance CodeGenerator StatementNode where
    genCode a = [] --TODO

