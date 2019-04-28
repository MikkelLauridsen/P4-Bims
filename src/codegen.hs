module Codegen
    ( genCodeStatement
    ) where

import JVM
import Ast
import Data.Map.Strict as Map

type ConstantPool = (Map NumeralNode UInt16, [PoolConstant], UInt16)

--getVar env var = Map.lookup var env
getConstantIndex :: NumeralNode -> ConstantPool -> (UInt16, ConstantPool)
getConstantIndex num cPool@(indexMap, elements, poolSize) =
    case Map.lookup num indexMap of
        Nothing      -> (poolSize, (indexMap', elements', poolSize'))
            where 
                indexMap' = insert num poolSize indexMap
                elements' = elements ++ [IntegerConstant (read num)]
                poolSize' = poolSize + 1
        (Just index) -> (index, cPool)
    

data Env = Env
    { envInsCount     :: Int
    , envVarMap       :: Map VarNode UInt8
    , envConstantPool :: ConstantPool
    , envMaxStack     :: Int
    , envMaxLocals    :: Int
    }

-- S, EnvV, EnvC, ConstantPool, maxStack, maxLocals -> (instructions, EnvV, EnvC, ConstantPool, maxStack, maxLocal)
genCodeStatement :: StatementNode -> Env -> ([JVMInstruction], Env)
genCodeStatement (AssignmentNode var a) env = 
    genCodeArithExpr a env

genCodeStatement (SkipNode) env = ([], env)

genCodeStatement (CompositeNode s1 s2) env =
    let (ins', env'') = genCodeStatement s1 env in
        let (ins, env') = genCodeStatement s2 env'' in
            (ins' ++ ins, env')

-- b, EnvV, EnvC, maxStack -> (instructions, EnvV, EnvC, maxStack)
-- genCodeBoolExpr :: BoolExprNode -> EnvV -> EnvC -> Int -> ([JVMInstruction], EnvC, Int)

-- b, EnvV, EnvC, maxStack -> (instructions, EnvV, EnvC, maxStack)
genCodeArithExpr :: ArithExprNode -> Env -> ([JVMInstruction], Env)
genCodeArithExpr (NumExprNode n) (Env insCount varMap cPool maxStack maxLocals) = 
    let (index, cPool') = getConstantIndex n cPool in
    ( [JVMldc_w index]
    , (Env (insCount + 1) varMap cPool' (maxStack + 1) maxLocals)
    )