module Codegen
    ( genCodeStatement
    , generateClassFile
    ) where

import JVM
import Ast
import Data.Map.Strict as Map

type ConstantPool = (Map NumeralNode UInt16, [PoolConstant], UInt16) -- map, pool, poolSize
type VariableEnv = (Map VarNode UInt8, UInt8) -- map, variable count
-- Note: there can be a maximum of 256 variables currently

data Env = Env
    { envInsCount     :: Int
    , envVariables    :: VariableEnv
    , envConstantPool :: ConstantPool
    , envMaxStack     :: UInt16
    , envMaxLocals    :: UInt16
    }

getConstantIndex :: NumeralNode -> ConstantPool -> (UInt16, ConstantPool)
getConstantIndex num cPool@(indexMap, elements, poolSize) =
    case Map.lookup num indexMap of
        Nothing      -> (poolSize + 1, (indexMap', elements', poolSize'))
            where 
                indexMap' = insert num poolSize indexMap
                elements' = elements ++ [(IntegerConstant (read num::UInt32))]
                poolSize' = poolSize + 1
        (Just index) -> (index + 1, cPool)

getVarIndex :: VarNode -> VariableEnv -> (UInt8, VariableEnv)
getVarIndex var varEnv@(indexMap, varCount) =
    case Map.lookup var indexMap of
        Nothing      -> (varCount + 1, (indexMap', varCount'))
            where 
                indexMap' = insert var varCount indexMap
                varCount' = varCount + 1
        (Just index) -> (index + 1, varEnv)

initialConstantPool = [
    {- 1 -}  MethodRef 6 13,
    {- 2 -}  FieldRef 14 15,
    {- 3 -}  StringRef 16,
    {- 4 -}  MethodRef 17 18,
    {- 5 -}  ClassRef 19,
    {- 6 -}  ClassRef 20,
    {- 7 -}  StringConstant "<init>",
    {- 8 -}  StringConstant "()V",
    {- 9 -}  StringConstant "Code",
    {- 10 -} StringConstant "main",
    {- 11 -} StringConstant "([Ljava/lang/String;)V",
    {- 12 -} StringConstant "Main.java",
    {- 13 -} NameAndType 7 8,
    {- 14 -} ClassRef 21,
    {- 15 -} NameAndType 22 23,
    {- 16 -} StringConstant "Hello, World",
    {- 17 -} ClassRef 24,
    {- 18 -} NameAndType 25 26,
    {- 19 -} StringConstant "Main",
    {- 20 -} StringConstant "java/lang/Object",
    {- 21 -} StringConstant "java/lang/System",
    {- 22 -} StringConstant "out",
    {- 23 -} StringConstant "Ljava/io/PrintStream;",
    {- 24 -} StringConstant "java/io/PrintStream",
    {- 25 -} StringConstant "println",
    {- 26 -} StringConstant "(I)V"
    ]

initInstructions = [
    JVMaload_0,
    JVMinvokespecial 1,
    JVMreturn
    ]

initialEnv = Env 0 (empty, 0) (empty, initialConstantPool, 26) 1 1

generateClassFile :: StatementNode -> ClassFile
generateClassFile ast =
    let (ins, (Env insCount _ (_, cPool, _) maxStack maxLocals)) = genCodeStatement ast initialEnv in ClassFile 
        [0xCA, 0xFE, 0xBA, 0xBE] -- magic number
        0                        -- version minor
        52                       -- version major
        cPool
        0x0021                   -- access flags 
        5                        -- this class
        6                        -- super class 
        []                       -- interfaces 
        []                       -- fields
        [ (MethodInfo 0x0001 7 8 [(CodeAttributeInfo 9 1 1 (getCodeBytes initInstructions) [] [])]), -- <init> method
          (MethodInfo 0x0009 10 11 [(CodeAttributeInfo 9 maxStack maxLocals (getCodeBytes (ins ++ [JVMreturn])) [] [])]) ] -- main method
        []                       --attributes

-- S, EnvV, EnvC, ConstantPool, maxStack, maxLocals -> (instructions, EnvV, EnvC, ConstantPool, maxStack, maxLocal)
genCodeStatement :: StatementNode -> Env -> ([JVMInstruction], Env)
genCodeStatement (AssignmentNode var a) env = 
    let (ins', (Env insCount vars cPool maxStack maxLocals)) = genCodeArithExpr a env in
        let (index, vars') = getVarIndex var vars in
            (ins' ++ 
            [JVMistore index]
            , (Env (insCount + 1) vars' cPool maxStack (maxLocals + 1)))


genCodeStatement (SkipNode) env = ([], env)

genCodeStatement (CompositeNode s1 s2) env =
    let (ins', env'') = genCodeStatement s1 env in
        let (ins, env') = genCodeStatement s2 env'' in
            (ins' ++ ins, env')

genCodeStatement (PrintNode a) env =
    let (ins, (Env insCount vars cPool maxStack maxLocals)) = genCodeArithExpr a env in
        (
            [JVMgetstatic 2] ++
            ins ++
            [JVMinvokevirtual 4]
            , (Env (insCount + 2) vars cPool maxStack (maxLocals + 1))
        )

-- b, EnvV, EnvC, maxStack -> (instructions, EnvV, EnvC, maxStack)
-- genCodeBoolExpr :: BoolExprNode -> EnvV -> EnvC -> Int -> ([JVMInstruction], EnvC, Int)

-- b, EnvV, EnvC, maxStack -> (instructions, EnvV, EnvC, maxStack)
genCodeArithExpr :: ArithExprNode -> Env -> ([JVMInstruction], Env)
genCodeArithExpr (NumExprNode n) (Env insCount vars cPool maxStack maxLocals) = 
    let (index, cPool') = getConstantIndex n cPool in
        ( [JVMldc_w index]
        , (Env (insCount + 1) vars cPool' (maxStack + 1) maxLocals)
        )

genCodeArithExpr (VarExprNode var) (Env insCount vars cPool maxStack maxLocals) = 
    let (index, vars') = getVarIndex var vars in
        ( [JVMiload index]
        , (Env (insCount + 1) vars' cPool (maxStack + 1) maxLocals)
        )