module Codegen
    ( genCodeStatement
    , generateClassFile
    ) where

import JVM
import Ast
import Data.Map.Strict as Map

type EnvC = (Map NumeralNode UInt16, [PoolConstant], UInt16) -- map, pool, poolSize
type EnvJVM = (UInt16, UInt16, UInt16, EnvC) -- byte count, maxStack, maxLocals, constants
type EnvV = (Map VarNode UInt8, UInt8) -- map, variable count
-- Note: there can be a maximum of 256 variables currently

data Env = Env
    { envByteCount     :: Int
    , envVariables    :: EnvV
    , envConstantPool :: EnvC
    , envMaxStack     :: UInt16
    , envMaxLocals    :: UInt16
    }

getConstantIndex :: NumeralNode -> EnvC -> (UInt16, EnvC)
getConstantIndex num cPool@(indexMap, elements, poolSize) =
    case Map.lookup num indexMap of
        Nothing      -> (poolSize + 1, (indexMap', elements', poolSize'))
            where 
                indexMap' = insert num poolSize indexMap
                elements' = elements ++ [(IntegerConstant (read num::UInt32))]
                poolSize' = poolSize + 1
        (Just index) -> (index + 1, cPool)

getVarIndex :: VarNode -> EnvV -> (UInt8, EnvV)
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

initialEnvV = (empty, 0)
initialEnvJ = (0, 1, 1, (empty, initialConstantPool, 26))

generateClassFile :: StatementNode -> ClassFile
generateClassFile ast =
    let (ins, _, (byteCount, maxStack, maxLocals, (_, cPool, _))) = genCodeStatement ast initialEnvV initialEnvJ in ClassFile 
        [0xCA, 0xFE, 0xBA, 0xBE] -- magic number
        0                        -- version minor
        49                       -- version major
        cPool
        0x0021                   -- access flags 
        5                        -- this class
        6                        -- super class 
        []                       -- interfaces 
        []                       -- fields
        [ (MethodInfo 0x0001 7 8 [(CodeAttributeInfo 9 1 1 (getCodeBytes initInstructions) [] [])]), -- <init> method
          (MethodInfo 0x0009 10 11 [(CodeAttributeInfo 9 maxStack maxLocals (getCodeBytes (ins ++ [JVMreturn])) [] [])]) ] -- main method
        []                       --attributes

-- Adds to byteCount, maxStack and maxLocals in Env
updateJVM :: UInt16 -> UInt16 -> UInt16 -> EnvJVM -> EnvJVM
updateJVM b s l (byteC, maxStack, maxLocals, envC) = (byteC + b, maxStack + s, maxLocals + l, envC)

updateJVME :: UInt16 -> UInt16 -> UInt16 -> EnvC -> EnvJVM -> EnvJVM
updateJVME b s l c (byteC, maxStack, maxLocals, envC) = (byteC + b, maxStack + s, maxLocals + l, c)

getInsByteCount :: EnvJVM -> UInt16
getInsByteCount (byteC, _, _, _) = byteC

genCodeStatement :: StatementNode -> EnvV -> EnvJVM -> ([JVMInstruction], EnvV, EnvJVM)
genCodeStatement (AssignmentNode var a) envV envJ =
    (
        ins' ++ [JVMistore index],
        envV',
        updateJVM 3 0 1 envJ'
    )
    where
        (ins', envJ') = genCodeArithExpr a envV envJ
        (index, envV') = getVarIndex var envV

genCodeStatement (SkipNode) envV envJ = ([], envV, envJ)

genCodeStatement (CompositeNode s1 s2) envV envJ =
    (ins' ++ ins, envV', envJ')
    where
        (ins', envV'', envJ'') = genCodeStatement s1 envV envJ
        (ins, envV', envJ') = genCodeStatement s2 envV'' envJ''

genCodeStatement (WhileNode b s) envV envJ =
    (
        [JVMgoto predLabel] ++
        sIns ++
        bIns ++
        [JVMif_icmpeq loopLabel],
        envV, updateJVM 3 0 0 envJ'
    )
    where
        envJ3 = updateJVM 3 0 0 envJ 
        (sIns, envV'', envJ'') = genCodeStatement s envV envJ3
        (bIns, envJ') = genCodeBoolExpr b envV'' envJ''
        predLabel = (getInsByteCount envJ'')
        loopLabel = (getInsByteCount envJ) + 3

genCodeStatement (PrintNode a) envV envJ =
    (
        [JVMgetstatic 2] ++
        aIns ++
        [JVMinvokevirtual 4]
        , envV
        , updateJVM 6 0 1 envJ'
    )
    where 
        (aIns, envJ') = genCodeArithExpr a envV envJ

genCodeBoolExpr :: BoolExprNode -> EnvV -> EnvJVM -> ([JVMInstruction], EnvJVM)
genCodeBoolExpr (ComparisonNode a1 a2) envV envJ =
    (ins ++ [
        JVMif_icmpne notEqualLabel,
        JVMiconst_1,
        JVMgoto endLabel,
        JVMiconst_0
    ], updateJVM 8 0 0 envJ')
    where
        ins = ins'' ++ ins'
        (ins'', envJ'') = genCodeArithExpr a1 envV envJ
        (ins', envJ') = genCodeArithExpr a2 envV envJ''
        notEqualLabel = ((getInsByteCount envJ') + 7)
        endLabel = ((getInsByteCount envJ') + 8)

genCodeArithExpr :: ArithExprNode -> EnvV -> EnvJVM -> ([JVMInstruction], EnvJVM)
genCodeArithExpr (NumExprNode n) envV envJ@(_, _, _, envC) = 
    let (index, envC') = getConstantIndex n envC in
        ( [JVMldc_w index]
        , updateJVME 3 1 0 envC' envJ
        )

genCodeArithExpr (VarExprNode var) envV envJ = 
    let (index, vars') = getVarIndex var envV in
        ( [JVMiload index]
        , updateJVM 2 1 0 envJ
        )
    
genCodeArithExpr (AddExprNode a1 a2) envV envJ =
        ( ins'' ++ ins' ++ [JVMiadd]
        , updateJVM 1 0 0 envJ'
        )
    where
        (ins'', envJ'') = genCodeArithExpr a1 envV envJ
        (ins', envJ') = genCodeArithExpr a2 envV envJ''

genCodeArithExpr (SubExprNode a1 a2) envV envJ =
        ( ins'' ++ ins' ++ [JVMisub]
        , updateJVM 1 0 0 envJ'
        )
    where
        (ins'', envJ'') = genCodeArithExpr a1 envV envJ
        (ins', envJ') = genCodeArithExpr a2 envV envJ''

genCodeArithExpr (MultExprNode a1 a2) envV envJ =
        ( ins'' ++ ins' ++ [JVMimul]
        , updateJVM 1 0 0 envJ'
        )
    where
        (ins'', envJ'') = genCodeArithExpr a1 envV envJ
        (ins', envJ') = genCodeArithExpr a2 envV envJ''

genCodeArithExpr (ArithParenNode a) envV envJ = genCodeArithExpr a envV envJ