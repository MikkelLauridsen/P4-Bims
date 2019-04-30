module Codegen
    ( genCodeStatement
    , generateClassFile
    ) where

import JVM
import Ast
import Data.Map.Strict as Map
import Data.Int

type EnvC = (Map NumeralNode UInt16, [PoolConstant], UInt16) -- map, pool, poolSize
type EnvJVM = (UInt16, UInt16, EnvC) -- maxStack, maxLocals, constants
type EnvV = (Map VarNode UInt8, UInt8) -- map, variable count
-- Note: there can be a maximum of 256 variables currently

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
initialEnvJ = (1, 1, (empty, initialConstantPool, 26))

generateClassFile :: StatementNode -> ClassFile
generateClassFile ast =
    let (ins, _, _, (maxStack, maxLocals, (_, cPool, _))) = genCodeStatement ast initialEnvV initialEnvJ in ClassFile 
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
updateJVM :: UInt16 -> UInt16 -> EnvJVM -> EnvJVM
updateJVM s l (maxStack, maxLocals, envC) = (maxStack + s, maxLocals + l, envC)

updateJVME :: UInt16 -> UInt16 -> EnvC -> EnvJVM -> EnvJVM
updateJVME s l c (maxStack, maxLocals, envC) = (maxStack + s, maxLocals + l, c)

genCodeStatement :: StatementNode -> EnvV -> EnvJVM -> ([JVMInstruction], Int16, EnvV, EnvJVM)
genCodeStatement (AssignmentNode var a) envV envJ =
    ( aIns ++ [JVMistore index]
    , aInsSize + 2
    , envV'
    , updateJVM 0 1 envJ'
    )
    where
        (aIns, aInsSize, envJ') = genCodeArithExpr a envV envJ
        (index, envV') = getVarIndex var envV

genCodeStatement (SkipNode) envV envJ = ([], 0, envV, envJ)

genCodeStatement (CompositeNode s1 s2) envV envJ =
    ( s1Ins ++ s2Ins
    , s1InsSize + s2InsSize
    , envV'
    , envJ'
    )
    where
        (s1Ins, s1InsSize, envV'', envJ'') = genCodeStatement s1 envV envJ
        (s2Ins, s2InsSize, envV', envJ') = genCodeStatement s2 envV'' envJ''

-- Todo: IfNode

genCodeStatement (WhileNode b s) envV envJ =
    ( [JVMgoto (3 + sInsSize)] ++
      sIns ++
      bIns ++
      [ JVMiconst_1
      , JVMif_icmpeq (-(sInsSize + bInsSize + 1))
      ]
    , sInsSize + bInsSize + 7
    , envV
    , envJ'
    )
    where
        (sIns, sInsSize, envV'', envJ'') = genCodeStatement s envV envJ
        (bIns, bInsSize, envJ') = genCodeBoolExpr b envV'' envJ''

genCodeStatement (PrintNode a) envV envJ =
    ( [JVMgetstatic 2] ++
      aIns ++
      [JVMinvokevirtual 4]
    , aInsSize + 6
    , envV
    , updateJVM 0 1 envJ'
    )
    where 
        (aIns, aInsSize, envJ') = genCodeArithExpr a envV envJ

genCodeBoolExpr :: BoolExprNode -> EnvV -> EnvJVM -> ([JVMInstruction], Int16, EnvJVM)
genCodeBoolExpr (ComparisonNode a1 a2) envV envJ =
    ( a1Ins ++ a2Ins ++
      [ JVMif_icmpne 7
      , JVMiconst_1
      , JVMgoto 4
      , JVMiconst_0
      ]
    , a1InsSize + a2InsSize + 8
    , envJ'
    )
    where
        (a1Ins, a1InsSize, envJ'') = genCodeArithExpr a1 envV envJ
        (a2Ins, a2InsSize, envJ') = genCodeArithExpr a2 envV envJ''

-- Todo: LessThanNode

genCodeBoolExpr (GreaterThanNode a1 a2) envV envJ =
    ( a1Ins ++ a2Ins ++
      [ JVMif_icmpgt 7
      , JVMiconst_0
      , JVMgoto 4
      , JVMiconst_1
      ]
    , a1InsSize + a2InsSize + 8
    , envJ'
    )
    where
        (a1Ins, a1InsSize, envJ'') = genCodeArithExpr a1 envV envJ
        (a2Ins, a2InsSize, envJ') = genCodeArithExpr a2 envV envJ''

-- Todo: NotNode

-- Todo: AndNode

-- Todo: OrNode

genCodeBoolExpr (BoolParenNode a) envV envJ = genCodeBoolExpr a envV envJ

genCodeArithExpr :: ArithExprNode -> EnvV -> EnvJVM -> ([JVMInstruction], Int16, EnvJVM)
genCodeArithExpr (NumExprNode n) envV envJ@(_, _, envC) = 
    let (index, envC') = getConstantIndex n envC in
        ( [JVMldc_w index]
        , 3
        , updateJVME 1 0 envC' envJ
        )

genCodeArithExpr (VarExprNode var) envV envJ = 
    let (index, vars') = getVarIndex var envV in
        ( [JVMiload index]
        , 2
        , updateJVM 1 0 envJ
        )
    
genCodeArithExpr (AddExprNode a1 a2) envV envJ =
        ( a1Ins ++ a2Ins ++ [JVMiadd]
        , a1InsSize + a2InsSize + 1
        , envJ'
        )
    where
        (a1Ins, a1InsSize, envJ'') = genCodeArithExpr a1 envV envJ
        (a2Ins, a2InsSize, envJ') = genCodeArithExpr a2 envV envJ''

genCodeArithExpr (SubExprNode a1 a2) envV envJ =
        ( a1Ins ++ a2Ins ++ [JVMisub]
        , a1InsSize + a2InsSize + 1
        , envJ'
        )
    where
        (a1Ins, a1InsSize, envJ'') = genCodeArithExpr a1 envV envJ
        (a2Ins, a2InsSize, envJ') = genCodeArithExpr a2 envV envJ''

genCodeArithExpr (MultExprNode a1 a2) envV envJ =
        ( a1Ins ++ a2Ins ++ [JVMimul]
        , a1InsSize + a2InsSize + 1
        , envJ'
        )
    where
        (a1Ins, a1InsSize, envJ'') = genCodeArithExpr a1 envV envJ
        (a2Ins, a2InsSize, envJ') = genCodeArithExpr a2 envV envJ''

genCodeArithExpr (ArithParenNode a) envV envJ = genCodeArithExpr a envV envJ