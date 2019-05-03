module Codegen
    ( genCodeStatement
    , generateClassFile
    ) where

import JVM
import Ast
import Data.Map.Strict as Map
import Data.Int

type EnvC = (Map NumeralNode UInt16, [PoolConstant], UInt16) -- map, pool, poolSize
type EnvJVM = (UInt16, UInt16, UInt16, EnvC) -- currentStack, maxStack, maxLocals, constants
type EnvV = (Map VarNode UInt8, UInt8) -- map, variable count
-- Note: there can be a maximum of 256 variables

getConstantIndex :: NumeralNode -> EnvC -> (UInt16, EnvC)
getConstantIndex num cPool@(indexMap, elements, poolSize) =
    case Map.lookup num indexMap of
        Nothing      -> (poolSize + 1, (indexMap', elements', poolSize'))
            where 
                indexMap' = insert num poolSize indexMap
                elements' = elements ++ [(IntegerConstant (read num::UInt32))]
                poolSize' = poolSize + 1
        (Just index) -> (index + 1, cPool)

getVarIndex :: VarNode -> EnvV -> (UInt8, EnvV, Bool)
getVarIndex var varEnv@(indexMap, varCount) =
    case Map.lookup var indexMap of
        Nothing      -> (varCount + 1, (indexMap', varCount'), False)
            where 
                indexMap' = insert var varCount indexMap
                varCount' = varCount + 1
        (Just index) -> (index + 1, varEnv, True)

initialConstantPool = [
    {- 1 -}  MethodRef 5 12,
    {- 2 -}  FieldRef 13 14,
    {- 3 -}  MethodRef 15 16,
    {- 4 -}  ClassRef 17,
    {- 5 -}  ClassRef 18,
    {- 6 -}  StringConstant "<init>",
    {- 7 -}  StringConstant "()V",
    {- 8 -}  StringConstant "Code",
    {- 9 -}  StringConstant "main",
    {- 10 -} StringConstant "([Ljava/lang/String;)V",
    {- 11 -} StringConstant "Main.java",
    {- 12 -} NameAndType 6 7,
    {- 13 -} ClassRef 19,
    {- 14 -} NameAndType 20 21,
    {- 15 -} ClassRef 22,
    {- 16 -} NameAndType 23 24,
    {- 17 -} StringConstant "Main",
    {- 18 -} StringConstant "java/lang/Object",
    {- 19 -} StringConstant "java/lang/System",
    {- 20 -} StringConstant "out",
    {- 21 -} StringConstant "Ljava/io/PrintStream;",
    {- 22 -} StringConstant "java/io/PrintStream",
    {- 23 -} StringConstant "println",
    {- 24 -} StringConstant "(I)V"
    ]

printConstantIndex = 3

initInstructions = [
    JVMaload_0,
    JVMinvokespecial 1,
    JVMreturn
    ]

initialEnvV = (empty, 0)
initialEnvJ = (2, 1, 1, (empty, initialConstantPool, fromIntegral (length initialConstantPool)))

generateClassFile :: StatementNode -> ClassFile
generateClassFile ast =
    let (ins, _, _, (_, maxStack, maxLocals, (_, cPool, _))) = genCodeStatement ast initialEnvV initialEnvJ in ClassFile 
        [0xCA, 0xFE, 0xBA, 0xBE] -- magic number
        0       -- version minor
        49      -- version major
        cPool
        0x0000  -- access flags
        4       -- this class (Main)
        5       -- super class (java/lang/Object)
        []      -- interfaces 
        []      -- fields
        [ (MethodInfo 0x0001 6 7 [(CodeAttributeInfo 8 1 1 (getCodeBytes initInstructions) [] [])]), -- <init> method
          (MethodInfo 0x0009 9 10 [(CodeAttributeInfo 8 maxStack maxLocals (getCodeBytes (ins ++ [JVMreturn])) [] [])]) ] -- main method
        []      --attributes

-- Adds to byteCount, maxStack and maxLocals in Env
updateJVM :: Int16 -> UInt16 -> EnvJVM -> EnvJVM
updateJVM s l envJ@(_, _, _, envC) = updateJVME s l envC envJ

updateJVME :: Int16 -> UInt16 -> EnvC -> EnvJVM -> EnvJVM
updateJVME s l c (curStack, maxStack, maxLocals, _) = (fromIntegral ((fromIntegral curStack) + s), max curStack maxStack, maxLocals + l, c)

-- Statements
genCodeStatement :: StatementNode -> EnvV -> EnvJVM -> ([JVMInstruction], Int16, EnvV, EnvJVM)
-- Assignment statement
genCodeStatement (AssignmentNode var a) envV envJ =
    ( aIns ++ [JVMistore index]
    , aInsSize + 2
    , envV'
    , if present
        then envJ' 
        else updateJVM 0 1 envJ'
    )
    where
        (aIns, aInsSize, envJ') = genCodeArithExpr a envV envJ
        (index, envV', present) = getVarIndex var envV

-- Skip statement
genCodeStatement (SkipNode) envV envJ = ([], 0, envV, envJ)

-- Composite statement
genCodeStatement (CompositeNode s1 s2) envV envJ =
    ( s1Ins ++ s2Ins
    , s1InsSize + s2InsSize
    , envV'
    , envJ'
    )
    where
        (s1Ins, s1InsSize, envV'', envJ'') = genCodeStatement s1 envV envJ
        (s2Ins, s2InsSize, envV', envJ') = genCodeStatement s2 envV'' envJ''

-- If statement
genCodeStatement (IfNode b s1 s2) envV envJ =
    ( bIns ++
      [ JVMiconst_1
      , JVMif_icmpne (s1InsSize + 6) -- jump else
      ] ++
      s1Ins ++
      [JVMgoto (s2InsSize + 3)] ++ -- jump end
      s2Ins -- label: else
      --label: end
    , bInsSize + s1InsSize + s2InsSize + 6
    , envV'
    , updateJVM (-1) 0 envJ'
    )
    where
        (bIns, bInsSize, envJ3) = genCodeBoolExpr b envV envJ
        (s1Ins, s1InsSize, envV'', envJ'') = genCodeStatement s1 envV envJ3
        (s2Ins, s2InsSize, envV', envJ') = genCodeStatement s2 envV'' envJ''

-- While statement
genCodeStatement (WhileNode b s) envV envJ =
    ( [JVMgoto (3 + sInsSize)] ++ -- jump predicate
      sIns ++ -- label: loop
      bIns ++ -- label: predicate
      [ JVMiconst_1
      , JVMif_icmpeq (-(sInsSize + bInsSize + 1)) -- jump loop
      ]
    , sInsSize + bInsSize + 7
    , envV
    , envJ'
    )
    where
        (sIns, sInsSize, envV'', envJ'') = genCodeStatement s envV envJ
        (bIns, bInsSize, envJ') = genCodeBoolExpr b envV'' envJ''

-- Print statement
genCodeStatement (PrintNode a) envV envJ =
    ( [JVMgetstatic 2] ++
      aIns ++
      [JVMinvokevirtual printConstantIndex] -- call print
    , aInsSize + 6
    , envV
    , updateJVM (-1) 0 envJ'
    )
    where 
        (aIns, aInsSize, envJ') = genCodeArithExpr a envV envJ

        
-- Boolean expressions
genCodeBoolExpr :: BoolExprNode -> EnvV -> EnvJVM -> ([JVMInstruction], Int16, EnvJVM)
-- Comparison boolean expression
genCodeBoolExpr (ComparisonNode a1 a2) envV envJ =
    ( a1Ins ++ a2Ins ++
      [ JVMif_icmpne 7 --jump true
      , JVMiconst_1
      , JVMgoto 4 --jump end
      , JVMiconst_0 --label: true
        --label: end
      ]
    , a1InsSize + a2InsSize + 8
    , updateJVM (-1) 0 envJ'
    )
    where
        (a1Ins, a1InsSize, envJ'') = genCodeArithExpr a1 envV envJ
        (a2Ins, a2InsSize, envJ') = genCodeArithExpr a2 envV envJ''

-- Less than boolean expression
genCodeBoolExpr (LessThanNode a1 a2) envV envJ =
    ( a1Ins ++ a2Ins ++
      [ JVMif_icmplt 7 --jump true
      , JVMiconst_0
      , JVMgoto 4 --jump end
      , JVMiconst_1 --label: true
        --label: end
      ]
    , a1InsSize + a2InsSize + 8
    , updateJVM (-1) 0 envJ'
    )
    where
        (a1Ins, a1InsSize, envJ'') = genCodeArithExpr a1 envV envJ
        (a2Ins, a2InsSize, envJ') = genCodeArithExpr a2 envV envJ''

-- Greater than boolean expression
genCodeBoolExpr (GreaterThanNode a1 a2) envV envJ =
    ( a1Ins ++ a2Ins ++
      [ JVMif_icmpgt 7 --jump true
      , JVMiconst_0
      , JVMgoto 4 --jump end
      , JVMiconst_1 --label: true
        --label: end
      ]
    , a1InsSize + a2InsSize + 8
    , updateJVM (-1) 0 envJ'
    )
    where
        (a1Ins, a1InsSize, envJ'') = genCodeArithExpr a1 envV envJ
        (a2Ins, a2InsSize, envJ') = genCodeArithExpr a2 envV envJ''

-- Not boolean expression
genCodeBoolExpr (NotNode b) envV envJ =
    ( bIns ++
      [ JVMiconst_1
      , JVMif_icmpne 7 --jump true
      , JVMiconst_0
      , JVMgoto 4 --jump end
      , JVMiconst_1 --label: true
        --label: end
      ]
    , bInsSize + 9
    , envJ'
    )
    where
        (bIns, bInsSize, envJ') = genCodeBoolExpr b envV envJ

-- And boolean expression
genCodeBoolExpr (AndNode b1 b2) envV envJ =
    ( b1Ins ++
      [ JVMiconst_1
      , JVMif_icmpne (b2InsSize + 11) --jump false
      ] ++
      b2Ins ++
      [ JVMiconst_1
      , JVMif_icmpne 7 --jump false
      , JVMiconst_1
      , JVMgoto 4 --jump end
      , JVMiconst_0 --label: false
        --label: end
      ]
    , b1InsSize + b2InsSize + 13
    , updateJVM (-1) 0 envJ'
    )
    where
        (b1Ins, b1InsSize, envJ'') = genCodeBoolExpr b1 envV envJ
        (b2Ins, b2InsSize, envJ') = genCodeBoolExpr b2 envV envJ''

-- Or boolean expression
genCodeBoolExpr (OrNode b1 b2) envV envJ =
    ( b1Ins ++
      [ JVMiconst_1
      , JVMif_icmpeq (b2InsSize + 11) --jump true
      ] ++
      b2Ins ++
      [ JVMiconst_1
      , JVMif_icmpeq 7 --jump true
      , JVMiconst_0
      , JVMgoto 4 --jump end
      , JVMiconst_1 --label: true
        -- label end
      ]
    , b1InsSize + b2InsSize + 13
    , updateJVM (-1) 0 envJ'
    )
    where
        (b1Ins, b1InsSize, envJ'') = genCodeBoolExpr b1 envV envJ
        (b2Ins, b2InsSize, envJ') = genCodeBoolExpr b2 envV envJ''

-- Parenthesis boolean expression
genCodeBoolExpr (BoolParenNode a) envV envJ = genCodeBoolExpr a envV envJ


-- Arithmetic expressions
genCodeArithExpr :: ArithExprNode -> EnvV -> EnvJVM -> ([JVMInstruction], Int16, EnvJVM)
-- Numeral arithmetic expression
genCodeArithExpr (NumExprNode n) envV envJ@(_, _, _, envC) = 
    let (index, envC') = getConstantIndex n envC in
        ( [JVMldc_w index]
        , 3
        , updateJVME 1 0 envC' envJ
        )

-- Variable arithmetic expression
genCodeArithExpr (VarExprNode var) envV envJ = 
    let (index, vars', _) = getVarIndex var envV in
        ( [JVMiload index]
        , 2
        , updateJVM 1 0 envJ
        )

-- Addition arithmetic expression
genCodeArithExpr (AddExprNode a1 a2) envV envJ =
        ( a1Ins ++ a2Ins ++ [JVMiadd]
        , a1InsSize + a2InsSize + 1
        , updateJVM (-1) 0 envJ'
        )
    where
        (a1Ins, a1InsSize, envJ'') = genCodeArithExpr a1 envV envJ
        (a2Ins, a2InsSize, envJ') = genCodeArithExpr a2 envV envJ''

-- Subtraction arithmetic expression
genCodeArithExpr (SubExprNode a1 a2) envV envJ =
        ( a1Ins ++ a2Ins ++ [JVMisub]
        , a1InsSize + a2InsSize + 1
        , updateJVM (-1) 0 envJ'
        )
    where
        (a1Ins, a1InsSize, envJ'') = genCodeArithExpr a1 envV envJ
        (a2Ins, a2InsSize, envJ') = genCodeArithExpr a2 envV envJ''

-- Multiplication arithmetic expression
genCodeArithExpr (MultExprNode a1 a2) envV envJ =
        ( a1Ins ++ a2Ins ++ [JVMimul]
        , a1InsSize + a2InsSize + 1
        , updateJVM (-1) 0 envJ'
        )
    where
        (a1Ins, a1InsSize, envJ'') = genCodeArithExpr a1 envV envJ
        (a2Ins, a2InsSize, envJ') = genCodeArithExpr a2 envV envJ''

-- Parenthesis arithmetic expression
genCodeArithExpr (ArithParenNode a) envV envJ = genCodeArithExpr a envV envJ