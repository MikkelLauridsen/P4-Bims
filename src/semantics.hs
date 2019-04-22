module Semantics 
    (
        interpret
    ) where

import Data.Map.Strict as Map
import Ast 

type Var = VarNode

type Env = Map VarNode Float

type Binding = (Var, Float)

except :: Env -> Binding -> Env
except env (var, value) = Map.insertWith const var value env

getVar :: Var -> Env -> Float
getVar var env = 
    case Map.lookup var env of
        Nothing      -> error $ "Error: variable '" ++ var ++ "' is unassigned."
        (Just value) -> value

interpret :: StatementNode -> Env
interpret stmt = evalStatement stmt (Map.fromList [])

-- "Aud" semantics
evalArithExpr :: ArithExprNode -> Env -> Float
evalArithExpr expr env =
     case expr of
        (AddExprNode an1 an2)   -> (evalArithExpr an1 env) + (evalArithExpr an2 env) 
        (SubExprNode an1 an2)   -> (evalArithExpr an1 env) - (evalArithExpr an2 env)
        (MultExprNode an1 an2)  -> (evalArithExpr an1 env) * (evalArithExpr an2 env)
        (ArithParenNode an1)    -> evalArithExpr an1 env
        (NumExprNode numeral)   -> read numeral
        (VarExprNode var)       -> getVar var env

-- "Bud" semantics
evalBoolExpr :: BoolExprNode -> Env -> Bool
evalBoolExpr expr env =
    case expr of
        (ComparisonNode an1 an2)  -> (evalArithExpr an1 env) == (evalArithExpr an2 env)
        (LessThanNode an1 an2)    -> (evalArithExpr an1 env) < (evalArithExpr an2 env)
        (GreaterThanNode an1 an2) -> (evalArithExpr an1 env) > (evalArithExpr an2 env)
        (NotNode bn)              -> not (evalBoolExpr bn env)
        (AndNode bn1 bn2)         -> (evalBoolExpr bn1 env) && (evalBoolExpr bn2 env)
        (OrNode bn1 bn2)          -> (evalBoolExpr bn1 env) || (evalBoolExpr bn2 env)
        (BoolParenNode bn)        -> evalBoolExpr bn env

-- "Kom" semantics
evalStatement :: StatementNode -> Env -> Env
evalStatement stmt env =
    case stmt of
        (AssignmentNode var an) -> (env `except` (var, evalArithExpr an env))
        (SkipNode)              -> env
        (CompositeNode sn1 sn2) -> evalStatement sn2 (evalStatement sn1 env)
        --(IfNode bn sn1 sn2)     -> 
        --(WhileNode bn sn)       -> 

--data State =  