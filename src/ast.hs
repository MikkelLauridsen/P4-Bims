module Ast 
    ( NumeralNode(..)
    , VarNode(..)
    , BoolExprNode(..)
    , ArithExprNode(..)
    , StatementNode(..)
    ) where

type NumeralNode = String

type VarNode = String

data BoolExprNode 
    = ComparisonNode ArithExprNode ArithExprNode
    | LessThanNode ArithExprNode ArithExprNode
    | GreaterThanNode ArithExprNode ArithExprNode
    | NotNode BoolExprNode
    | AndNode BoolExprNode BoolExprNode
    | OrNode BoolExprNode BoolExprNode
    | BoolParenNode BoolExprNode 

data ArithExprNode 
    = NumExprNode NumeralNode
    | VarExprNode VarNode
    | AddExprNode ArithExprNode ArithExprNode
    | MultExprNode ArithExprNode ArithExprNode
    | SubExprNode ArithExprNode ArithExprNode
    | ArithParenNode ArithExprNode

data StatementNode
    = AssignmentNode VarNode ArithExprNode
    | SkipNode
    | CompositeNode StatementNode StatementNode
    | IfNode BoolExprNode StatementNode StatementNode
    | WhileNode BoolExprNode StatementNode
