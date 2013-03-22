module LessTypes where

data Statement = ScopeS Scope | RuleS Rule | MixinS Mixin | VariableS Variable deriving Show

data Scope = Scope
    { selector :: Selector
    , rules :: [Rule]
    , subscopes :: [Scope]
    , mixins :: [Mixin]
    , variables :: [Variable]
    } deriving Show
data Mixin = Mixin
    { args :: [Param]
    , body :: Scope
    , guards :: [BoolExpression]
    } deriving Show
data Variable = Variable Identifier Expression deriving Show
data Rule = Rule Property Expression deriving Show

data Param = Param Identifier (Maybe Expression) deriving Show

--FIXME PLACEHOLDER
type Selector = String
type Identifier = String
type Property = String
type Expression = String

{-
data Expression = Pixel Float
                | Point Float
                | Em Float
                | Percent Float
                | Number Float
                | Application String [Expression]
                | Operation String Expression Expression
                | Blob String deriving Show
-}

data BoolExpression = Yep
                    | Nope
                    | Not BoolExpression
                    | Or BoolExpression BoolExpression
                    | And BoolExpression BoolExpression
                    | BoolOperation String Expression Expression deriving (Show, Eq)

filterStatements :: [Statement] -> ([Scope], [Rule], [Mixin], [Variable])
filterStatements (x:xs) = case x of
        ScopeS    a -> (a:s, r, m, v)
        RuleS     a -> (s, a:r, m, v)
        MixinS    a -> (s, r, a:m, v)
        VariableS a -> (s, r, m, a:v)
    where
    (s, r, m, v) = filterStatements xs
filterStatements [] = ([], [], [], [])
