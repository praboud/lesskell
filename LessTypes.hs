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

--TYPES FOR SELECTOR
--nested list: group, adjacent, descendant, child
type Selector = [SelectorCombinator]
data SelectorCombinator = Combinator Char SimpleSelectorSeq SelectorCombinator
                        | Terminus SimpleSelectorSeq
                        deriving (Eq, Ord)
type SimpleSelectorSeq = [SimpleSelector]
data SimpleSelector = TypeSelector String -- h1
                   | UniversalSelector -- *
                   | AttributeSelector String -- [placeholder]
                   | ClassSelector String
                   | PseudoClassSelector String
                   | IdSelector String
                   | NotSelector SimpleSelector
                   | ParentRef
                   deriving (Eq, Ord)

instance Show SelectorCombinator where
    show (Combinator t ss sc) = (concat $ map show ss) ++ (combSep t) ++ (show sc)
        where
        combSep ' ' = " "
        combSep c = ' ' : c : " "
    show (Terminus ss) = concat $ map show ss

instance Show SimpleSelector where
    show (TypeSelector x) = x
    show UniversalSelector = "*"
    show (AttributeSelector x) = '[' : x ++ "]"
    show (ClassSelector x) = '.' : x
    show (IdSelector x) = '#' : x
    show (NotSelector s) = ":not(" ++ (show s) ++ ")"
    show (PseudoClassSelector x) = ':' : x
    show ParentRef = "&"

--FIXME PLACEHOLDER
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
