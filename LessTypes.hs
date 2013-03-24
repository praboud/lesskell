module LessTypes where

data Statement = ScopeS Scope
               | RuleS Rule
               | MixinS Mixin
               | VariableS Variable
               | IncludeS Include
               deriving Show

data Scope = Scope
    { selector :: Selector
    , rules :: [Rule]
    , includes :: [Include]
    , subscopes :: [Scope]
    , mixins :: [Mixin]
    , variables :: [Variable]
    } 
    deriving Show

data Include = Include String [Expression] deriving Show

data Mixin = Mixin
    { args :: [Param]
    , body :: Scope
    , guards :: [BoolExpression]
    } deriving Show
data Variable = Variable Identifier Expression deriving Show
data Rule = Rule Property Expression deriving Show

data Param = Param Identifier
           | DefaultParam Identifier Expression
           deriving Show

--TYPES FOR SELECTOR
--nested list: group, adjacent, descendant, child
type Selector = [SelectorCombinator]
data SelectorCombinator = Combinator Char SimpleSelectorSeq SelectorCombinator
                        | Terminus SimpleSelectorSeq
                        | Dummy
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
    show (Dummy) = "Dummy"

instance Show SimpleSelector where
    show (TypeSelector x) = x
    show UniversalSelector = "*"
    show (AttributeSelector x) = '[' : x ++ "]"
    show (ClassSelector x) = '.' : x
    show (IdSelector x) = '#' : x
    show (NotSelector s) = ":not(" ++ (show s) ++ ")"
    show (PseudoClassSelector x) = ':' : x
    show ParentRef = "&"

-- error reporting
data ProcessError = ProcessError String

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

filterStatements :: [Statement] -> ([Scope], [Rule], [Include], [Mixin], [Variable])
filterStatements (x:xs) = case x of
        ScopeS    a -> (a:s, r, i, m, v)
        RuleS     a -> (s, a:r, i, m, v)
        IncludeS  a -> (s, r, a:i, m, v)
        MixinS    a -> (s, r, i, a:m, v)
        VariableS a -> (s, r, i, m, a:v)
    where
    (s, r, i, m, v) = filterStatements xs
filterStatements [] = ([], [], [], [], [])
