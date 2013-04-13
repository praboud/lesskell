{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Less.Types where

import Data.List (intercalate, sortBy)
import Data.Word(Word32)
import Data.Bits((.&.), shiftR)
import Text.Printf
import Control.Monad.Trans.Either (EitherT)
import Text.Parsec.Error (ParseError)

--------------------------
-- Top Level Statements --
--------------------------

data Statement = ScopeS Scope
               | RuleS Rule
               | MixinS Mixin
               | VariableS Variable
               | IncludeS Include
               | ImportS Import
               deriving Show

data Scope = Scope
    { selector :: Selector
    , rules :: [Rule]
    , includes :: [Include]
    , imports :: [Import]
    , subscopes :: [Scope]
    , mixins :: [Mixin]
    , variables :: [Variable]
    }
    deriving Show

data Include = Include String [[Expression]] deriving Show

type Import = FilePath

data Mixin = Mixin
    { args :: [Param]
    , body :: Scope
    , guards :: Maybe BoolExpression
    } deriving Show
data Rule = Rule Property [Expression] deriving Show
type Property = String

filterStatements :: [Statement] -> ([Scope], [Rule], [Include], [Import], [Mixin], [Variable])
filterStatements (x:xs) = case x of
        ScopeS    a -> (a:s, r, i, p, m, v)
        RuleS     a -> (s, a:r, i, p, m, v)
        IncludeS  a -> (s, r, a:i, p, m, v)
        ImportS   a -> (s, r, i, a:p, m, v)
        MixinS    a -> (s, r, i, p, a:m, v)
        VariableS a -> (s, r, i, p, m, a:v)
    where
    (s, r, i, p, m, v) = filterStatements xs
filterStatements [] = ([], [], [], [], [], [])


---------------
-- Selectors --
---------------

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


---------------------
-- Error Reporting --
---------------------

-- could stand to be expanded

data ProcessError = ProcessError String
                  | TypeError String Expression
                  | ArgumentError [(ExpectedType, Maybe Expression)] [Expression]
                  | ParseError ParseError

rethrowParseError :: Either ParseError a -> Processed a
rethrowParseError (Right x) = Right x
rethrowParseError (Left e) = Left (ParseError e)

type Processed = Either ProcessError

type IOProcessed = EitherT ProcessError IO


-----------------
-- Expressions --
-----------------

data Variable = Variable Identifier [Expression] deriving Show

data Param = Param Identifier
           | DefaultParam Identifier [Expression]
           deriving Show

type Identifier = String

data Expression = Literal String
                | Number Unit Double
                | Color Word32
                | Identifier Int Identifier
                | BinOp Operator Expression Expression
                | FuncApp Identifier [Expression]
                deriving Eq

instance Show Expression where
    show (Literal s) = s
    show (Number u n) = n' ++ show u
        where
        n' = if isInt n 6 then show (round n) else show n
        isInt x n = (round $ 10^n * (x - (fromIntegral $ round x))) == 0
    show (Color w)
        | a == 0 = '#' : printf "%06x" (shiftR w 8)
        | otherwise = printf "rgba(%d,%d,%d,%d)" r g b a
        where
        r = w .&. 0xff000000
        g = w .&. 0x00ff0000
        b = w .&. 0x0000ff00
        a = w .&. 0x000000ff
    show _ = "expression"

instance Show Unit where
    show NA = ""
    show Pt = "pt"
    show Px = "px"
    show Percent = "%"
    show Em = "em"

data Unit = NA | Pt | Px | Percent | Em deriving Eq
type Operator = Char

data BoolExpression = Yep
                    | Nope
                    | Not BoolExpression
                    | Or BoolExpression BoolExpression
                    | And BoolExpression BoolExpression
                    | BoolOperation String Expression Expression deriving (Show, Eq)

----------------------
-- Native Functions --
----------------------

type NativeFunction = [Variable] -> [Expression] -> Processed Expression

data ExpectedType = NumberT | LiteralT | ColourT


--------------------------
-- Output Intermediates --
--------------------------

data CSS = CSS Selector [CSSRule]
data CSSRule = CSSRule Property String deriving Eq

instance Show CSS where
    show (CSS sel rules) = case rules of
        [] -> ""
        rules -> (intercalate ", " $ map show sel) ++ " {\n" ++ (intercalate ";\n" $ map show rules) ++ "\n}\n"

instance Show CSSRule where
    show (CSSRule p v) = p ++ ": " ++ v


-------------------------------
-- Inherit Class and Helpers --
-------------------------------

-- general idea is to define when two declarations overlap, and which
-- declaration to use as the right one when there is an overlap

inherit :: Inherit x k => [x] -> [x] -> [x]
inherit parent child = inherit_h (sortByKey parent) (sortByKey child)
    where
    inherit_h [] ys = ys
    inherit_h xs [] = xs
    inherit_h xt@(x:xs) yt@(y:ys)
        | kx == ky = conflict x y ++ inherit xs ys
        | kx < ky = x : inherit xs yt
        | otherwise = y : inherit xt ys
        where
        kx = key x
        ky = key y
    sortByKey = sortBy (\x y -> compare (key x) (key y))

class Ord k => Inherit x k | x -> k where
    conflict :: x -> x -> [x]
    key :: x -> k

instance Inherit Mixin Selector where
    conflict s1@(Mixin a1 _ g1) s2@(Mixin a2 _ g2)
        | length a1 /= length a2 = [s1, s2]
        | g1 /= g2 = [s1, s2]
        | otherwise = [s2]
    key = selector . body

instance Inherit Variable Identifier where
    conflict _ v2 = [v2]
    key (Variable id _) = id

instance Inherit CSSRule Property where
    conflict _ v2 = [v2]
    key (CSSRule prop _) = prop

instance Inherit Rule Property where
    conflict _ v2 = [v2]
    key (Rule prop _) = prop
