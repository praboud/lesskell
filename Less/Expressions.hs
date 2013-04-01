module Less.Expressions (evalExp) where

import Less.Types
import Data.Maybe (fromJust)

-- function deps
import Less.Parser(unitParser, colourParser)
import Text.ParserCombinators.Parsec(parse)

evalExp :: [Variable] -> Expression -> Either ProcessError [Expression]
evalExp _ x@(Literal _) = return [x]
evalExp _ x@(Number _ _) = return [x]
evalExp _ x@(Color _) = return [x]
evalExp vs (BinOp op l r) = do
    (Number unit l') <- getNumber vs l
    (Number _ r') <- getNumberWithUnit vs (Just unit) r
    let op' = evalOp op
    return $ [Number unit (op' l' r')]
evalExp vs (Identifier 1 v) = evalVariable vs v >>= mapM (evalExp vs) >>= return . concat

evalOp = fromJust . flip lookup ops

ops = 
    [ ('+', (+))
    , ('-', (-))
    , ('*', (*))
    , ('/', (/))
    ]
    
-- helpers

-- expects an expression evaling to a number, possibly of a particular unit
-- returns eval'd number, or throws a TypeError on bad match
getNumberWithUnit :: [Variable] -> Maybe Unit -> Expression -> Either ProcessError Expression
getNumberWithUnit vs uexp e = do
    evald <- evalExp vs e
    case evald of
        [n@(Number u val)] -> if unitMatch u
            then return n
            else Left $ TypeError (show u) e
        x -> Left $ TypeError "number" e
    where
    unitMatch u = case (uexp, u) of
        (Nothing, _) -> True
        (Just NA, _) -> True
        (_, NA)  -> True
        (Just a, b) -> a == b

getNumber vs = getNumberWithUnit vs Nothing

evalVariable :: [Variable] -> Identifier -> Processed [Expression]
evalVariable [] id = Left $ ProcessError ("Unbound variable " ++ id)
evalVariable ((Variable name val):vs) id
    | name == id = return val
    | otherwise = evalVariable vs id

----------------------------------------
-- Implementation of native functions --
----------------------------------------

-- in a more perfect world, this would be in a seperate file,
-- but ghc support for mutually recursive modules isn't great

nativeFunctions =
    [ ("unit", unit)
    , ("color", colour)

    , ("ceil", mathFun (fromIntegral . ceiling))
    , ("floor", mathFun (fromIntegral . floor))
    -- percentage
    -- round
    , ("sqrt", mathFun sqrt)
    , ("sin", mathFun sin)
    , ("asin", mathFun asin)
    , ("cos", mathFun cos)
    , ("acos", mathFun acos)
    , ("tan", mathFun tan)
    , ("atan", mathFun atan)
    ] :: [(String, NativeFunction)]

-- 
-- Nothing for required, Just defaultVal for optional
getArgs :: [Variable] -> [(ExpectedType, Maybe Expression)] -> [Expression] -> Processed [Expression]
getArgs vs exps args = parityCheck exps args
    where
    err = Left $ ArgumentError exps args

    parityCheck [] [] = return []
    parityCheck ((_, Nothing): _) [] = err
    parityCheck ((_, Just v):es) [] = parityCheck es [] >>= return . (v:)
    parityCheck ((t, _):es) (a:as) = do
        a' <- evalExp vs a
        v <- case correctType t a' of
            (Just a'') -> return a''
            Nothing -> Left $ TypeError "Number" a
        parityCheck es as >>= return . (v:)
        
    correctType NumberT [x@(Number _ _)] = Just x
    correctType LiteralT [x@(Literal _)] = Just x
    correctType ColourT [x@(Color _)] = Just x
    correctType _ _ = Nothing
    

-- helper methods for native functions

stringToUnit :: String -> Processed Unit
stringToUnit s = case parse unitParser "less" s of
    (Right u) -> return u
    (Left _) -> Left $ ProcessError ("Expected one of %, pt, px, em or nothing; got " ++ s)

stringToColour s = case parse colourParser "less" s of
    (Right c) -> return c
    (Left _) -> Left $ ProcessError ("Expected colour; got " ++ s)

-- implementations of native functions

unit vs args = do
    [Number _ n, Literal str] <- getArgs vs [(NumberT, Nothing), (LiteralT, Just (Literal ""))] args
    stringToUnit str >>= return . flip Number n

colour vs args = do
    [Literal str] <- getArgs vs [(LiteralT, Nothing)] args
    stringToColour str

mathFun f vs args = do
    [Number u n] <- getArgs vs [(NumberT, Nothing)] args
    return $ Number u (f n)
