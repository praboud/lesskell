module LessExpressions (evalExp) where

import LessTypes
import Data.Maybe (fromJust)

evalExp :: [Variable] -> Expression -> Either ProcessError [Expression]
evalExp _ x@(Literal _) = return [x]
evalExp _ x@(Number _ _) = return [x]
evalExp vs (BinOp op l r) = do
    (unit, l') <- extractNumber vs Nothing l
    (_, r') <- extractNumber vs (Just unit) r
    let op' = evalOp op
    return $ [Number unit (op' l' r')]
evalExp vs (Identifier 1 v) = lookupVariable vs v >>= mapM (evalExp vs) >>= return . concat

evalOp = fromJust . flip lookup ops

ops = 
    [ ('+', (+))
    , ('-', (-))
    , ('*', (*))
    , ('/', (/))
    ]
    

extractNumber :: [Variable] -> Maybe Unit -> Expression -> Either ProcessError (Unit, Double)
extractNumber vs u1 e = do
    evald <- evalExp vs e
    case evald of
        [n@(Number u2 val)] -> if maybe True (u2==) u1
            then return (u2, val)
            else Left $ TypeError (show u1) e
        x -> Left $ TypeError "number" e

lookupVariable :: [Variable] -> Identifier -> Either ProcessError [Expression]
lookupVariable [] id = Left $ ProcessError ("Unbound variable " ++ id)
lookupVariable ((Variable name val):vs) id
    | name == id = return val
    | otherwise = lookupVariable vs id
