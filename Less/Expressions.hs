module Less.Expressions (evalExp) where

import Less.Types
import Data.Maybe (fromJust)

evalExp :: [Variable] -> Expression -> Either ProcessError [Expression]
evalExp _ x@(Literal _) = return [x]
evalExp _ x@(Number _ _) = return [x]
evalExp _ x@(Color _) = return [x]
evalExp vs (BinOp op l r) = do
    (Number unit l') <- extractNumber vs Nothing l
    (Number _ r') <- extractNumber vs (Just unit) r
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
    

extractNumber :: [Variable] -> Maybe Unit -> Expression -> Either ProcessError Expression
extractNumber vs uexp e = do
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

lookupVariable :: [Variable] -> Identifier -> Either ProcessError [Expression]
lookupVariable [] id = Left $ ProcessError ("Unbound variable " ++ id)
lookupVariable ((Variable name val):vs) id
    | name == id = return val
    | otherwise = lookupVariable vs id
