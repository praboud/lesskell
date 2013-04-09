module Less.Processor (process) where

import Less.Types
import Less.Selectors (addSelectorContext)
import Less.Expressions
import Data.Maybe (fromMaybe, fromJust, isJust)
import Control.Monad ((>=>))

---------------------
-- Main Processing --
---------------------
process :: [Statement] -> Either ProcessError [CSS]
process xs = do
    (m', v') <- bindMixVar s m v
    evalScope [] (Scope [Dummy] [] i p s m' v')
    where
    (s, [], i, p, m, v) = filterStatements xs

extractMixins :: [Scope] -> [Mixin]
extractMixins = map (\s -> Mixin [] s Nothing) . filter scopeIsSimpleClass
    where
    scopeIsSimpleClass (Scope [Terminus [ClassSelector _]] _ _ _ _ _ _) = True
    scopeIsSimpleClass _ = False

---------------------
-- Main Processors --
---------------------

evalScope alreadySeen scope@(Scope sel _ _ _ _ _ _) = do
    (rules, css) <- eval alreadySeen scope
    return $ (CSS sel rules):css

evalScopes alreadySeen sel m v = mapM (evalScope alreadySeen . prep) >=> return . concat
    where prep = (contextualizeSel sel) . (contextualizeEnv m v)

evalMul :: [Include] -> [Scope] -> Either ProcessError ([CSSRule], [CSS])
evalMul alreadySeen scopes = do
    (rules, css) <- mapM (eval alreadySeen) scopes >>= return . unzip
    return (concat rules, concat css)

eval :: [Include] -> Scope -> Either ProcessError ([CSSRule], [CSS])
eval alreadySeen (Scope sel r i p sub m v) = do
    -- evaluate variables in their own scope
    (m', v') <- bindMixVar sub m v
    -- evaluate all of our subscopes and rules
    ourRules <- mapM (evalRule v') r
    ourCSS <- evalScopes alreadySeen sel m' v' sub
    -- include all other mixins, but ignore includes we have already seen
    (includeRules, includeCSS) <-
        mapM (lookupMixin sel m' v') i
        >>= evalMul alreadySeen
    return (inherit includeRules ourRules, ourCSS ++ includeCSS)

----------------------
-- Helper Functions --
----------------------

lookupMixin :: Selector -> [Mixin] -> [Variable] -> Include -> Either ProcessError Scope
lookupMixin sel ((Mixin takes (Scope [Terminus [ClassSelector name1]] r i p subs m v) guards):_) vs include@(Include name2 gives)
    | nameMatch && parityMatch && guardMatch = return $ Scope sel r i p subs m allVars
    where
    nameMatch = name1 == name2
    parityMatch = isJust paramVars
    guardMatch = fromMaybe True $ guards >>= return . evalBool (inherit vs $ fromJust paramVars)
    paramVars = processParams takes gives
    -- allVars variables from parent scope, our scope, and given as parameters
    allVars = inherit vs (inherit v (fromJust paramVars))
lookupMixin sel (_:ms) vs include = lookupMixin sel ms vs include
lookupMixin _ _ _ (Include name _) = Left (ProcessError ("Could not match include " ++ name ))

contextualizeSel psel (Scope csel cr ci cp csub cm cv) =
    (Scope newSel cr ci cp csub cm cv)
    where
    newSel = addSelectorContext psel csel

contextualizeEnv :: [Mixin] -> [Variable] -> Scope -> Scope
contextualizeEnv pm pv (Scope csel cr ci cp csub cm cv) =
    Scope csel cr ci cp csub newMixins newVar
    where
    newMixins = inherit pm cm
    newVar = inherit pv cv

bindMixVar :: [Scope] -> [Mixin] -> [Variable] -> Either ProcessError ([Mixin], [Variable])
bindMixVar sub m v = do
    v' <- mapM (evalVar v) v -- not sure about this
    let m' = map (\(Mixin p s g) -> Mixin p (contextualizeEnv m' v' s) g)
             $ inherit (extractMixins sub) m
    return (m', v')

-- some stuff having to do with expressions
evalRule :: [Variable] -> Rule -> Either ProcessError CSSRule
evalRule vs (Rule prop exps) = mapM (evalExp vs) exps >>= return . CSSRule prop . unwords . map show . concat

evalVar vs (Variable id exps) = mapM (evalExp vs) exps >>= return . Variable id . concat

evalBool :: [Variable] -> BoolExpression -> Bool
evalBool _ _ = True

processParams :: [Param] -> [[Expression]] -> Maybe [Variable]
-- match params expected against expressions given
-- return Nothing if there is no match
-- return Just [Variable] if there is, where the variables are a list of new
-- bindings created by the params to expressions map
processParams [] [] = Just []
processParams [] _ = Nothing
processParams ((Param _ ):_) [] = Nothing
processParams ((DefaultParam v e):xs) [] = processParams xs [] >>= return . ((Variable v e):)
processParams (x:xs) (y:ys) = processParams xs ys >>= return . ((Variable name y):)
    where
    name = case x of
        Param n -> n
        DefaultParam n _ -> n
