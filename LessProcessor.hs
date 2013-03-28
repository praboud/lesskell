module LessProcessor (process) where

import LessTypes
import LessSelectors (addSelectorContext)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Control.Monad ((>=>))

---------------------
-- Main Processing --
---------------------
process :: [Statement] -> Either ProcessError [CSS]
process xs = do
    (m', v') <- bindMixVar s m v
    evalScope (Scope [Dummy] [] i s m' v')
    where
    (s, [], i, m, v) = filterStatements xs

extractMixins :: [Scope] -> [Mixin]
extractMixins = map (\s -> Mixin [] s Nothing) . filter scopeIsSimpleClass
    where
    scopeIsSimpleClass (Scope [Terminus [ClassSelector _]] _ _ _ _ _) = True
    scopeIsSimpleClass _ = False

---------------------
-- Main Processors --
---------------------

-- main function which squashes less types down to straight css
-- this gets a little complicated, so it bears commenting
-- the overall concept is to convert a scope
-- (less version of a class with variables, mixins and subscopes)
-- into a css class
evalScope :: Scope -> Either ProcessError [CSS]
evalScope (Scope sel r i sub m v) = do
    -- evaluate variables in their own scope
    (m', v') <- bindMixVar sub m v

    -- include all include statements
    -- these introduce new subscopes and rules into the scope
    (includeRules, includeCSS) <- mapM (lookupMixin sel m' v') i >>= evalIncludes []

    -- evaluate all of our subscopes
    ourCSS <- evalScopes sel m' v' sub

    -- eval all rules with the context from this scope
    ourRules <- mapM (evalRule v') r

    -- assemble our rules and subs, and those from includes
    -- our own rules take precedence over included rules
    return $ ((CSS sel (inherit includeRules ourRules)) : ourCSS ++ includeCSS)

evalScopes sel m v = mapM (evalScope . (contextualizeSel sel) . (contextualizeEnv m v)) >=> return . concat 

evalIncludes :: [Include] -> [Scope] -> Either ProcessError ([CSSRule], [CSS])
evalIncludes alreadySeen scopes = do
    (rules, css) <- mapM (evalInclude alreadySeen) scopes >>= return . unzip
    return (concat rules, concat css)

evalInclude :: [Include] -> Scope -> Either ProcessError ([CSSRule], [CSS])
evalInclude alreadySeen (Scope sel r i sub m v) = do
    (m', v') <- bindMixVar sub m v
    ourRules <- mapM (evalRule v') r
    ourCSS <- evalScopes sel m' v' sub
    (includeRules, includeCSS) <-
        mapM (lookupMixin sel m' v' >=> return . contextualizeSel sel) i
        >>= evalIncludes alreadySeen
    return (ourRules ++ includeRules, ourCSS ++ includeCSS)

----------------------
-- Helper Functions --
----------------------

lookupMixin :: Selector -> [Mixin] -> [Variable] -> Include -> Either ProcessError Scope
lookupMixin sel ((Mixin takes (Scope [Terminus [ClassSelector name1]] r i subs m v) guards):_) vs include@(Include name2 gives)
    | nameMatch && parityMatch && guardMatch = return $ Scope sel r i subs m allVars
    where
    nameMatch = name1 == name2
    parityMatch = isJust paramVars
    guardMatch = fromMaybe True $ guards >>= return . evalBool (inherit vs $ fromJust paramVars)
    paramVars = processParams takes gives
    -- allVars variables from parent scope, our scope, and given as parameters
    allVars = inherit vs (inherit v (fromJust paramVars))
lookupMixin sel (_:ms) vs include = lookupMixin sel ms vs include
lookupMixin _ _ _ (Include name _) = Left (ProcessError ("Could not match include " ++ name ))

contextualizeSel psel (Scope csel cr ci csub cm cv) = 
    (Scope newSel cr ci csub cm cv)
    where
    newSel = addSelectorContext psel csel

contextualizeEnv :: [Mixin] -> [Variable] -> Scope -> Scope
contextualizeEnv pm pv (Scope csel cr ci csub cm cv) = Scope csel cr ci csub newMixins newVar
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
lookupVariable [] id = Left $ ProcessError ("Unbound variable " ++ id)
lookupVariable ((Variable name val):vs) id
    | name == id = return val
    | otherwise = lookupVariable vs id

evalRule :: [Variable] -> Rule -> Either ProcessError CSSRule
evalRule vs (Rule prop exp) = evalExp vs exp >>= return . CSSRule prop

evalVar vs (Variable id exp) = evalExp vs exp >>= return . Variable id . Literal

evalExp :: [Variable] -> Expression -> Either ProcessError String
evalExp _ (Literal s) = return s
evalExp vs (Identifier v) = lookupVariable vs v >>= evalExp vs
-- FIXME: I need to evaluate expressions

evalBool :: [Variable] -> BoolExpression -> Bool
evalBool _ _ = True

processParams :: [Param] -> [Expression] -> Maybe [Variable]
processParams [] [] = Just []
processParams ((Param _ ):_) [] = Nothing
processParams ((DefaultParam v e):xs) [] = processParams xs [] >>= return . ((Variable v e):)
processParams [] (_:_) = Nothing
processParams (x:xs) (y:ys) = processParams xs ys >>= return . ((Variable name y):)
    where
    name = case x of
        Param n -> n
        DefaultParam n _ -> n
