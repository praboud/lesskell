module Less.Processor (process) where

import Less.Types
import Less.Selectors (addSelectorContext)
import Less.Expressions
import Less.Parser (parseLess)

import Data.Maybe (fromMaybe, fromJust, isJust)
import Control.Monad ((>=>), liftM, foldM)
import Control.Monad.Trans.Either

import System.FilePath (takeExtension, addExtension, replaceFileName)

---------------------
-- Main Processing --
---------------------
process :: FilePath -> [Statement] -> IOProcessed [CSS]
process path xs = do
    (m', v') <- hoistEither $ bindMixVar s m v
    evalScope path [] (Scope [Dummy] [] i p s m' v')
    where
    (s, [], i, p, m, v) = filterStatements xs

---------------------
-- Main Processors --
---------------------

evalScope :: FilePath -> [Include] -> Scope -> IOProcessed [CSS]
evalScope path alreadySeen scope@(Scope sel _ _ _ _ _ _) = do
    (rules, css) <- eval path alreadySeen scope
    return $ (CSS sel rules):css

evalScopes :: FilePath -> [Include] -> Selector -> [Mixin] -> [Variable] -> [Scope] -> IOProcessed [CSS]
evalScopes path alreadySeen sel m v = mapM (evalScope path alreadySeen . prep) >=> return . concat
    where prep = (contextualizeSel sel) . (contextualizeEnv m v)

evalMul :: FilePath -> [Include] -> [Scope] -> IOProcessed ([CSSRule], [CSS])
evalMul path alreadySeen scopes = do
    (rules, css) <- mapM (eval path alreadySeen) scopes >>= return . unzip
    return (concat rules, concat css)

eval :: FilePath -> [Include] -> Scope -> IOProcessed ([CSSRule], [CSS])
eval path alreadySeen (Scope sel r i p sub m v) = do
    (ps, pr, pi, pm, pv) <- evalImports path p
    -- evaluate variables in their own scope
    (m', v') <- hoistEither $ bindMixVar (sub ++ ps) (inherit pm m) (inherit pv v)
    -- evaluate all of our subscopes and rules
    ourRules <- hoistEither $ mapM (evalRule v') (inherit pr r)
    ourCSS <- evalScopes path alreadySeen sel m' v' (sub ++ ps)
    -- include all other mixins, but ignore includes we have already seen
    (includeRules, includeCSS) <-
        hoistEither (mapM (lookupMixin sel m' v') (i ++ pi))
        >>= evalMul path alreadySeen
    right (inherit includeRules ourRules, ourCSS ++ includeCSS)

----------------------
-- Helper Functions --
----------------------

extractMixins :: [Scope] -> [Mixin]
extractMixins = map (\s -> Mixin [] s Nothing) . filter scopeIsSimpleClass
    where
    scopeIsSimpleClass (Scope [Terminus [ClassSelector _]] _ _ _ _ _ _) = True
    scopeIsSimpleClass _ = False

lookupMixin :: Selector -> [Mixin] -> [Variable] -> Include -> Processed Scope
lookupMixin sel ((Mixin takes (Scope [Terminus [ClassSelector name1]] r i p subs m v) guards):_) vs (Include name2 gives)
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

bindMixVar :: [Scope] -> [Mixin] -> [Variable] -> Processed ([Mixin], [Variable])
bindMixVar sub m v = do
    v' <- mapM (evalVar v) v -- not sure about this
    let m' = map (\(Mixin p s g) -> Mixin p (contextualizeEnv m' v' s) g)
             $ inherit (extractMixins sub) m
    return (m', v')

-- some stuff having to do with expressions
evalRule :: [Variable] -> Rule -> Processed CSSRule
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

-- recursively process all imports
-- don't import things twice
evalImports :: FilePath -> [Import] -> IOProcessed ([Scope], [Rule], [Include], [Mixin], [Variable])
evalImports path = liftM snd . foldM (evalImport_h path) ([], ([], [], [], [], [])) . map assumeLessExtension
    where
    assumeLessExtension path = case takeExtension path of
        "" -> addExtension path "less"
        _ -> path
    evalImport_h relPath acc@(alreadySeen, (s1, r1, i1, m1, v1)) path
        | path `elem` alreadySeen = return acc
        | otherwise = do
            (s2, r2, i2, p2, m2, v2) <- liftM filterStatements $ EitherT $ liftM parseLess $ readFile path'
            let combined = (s1 ++ s2, r1 ++ r2, i1 ++ i2, m1 ++ m2, v1 ++ v2)
            foldM (evalImport_h path') (alreadySeen', combined) p2
        where
        alreadySeen' = path : alreadySeen
        path' = replaceFileName relPath path
