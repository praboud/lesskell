module LessProcessor (process) where
import LessParser
import LessTypes
import Data.List
import Data.Function
import Data.Maybe (fromMaybe, fromJust, isJust)
import Control.Monad ((>=>))

class Ord x => Inherit x where
    conflict :: x -> x -> [x]

instance Eq Mixin where
    m1 == m2 = (selector $ body m1) == (selector $ body m2)

instance Ord Mixin where
    compare m1 m2 = compare (selector $ body m1) (selector $ body m2)
    
instance Inherit Mixin where
    conflict s1@(Mixin a1 b1 g1) s2@(Mixin a2 b2 g2)
        | length a1 /= length a2 = [s1, s2]
        | g1 /= g2 = [s1, s2]
        | otherwise = [s2]

instance Eq Variable where
    (Variable id1 _) == (Variable id2 _) = id1 == id2

instance Ord Variable where
    compare (Variable id1 _) (Variable id2 _) = compare id1 id2

instance Inherit Variable where
    conflict v1 v2 = [v2]

instance Eq Rule where
    (Rule p1 _) == (Rule p2 _) = p1 == p2

instance Ord Rule where
    compare (Rule p1 _) (Rule p2 _) = compare p1 p2

instance Inherit Rule where
    conflict v1 v2 = [v2]
{-
renderScope (Scope sel rules sub mix var) = (renderSelector sel) ++ " {\n" ++ unlines (map (renderRule . eval mix var) rules) ++ "}\n"

renderRule (Rule prop exp) = prop ++ ": " ++ exp ++ ";"
renderSelector = intercalate ", " . map show

-- FIXME PLACEHOLDER
eval :: [Mixin] -> [Variable] -> Rule -> Rule
eval _ _ r = r
-}

process :: [Statement] -> Either ProcessError String
process xs = eval (Scope [Dummy] [] i s m' v)  >>= return . concat . map show 
    where
    (s, [], i, m, v) = filterStatements xs
    m' = inherit (extractMixins s) m

extractMixins :: [Scope] -> [Mixin]
extractMixins = map (\s -> Mixin [] s []) . filter scopeIsSimpleClass
    where
    scopeIsSimpleClass (Scope [Terminus [ClassSelector name1]] _ _ _ _ _) = True
    scopeIsSimpleClass _ = False
    

-- output intermediates
data CSS = CSS Selector [CSSRule]
data CSSRule = CSSRule Property String

instance Show CSS where
    show (CSS sel rules) = case rules of
        [] -> ""
        rules -> (intercalate ", " $ map show sel) ++ " {\n" ++ (intercalate ";\n" $ map show rules) ++ "\n}\n"

instance Show CSSRule where
    show (CSSRule p v) = p ++ ": " ++ v


-- main function which squashes less types down to straight css
-- this gets a little complicated, so it bares commenting
-- the overall concept is to convert a scope (less version of a class with variables, mixins and subscopes)
-- into a css class
eval :: Scope -> Either ProcessError [CSS]
eval scope@(Scope sel r i sub m v) = do
    -- include all include statements
    -- these introduce new subscopes and rules into the scope
    includes <- mapM (lookupMixin m v) i

    -- *** evaluate all subscopes ***
    -- recursively evaluate all subscopes with the mixins/variable from this scope
    -- append the selector of this scope to these subscopes
    ourSubs <- fmap concat $ mapM (contextualizeEnv scope >=> eval . (contextualizeSel sel)) sub
    -- recursively evaluate all subscopes introduced by mixins 
    -- use the mixins/variables from the mixin scope, then from this scope (in that order of precendence)
    -- append the selector of this scope to these subscopes, not the selector of the mixin
    includeSubs <- fmap (concat . concat) $ mapM (\iscope@(Scope _ _ _ s _ _)  -> mapM (contextualizeEnv iscope >=> eval . contextualizeSel sel) s) includes
    
    -- *** evaluate all rules ***
    -- with the context from this scope
    ourRules <- mapM (evalRule v) r
    -- with the context from the respective mixin
    includeRules <- mapM (\(Scope _ r _ _ _ v) -> mapM (evalRule v) r) includes >>= return . concat
    return $ ((CSS sel (ourRules++includeRules)) : ourSubs ++ includeSubs)
    where
    contextualizeEnv :: Scope -> Scope -> Either ProcessError Scope
    contextualizeEnv (Scope _ _ _ psub pm pv) (Scope csel cr ci csub cm cv) =
        return $ Scope csel cr ci csub newMixins newVar
            where
            newMixins = inherit (extractMixins psub) $ inherit pm cm
            newVar = inherit pv cv
    contextualizeSel psel (Scope csel cr ci csub cm cv) = (Scope newSel cr ci csub cm cv)
        where
        newSel = addSelectorContext psel csel

    lookupMixin :: [Mixin] -> [Variable] -> Include -> Either ProcessError Scope
    lookupMixin [] _ (Include name _) = Left (ProcessError ("Could not match include " ++ name ))
    lookupMixin ((Mixin takes s@(Scope sel@[Terminus [ClassSelector name1]] r i subs m v) guards):ms) vs include@(Include name2 gives)
        | nameMatch && parityMatch && guardMatch = return $ Scope [Terminus [ParentRef]] r i subs m allVars 
        | otherwise = lookupMixin ms vs include
        where
        nameMatch = name1 == name2
        parityMatch = isJust paramVars
        guardMatch = all (evalBool $ inherit vs $ fromJust paramVars) guards
        paramVars = processParams takes gives
        -- allVars variables from parent scope, our scope, and given as parameters
        allVars = inherit vs (inherit v (fromJust paramVars))

evalRule :: [Variable] -> Rule -> Either ProcessError CSSRule
evalRule vs (Rule prop exp) = return $ CSSRule prop (show exp)

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


parityP :: [x] -> [y] -> Bool
parityP [] [] = True
parityP _ [] = False
parityP [] _ = False
parityP (_:xs) (_:ys) = parityP xs ys

addScopeContext parent@(Scope sp _ _ _ mp vp) (Scope sc rc ic sub mc vc) = Scope newSel rc ic sub newMix newVar
    where
    newSel = addSelectorContext sp sc
    newMix = inherit ((Mixin [] parent []) : mp) mc
    newVar = inherit vp vc

addSelectorContext :: Selector -> Selector -> Selector
addSelectorContext psel csel = concat $ map (\x -> fromMaybe (withoutParentRef x) (withParentRef x)) csel
    where
    withParentRef = subParentRefComb psel
    withoutParentRef c = map (\p -> appendCombToComb p ' ' c) psel

subParentRefComb :: Selector -> SelectorCombinator -> Maybe Selector
subParentRefComb psel (Terminus seq) = subParentRef psel seq
subParentRefComb psel csel@(Combinator t seq comb) = case this of
    Nothing -> next >>= Just . map (Combinator t seq)
    Just this' -> Just $ map (\(p, c) -> appendCombToComb p t c) $ cross this' $ fromMaybe [comb] next
    where
    this = subParentRef psel seq
    next = subParentRefComb psel comb

appendCombToComb Dummy ' ' comb = comb
--appendCombToComb comb ' ' Dummy = comb
appendCombToComb (Terminus seq) t comb = Combinator t seq comb
appendCombToComb (Combinator t1 seq comb1) t2 comb2 = Combinator t1 seq $ appendCombToComb comb1 t2 comb2
-- FIXME: I should have a ProcessError cascade up
appendCombToComb _ _ _ = undefined

subParentRef :: Selector -> SimpleSelectorSeq -> Maybe Selector
subParentRef _ [] = Nothing
subParentRef psel (c:cs) = case c of
    ParentRef -> Just $ map (flip appendSeqToComb cs) psel
    _ -> subParentRef psel cs >>= return . map (flip prependSeqToComb c)

prependSeqToComb :: SelectorCombinator -> SimpleSelector -> SelectorCombinator
prependSeqToComb (Terminus xs) x = Terminus (x:xs)
prependSeqToComb (Combinator t xs c) x = Combinator t (x:xs) c

appendSeqToComb :: SelectorCombinator -> SimpleSelectorSeq -> SelectorCombinator
appendSeqToComb (Combinator t xs c) ys = Combinator t xs $ appendSeqToComb c ys
appendSeqToComb (Terminus xs) ys = Terminus (xs ++ ys)

cross :: [x] -> [y] -> [(x, y)]
cross xs ys = concat [[(x, y) | y <- ys] | x <- xs]


inherit :: Inherit x => [x] -> [x] -> [x]
inherit parent child = inherit_h (sort parent) (sort child)
    where
    inherit_h [] ys = ys
    inherit_h xs [] = xs
    inherit_h xt@(x:xs) yt@(y:ys)
        | x == y = conflict x y ++ inherit xs ys
        | x < y = x : inherit xs yt
        | otherwise = y : inherit xt ys
