module LessProcessor (process) where
import LessParser
import LessTypes
import Data.List
import Data.Function
import Data.Maybe (fromMaybe)

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

renderScope (Scope sel rules sub mix var) = (renderSelector sel) ++ " {\n" ++ unlines (map (renderRule . eval mix var) rules) ++ "}\n"

renderRule (Rule prop exp) = prop ++ ": " ++ exp ++ ";"
renderSelector = intercalate ", " . map show

-- FIXME PLACEHOLDER
eval :: [Mixin] -> [Variable] -> Rule -> Rule
eval _ _ r = r

process :: [Statement] -> String
process xs = concat $ map renderScope $ concat $ map flatten $ map f s
    where
    f (Scope sel rules sub mix var) = Scope sel rules sub (inherit m mix) (inherit v var)
    (s, [], m, v) = filterStatements xs

-- takes a scope, generates 1 or more scopes all of which have no children
-- each scope has the scope of all of its (former) parent scopes
flatten :: Scope -> [Scope]
flatten p@(Scope sel rules subs mixins variables) = case rules of
    [] -> ss
    otherwise -> s:ss
    where
    s = Scope sel rules [] mixins variables
    ss = map (addScopeContext p) $ concat $ map flatten subs

addScopeContext (Scope sp _ _ mp vp) (Scope sc rc sub mc vc) = Scope newSel rc sub newMix newVar
    where
    newSel = addSelectorContext sp sc
    newMix = inherit mp mc
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

appendCombToComb (Terminus seq) t comb = Combinator t seq comb
appendCombToComb (Combinator t1 seq comb1) t2 comb2 = Combinator t1 seq $ appendCombToComb comb1 t2 comb2

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
