module LessProcessor (process) where
import LessParser
import LessTypes
import Data.List
import Data.Function

class Inherit x where
    key :: x -> String
    conflict :: x -> x -> [x]
    
instance Inherit Mixin where
    key (Mixin _ (Scope sel _ _ _ _) _) = sel
    conflict s1@(Mixin a1 b1 g1) s2@(Mixin a2 b2 g2)
        | length a1 /= length a2 = [s1, s2]
        | g1 /= g2 = [s1, s2]
        | otherwise = [s2]

instance Inherit Variable where
    key (Variable id _) = id
    conflict v1 v2 = [v2]

instance Inherit Rule where
    key (Rule prop _) = prop
    conflict v1 v2 = [v2]

renderScope (Scope sel rules sub mix var) = sel ++ "{\n" ++ unlines (map (show . eval mix var) rules) ++ "}\n"

renderRule (Rule prop exp) = prop ++ ": " ++ exp ++ ";"

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
    ss = map (addContext p) $ concat $ map flatten subs

addContext (Scope sp _ _ mp vp) (Scope sc rc sub mc vc) = Scope (joinScope sp sc) rc sub (inherit mp mc) (inherit vp vc)

-- FIXME PLACEHOLDER
joinScope x y =  x ++ (' ' : y)

inherit :: Inherit x => [x] -> [x] -> [x]
inherit parent child = inherit_h (sortOnKey parent) (sortOnKey child)
    where
    sortOnKey = sortBy (compare `on` key)
    inherit_h [] ys = ys
    inherit_h xs [] = xs
    inherit_h xt@(x:xs) yt@(y:ys)
        | kx == ky = conflict x y ++ inherit xs ys
        | kx < ky = x : inherit xs yt
        | otherwise = y : inherit xt ys
        where
        kx = key x
        ky = key y
