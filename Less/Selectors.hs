module Less.Selectors (addSelectorContext) where

import Less.Types
import Data.Maybe (fromMaybe)

addSelectorContext :: Selector -> Selector -> Selector
addSelectorContext psel csel = concat $ map (\x -> fromMaybe (withoutParentRef x) (withParentRef x)) csel
    where
    withParentRef = subParentRefComb psel
    withoutParentRef c = map (\p -> appendCombToComb p ' ' c) psel

subParentRefComb :: Selector -> SelectorCombinator -> Maybe Selector
subParentRefComb psel (Terminus seq) = subParentRef psel seq
subParentRefComb psel (Combinator t seq comb) = case this of
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
