{-
 - This file is part of Lesskell.
 -
 - Lesskell is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - Lesskell is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with Lesskell.  If not, see <http://www.gnu.org/licenses/>.
 -}

module Less.Selectors (addSelectorContext) where

import Less.Types
import Data.Maybe (fromMaybe)

addSelectorContext :: Selector -> Selector -> Selector
addSelectorContext psel csel = concat $ map (\x -> fromMaybe (withoutParentRef x) (withParentRef x)) csel
    where
    withParentRef = subParentRefComb psel
    withoutParentRef c = map (\p -> appendCombToComb p ' ' c) psel

-- return nothing if no parent reference
-- return selector group with parent references replaced if there is
-- so .foo, .bar { &.baz {} } -> .foo.baz, .bar.baz {}
subParentRefComb :: Selector -> SelectorCombinator -> Maybe Selector
subParentRefComb psel (Terminus seq) = subParentRef psel seq
subParentRefComb psel (Combinator t seq comb) = case this of
    Nothing -> next >>= Just . map (Combinator t seq)
    Just this' -> Just $ map (\(p, c) -> appendCombToComb p t c) $ cross this' $ fromMaybe [comb] next
    where
    this = subParentRef psel seq
    next = subParentRefComb psel comb
subParentRefComb _ Dummy = undefined

appendCombToComb Dummy ' ' comb = comb
appendCombToComb Dummy _ _ = undefined
appendCombToComb (Terminus seq) t comb = Combinator t seq comb
appendCombToComb (Combinator t1 seq comb1) t2 comb2 = Combinator t1 seq $ appendCombToComb comb1 t2 comb2
-- FIXME: I should have a ProcessError cascade up

subParentRef :: Selector -> SimpleSelectorSeq -> Maybe Selector
subParentRef _ [] = Nothing
subParentRef psel (c:cs) = case c of
    ParentRef -> Just $ map (flip appendSeqToComb cs) psel
    _ -> subParentRef psel cs >>= return . map (flip prependSeqToComb c)

prependSeqToComb :: SelectorCombinator -> SimpleSelector -> SelectorCombinator
prependSeqToComb (Terminus xs) x = Terminus (x:xs)
prependSeqToComb (Combinator t xs c) x = Combinator t (x:xs) c
prependSeqToComb Dummy _ = undefined

appendSeqToComb :: SelectorCombinator -> SimpleSelectorSeq -> SelectorCombinator
appendSeqToComb (Combinator t xs c) ys = Combinator t xs $ appendSeqToComb c ys
appendSeqToComb (Terminus xs) ys = Terminus (xs ++ ys)
appendSeqToComb Dummy _ = undefined

cross :: [x] -> [y] -> [(x, y)]
cross xs ys = concat [[(x, y) | y <- ys] | x <- xs]
