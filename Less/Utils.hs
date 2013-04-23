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

module Less.Utils(compile, report) where

import Less.Types
import Less.Parser (parseLess)
import Less.Processor (process)
import Text.ParserCombinators.Parsec (sourceLine, sourceColumn)
import Text.Parsec.Error (errorMessages, errorPos,
    Message(Message, SysUnExpect, UnExpect, Expect ))
import Control.Monad (liftM)
import Control.Monad.Trans.Either (hoistEither, EitherT(EitherT))


report :: String -> ProcessError -> String
report raw err = case err of
    (ProcessError s) -> s
    (TypeError exp got) -> "Expected " ++ exp ++ ", got: " ++ show got
    (ArgumentError _ _) ->  "Bad arguments" -- placeholder
    (ParseError perr) -> "Parse error at line " ++ (show lineNum) ++ ", column " ++ (show colNum) ++ "\n" ++ snippet ++ "\n" ++ report
        where
        snippet = (fileLines !! (lineNum - 1)) ++ "\n" ++ (replicate (colNum - 1) ' ') ++ "^"
        report = unlines $ map displayMessage messages
        messages = errorMessages perr
        pos = errorPos perr
        lineNum = sourceLine pos
        colNum = sourceColumn pos

        displayMessage msg = case msg of
            SysUnExpect s -> "Unexpected: " ++ s
            UnExpect s -> "Unexpected: " ++ s
            Expect s -> "Expected: " ++ s
            Message s -> s
        fileLines = lines raw


compile :: FilePath -> IOProcessed [CSS]
compile path = (EitherT $ liftM Right $ readFile path) >>= (hoistEither . parseLess) >>= process path
