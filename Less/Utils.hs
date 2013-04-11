module Less.Utils(compile, report) where

import Less.Types
import Less.Parser (parseLess)
import Less.Processor (process)
import Text.ParserCombinators.Parsec (sourceLine, sourceColumn)
import Text.Parsec.Error (errorMessages, errorPos, ParseError,
    Message(Message, SysUnExpect, UnExpect, Expect ))
import Control.Monad ((>=>))
import Control.Monad.Trans.Either (hoistEither)


report :: String -> ProcessError -> String
report raw err = case err of
    (ProcessError s) -> s
    (TypeError exp got) -> "Expected " ++ exp ++ ", got: " ++ show got
    (ArgumentError exp got) ->  "Bad arguments" -- placeholder
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
        fileLines = lines raw


compile :: String -> IOProcessed [CSS]
compile = (hoistEither . parseLess) >=> process
