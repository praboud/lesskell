module Less.Utils(compile) where

import Less.Types (ProcessError(ProcessError, TypeError), CSS(CSS))
import Less.Parser (lessParser)
import Less.Processor (process)
import Text.ParserCombinators.Parsec (parse, sourceLine, sourceColumn)
import Text.Parsec.Error (errorMessages, errorPos, ParseError,
    Message(Message, SysUnExpect, UnExpect, Expect ))

class ErrorReport x where
    report :: [String] -> x -> String

instance ErrorReport ParseError where
    report = displayError

instance ErrorReport ProcessError where
    report _ (ProcessError s) = s
    report _ (TypeError exp got) = "Expected " ++ exp ++ ", got: " ++ show got

captureError :: ErrorReport e => [String] -> Either e x -> Either String x
captureError _ (Right x) = Right x
captureError lines (Left e) = Left $ report lines e

compile :: String -> Either String [CSS]
compile contents = (captureError fileLines $ parse lessParser "less" contents)
    >>= captureError fileLines . process
    where
    fileLines = lines contents

displayError contents err = "Parse error at line " ++ (show lineNum) ++ ", column " ++ (show colNum) ++ "\n" ++ snippet ++ "\n" ++ report
    where
    snippet = (contents !! (lineNum - 1)) ++ "\n" ++ (replicate (colNum - 1) ' ') ++ "^"
    report = unlines $ map displayMessage messages
    messages = errorMessages err
    pos = errorPos err
    lineNum = sourceLine pos
    colNum = sourceColumn pos

    displayMessage msg = case msg of
        SysUnExpect s -> "Unexpected: " ++ s
        UnExpect s -> "Unexpected: " ++ s
        Expect s -> "Expected: " ++ s
        Message s -> s
