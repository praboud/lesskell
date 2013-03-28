module Less(compile) where
import LessTypes (ProcessError(ProcessError), CSS(CSS))
import LessParser (lessParser)
import LessProcessor (process)
import Text.ParserCombinators.Parsec (parse, sourceLine, sourceColumn)
import Text.Parsec.Error (errorMessages, errorPos, ParseError,
    Message(Message, SysUnExpect, UnExpect, Expect ))
import System.IO (stderr, hPutStrLn)

class ErrorReport x where
    report :: [String] -> x -> String

instance ErrorReport ParseError where
    report = displayError

instance ErrorReport ProcessError where
    report _ (ProcessError s) = s

captureError :: ErrorReport e => [String] -> Either e x -> Either String x
captureError _ (Right x) = Right x
captureError lines (Left e) = Left $ report lines e

compile :: String -> Either String [CSS]
compile contents = (captureError fileLines $ parse lessParser "less" contents)
    >>= captureError fileLines . process
    where
    fileLines = lines contents

{-
main = do
    less <- getContents
    case compile less >>= return . concat . map show of
        Left err -> hPutStrLn stderr err
        Right val -> putStr val
-}


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
