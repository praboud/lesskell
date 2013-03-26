import LessTypes
import LessParser
import LessProcessor
import Text.ParserCombinators.Parsec hiding (whitespace)
import Text.Parsec.Error

class ErrorReport x where
    report :: [String] -> x -> String

instance ErrorReport ParseError where
    report = displayError

instance ErrorReport ProcessError where
    report _ (ProcessError s) = s

captureError :: ErrorReport e => [String] -> Either e x -> Either String x
captureError _ r@(Right x) = Right x
captureError lines l@(Left e) = Left $ report lines e

main = do
    file <- getContents
    let fileLines = lines file
    let parsed = (captureError fileLines $ parse lessParser "less" file)
    --print parsed
    let processed = parsed >>= captureError fileLines . process
    case processed of
        Left err -> putStrLn err
        Right val -> putStr val


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
