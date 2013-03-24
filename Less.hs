import LessParser
import LessProcessor
import Text.ParserCombinators.Parsec hiding (whitespace)
import Text.Parsec.Error

main = do
    file <- getContents
    case parse lessParser "less" file of
        Left err -> putStrLn $ displayError file err
        Right val -> putStr $ process val


displayError contents err = "Parse error at line " ++ (show lineNum) ++ ", column " ++ (show colNum) ++ "\n" ++ snippet ++ "\n" ++ report
    where
    snippet = ((lines contents) !! (lineNum - 1)) ++ "\n" ++ (replicate (colNum - 1) ' ') ++ "^"
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
