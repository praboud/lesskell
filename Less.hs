import LessParser
import LessProcessor
import Text.ParserCombinators.Parsec hiding (whitespace)

main = do
    file <- getContents
    case parse lessParser "less" file of
        Left err -> putStrLn $ "Parse error " ++ show err
        Right val -> putStr $ process val
