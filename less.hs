import Less.Utils (compile)
import System.IO (stderr, hPutStrLn)

main = do
    less <- getContents
    case compile less >>= return . concat . map show of
        Left err -> hPutStrLn stderr err
        Right val -> putStr val
