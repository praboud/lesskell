import Less.Utils (compile, report)
import System.IO (stderr, hPutStrLn)
import Control.Monad.Trans.Either

main = do
    less <- getContents
    eitherT
        (hPutStrLn stderr . report less)
        (putStr . concat . map show)
        (compile less)
