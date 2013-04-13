import Less.Utils (compile, report)
import System.IO (stderr, hPutStrLn)
import System.Environment (getArgs)
import Control.Monad (liftM)
import Control.Monad.Trans.Either

main = do
    path <- liftM head $ getArgs
    eitherT
        (\e -> do { less <- readFile path; hPutStrLn stderr $ report less e })
        (putStr . concat . map show)
        (compile path)
