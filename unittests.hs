import Less.Utils(compile)
import Less.Types
import Data.Maybe (isJust, fromJust)
import Control.Monad (foldM)
import Control.Monad.Trans.Either
import System.FilePath (takeExtension, replaceExtension, joinPath)
import System.Directory (getDirectoryContents)
import Text.Printf (printf)

-- responsible for checking if two css sets are equivalent
equivalentCSS :: [CSS] -> [CSS] -> Bool
equivalentCSS c1 c2 = isJust $ foldM (flip ruleInCSS) c1 c2

ruleInCSS :: CSS -> [CSS] -> Maybe [CSS]
ruleInCSS _ [] = Nothing
ruleInCSS (CSS s1 rs1) ((CSS s2 rs2):rest)
    | s1 == s2 && isJust inSet = Just ((CSS s2 (fromJust inSet)):rest)
    where
    inSet = foldM (flip ruleInSet) rs2 rs1
ruleInCSS c (r2:rs) = ruleInCSS c rs >>= return . (r2:)

ruleInSet :: CSSRule -> [CSSRule] -> Maybe [CSSRule]
ruleInSet _ [] = Nothing
ruleInSet r1 (r2:rs)
    | r1 == r2 = return rs
    | otherwise = ruleInSet r1 rs >>= return . (r2:)

-- responsible for comparing less and css to check for equivalence
runTestCase :: FilePath -> FilePath -> IO Bool
runTestCase less css = do
    let compiled = do
        less' <- compile less
        css' <- compile css
        return (less', css')
    eitherT
        (return . const False)
        (return . uncurry equivalentCSS)
        compiled

lessTestDir = "tests"


main = do
    lessFiles <- getDirectoryContents lessTestDir
        >>= return . filter ((".less"==) . takeExtension)
            . map (\d -> joinPath [lessTestDir, d])
       -- >>= mapM canonicalizePath
    let expectedFiles = map (flip replaceExtension ".css") lessFiles
    results <- mapM (uncurry runTestCase) $ zip lessFiles expectedFiles
    let total = length results
    let passed = length $ filter (==True) results

    mapM_ (printCase total) $ zip3 results lessFiles [1..]

    putStrLn ""
    if total == passed
        then putStrLn "aw yiss"
        else printf "%d / %d tests passed\n" passed total
    where
    printCase :: Int -> (Bool, FilePath, Int) -> IO()
    printCase t (success, path, n) = printf "[%d / %d] test %s %s\n" n t path msg
        where
        msg = if success then "passed" else "failed"
