{-
 - This file is part of Lesskell.
 -
 - Lesskell is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - Lesskell is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with Lesskell.  If not, see <http://www.gnu.org/licenses/>.
 -}

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
