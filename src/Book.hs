module Book where

import Prelude ()
import Relude

import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.IO as IO
import Control.Monad.Trans.Resource (allocate, runResourceT)

getDataDir :: IO FilePath
getDataDir = do
    dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
    Dir.createDirectoryIfMissing True dir
    return dir

writeGreetingFile :: IO ()
writeGreetingFile = do
    dir <- getDataDir
    h <- IO.openFile (dir </> "greeting.txt") WriteMode
    IO.hPutStrLn h "hello"
    IO.hPutStrLn h "world"
    IO.hClose h

writeGreetingSafe :: IO ()
-- @IO es un type application y permite informar a GHCI que el tipo base es IO. Se puede eliminar si de define el tipo de la función
writeGreetingSafe = runResourceT @IO do
    dir <- liftIO getDataDir
    (_releaseKey, h) <- allocate
        (IO.openFile (dir </> "greeting-safe.txt") WriteMode)
        IO.hClose
    liftIO (IO.hPutStrLn h "hello")
    liftIO (IO.hPutStrLn h "world")