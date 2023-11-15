module Book where

import Prelude ()
import Relude

import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.IO as IO
import Control.Monad.Trans.Resource (allocate, runResourceT, ResourceT, ReleaseKey)
import qualified System.IO as Io
import System.IO (hShow)

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

fileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
fileResource path mode = allocate (Io.openFile path mode) IO.hClose

writeGreetingSafe :: IO ()
writeGreetingSafe = runResourceT @IO do
    dir <- liftIO getDataDir
    (_releaseKey, h) <- fileResource (dir </> "greeting-safe.txt") WriteMode
    liftIO (IO.hPutStrLn h "hello")
    liftIO (IO.hPutStrLn h "world")

handlePrintTest :: IO ()
handlePrintTest = do 
    dir <- getDataDir
    h <- IO.openFile (dir </> "greeting.txt") WriteMode
    print h
    hShow h >>= print
    IO.hClose h
    hShow h >>= print
