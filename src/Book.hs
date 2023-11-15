{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Book where

import           Prelude                      ()
import           Relude

import qualified Control.Exception.Safe       as Ex
import           Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate,
                                               runResourceT)
import qualified Data.Char                    as Char
import           Data.Text                    ()
import qualified Data.Text                    as T

import qualified Data.ByteString              as BS
import qualified Data.Text.IO                 as T
import qualified System.Directory             as Dir
import           System.FilePath              ((</>))
import qualified System.IO                    as IO
import qualified System.IO                    as Io
import           System.IO                    (hShow)



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

howManyHandles :: IO ()
howManyHandles = runResourceT @IO do
    hs <- openManyHandles
    putStrLn ("Opened " <> show (length hs) <> " handles")

openManyHandles :: ResourceT IO [Handle]
openManyHandles = do
    h <- fileResourceMaybe
    case h of
      Nothing -> pure []
      Just h' -> do
        hs <- openManyHandles
        pure $ h' : hs


fileResourceMaybe :: ResourceT IO (Maybe Handle)
fileResourceMaybe = do
  dir <- liftIO getDataDir
  result <- Ex.tryIO do
    (_, h) <- allocate (IO.openFile (dir </> "greeting.txt") ReadMode) IO.hClose
    pure h
  case result of
    Right x -> pure $ Just x
    Left e -> do
      print $ displayException e
      pure Nothing

helloTextFile :: IO ()
helloTextFile = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> "greeting.txt") WriteMode
  liftIO do
    T.hPutStrLn h (T.pack "hello")
    T.hPutStrLn h (T.pack "world")

printFileContentsUpperCase :: IO ()
printFileContentsUpperCase = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
  liftIO (printCapitalizedText h)

printCapitalizedText :: Handle -> IO ()
printCapitalizedText h = continue
  where
    continue = do
      chunk <- T.hGetChunk h
      unless (T.null chunk) (do
        T.putStr (T.toUpper chunk)
        continue)

repeatUntilIO :: IO chunk -- ^ Producer of chunks
  -> (chunk -> Bool) -- ^ Does chunk indicate end of file?
  -> (chunk -> IO x) -- ^ What to do with each chunk
  -> IO ()
repeatUntilIO = repeatUntil

printFileContentsUpperCase2 :: IO ()
printFileContentsUpperCase2 = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
  liftIO (repeatUntilIO (T.hGetChunk h) T.null (T.putStr . T.toUpper))

digitsOnly :: Text -> Text
digitsOnly = T.filter Char.isDigit

capitalizeLast :: Text -> Text
capitalizeLast xs = T.init xs <> T.singleton (Char.toUpper (T.last xs))

unParen :: Text -> Maybe Text
unParen xs = case T.unsnoc xs of
  Nothing       -> Nothing
  Just (xs', x) -> if x == ')' then Just xs' else Nothing


characterCount :: FilePath -> IO Int
characterCount path = runResourceT @IO do
  (_, h) <- fileResource path ReadMode
  liftIO (countCharacters h)

countCharacters :: Handle -> IO Int
countCharacters h = do
  chunk <- T.hGetChunk h
  if T.null chunk
    then return 0
    else do
      rest <- countCharacters h
      return (T.length chunk + rest)

unless' :: Bool -> IO () -> IO ()
unless' True  _ = return ()
unless' False x = x

when' :: Bool -> IO () -> IO ()
when' True  x = x
when' False _ = return ()

repeatUntil :: (Monad m) => m chunk -> (chunk -> Bool) -> (chunk -> m x) -> m ()
repeatUntil producer isEnd action = continue
  where
    continue = do
      chunk <- producer
      if isEnd chunk
        then return ()
        else action chunk >> continue

exampleBytes :: [Word8]
exampleBytes = [104, 101, 108, 108, 111] :: [Word8]

copyGreetingFile :: IO ()
copyGreetingFile = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h1) <-
    binaryFileResource (dir </> "greeting.txt") ReadMode
  (_, h2) <-
    binaryFileResource (dir </> "greeting2.txt") WriteMode
  liftIO $ repeatUntil (BS.hGetSome h1 1024) BS.null \chunk ->
    BS.hPutStr h2 chunk

binaryFileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
binaryFileResource path mode =
  allocate (IO.openBinaryFile path mode) IO.hClose
