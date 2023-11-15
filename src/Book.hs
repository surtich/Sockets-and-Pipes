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
import qualified Data.Text.Encoding           as T
import qualified Data.Text.IO                 as T

import           Network.Socket               (Socket)
import qualified Network.Socket               as S
import qualified Network.Socket.ByteString    as S
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


helloByteString :: IO ()
helloByteString = do
  IO.hSetBinaryMode stdout True
  BS.hPut stdout (BS.pack helloBytes)

helloBytes :: [Word8]
helloBytes = [
  104, 101, 108, 108, 111, -- hello
  32, -- space
  119, 111, 114, 108, 100, 33, -- world!
  10 ]


greet :: BS.ByteString -> IO ()
greet nameBS = case T.decodeUtf8' nameBS of
  Left _         -> putStrLn "Invalid byte string"
  Right nameText -> T.putStrLn (T.pack "Hello, " <> nameText)

asciiUpper :: BS.ByteString -> BS.ByteString
asciiUpper = BS.map asciiUpperByte

asciiUpperByte :: Word8 -> Word8
asciiUpperByte w
  | w >= 97 && w <= 122 = w - 32
  | otherwise           = w

makeFriend :: S.SockAddr -> IO ()
makeFriend address = do
  s <- S.socket S.AF_INET S.Stream S.defaultProtocol -- 1
  S.setSocketOption s S.UserTimeout 1000
  S.connect s address -- 2
  S.sendAll s $ T.encodeUtf8 $ -- 3
    T.pack "Hello, will you be my friend?"
  repeatUntil (S.recv s 1024) BS.null BS.putStr  -- 4

makeFriendSafely :: S.SockAddr -> IO ()
makeFriendSafely address = runResourceT @IO do
  (_, s) <- allocate
    ( S.socket S.AF_INET S.Stream S.defaultProtocol)
    S.close
  liftIO do
    S.setSocketOption s S.UserTimeout 10000
    S.connect s address -- 2
    S.sendAll s $ T.encodeUtf8 $ -- 3
      T.pack "Hello, will you be my friend?"
    repeatUntil (S.recv s 1024) BS.null BS.putStr
    S.gracefulClose s 1000

makeFriendAddrInfo :: S.AddrInfo -> IO ()
makeFriendAddrInfo addressInfo = runResourceT @IO do
  (_, s) <- openAndConnect addressInfo
  liftIO do
    S.sendAll s $ T.encodeUtf8 $
      T.pack "Hello, will you be my friend?"
    repeatUntil (S.recv s 1024) BS.null BS.putStr
    S.gracefulClose s 1000

findHaskellWebsite :: IO S.AddrInfo
findHaskellWebsite = resolve "80" "www.haskell.org"


openAndConnect :: S.AddrInfo -> ResourceT IO (ReleaseKey, Socket)
openAndConnect addressInfo = allocate setup S.close
  where
    setup = do
      s <- S.openSocket addressInfo
      S.setSocketOption s S.UserTimeout 1000
      S.connect s (S.addrAddress addressInfo)
      return s

resolve :: S.ServiceName -> S.HostName -> IO S.AddrInfo
resolve port host = do
  addrInfos <- S.getAddrInfo
    (Just S.defaultHints { S.addrSocketType = S.Stream })
    (Just host)
    (Just port)
  case addrInfos of
    []    -> fail "getAddrInfo returned []"
    x : _ -> return x

