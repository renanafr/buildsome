{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Lib.FSHook
  ( FSHook
  , with
  , InputHandler, OutputHandler, Handlers(..)
  , AccessDoc
  , runCommand
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, myThreadId, killThread)
import Control.Concurrent.MVar
import Control.Exception.Async (handleSync)
import Control.Monad
import Data.ByteString (ByteString)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Lib.AccessType (AccessType(..))
import Lib.Argv0 (getArgv0)
import Lib.ByteString (unprefixed)
import Lib.FilePath (FilePath, takeDirectory, (</>))
import Lib.Fresh (Fresh)
import Lib.IORef (atomicModifyIORef_)
import Lib.Sock (recvLoop_, withUnixSeqPacketListener)
import Network.Socket (Socket)
import Paths_buildsome (getDataFileName)
import Prelude hiding (FilePath)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as M
import qualified Lib.AsyncContext as AsyncContext
import qualified Lib.FSHook.Protocol as Protocol
import qualified Lib.Fresh as Fresh
import qualified Lib.Process as Process
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS
import qualified System.Posix.ByteString as Posix

type AccessDoc = ByteString

type JobId = ByteString

type InputHandler = AccessType -> AccessDoc -> FilePath -> IO ()
type OutputHandler = AccessDoc -> FilePath -> IO ()

data Handlers = Handlers
  { handleInput :: InputHandler
  , handleDelayedInput :: InputHandler
  , handleOutput :: OutputHandler
  }

data RunningJob = RunningJob
  { jobLabel :: ByteString
  , jobActiveConnections :: IORef (Map Int (ThreadId, MVar ()))
  , jobFreshConnIds :: Fresh Int
  , jobThreadId :: ThreadId
  , jobHandlers :: Handlers
  , jobRootFilter :: FilePath
  }

data FSHook = FSHook
  { fsHookRunningJobs :: IORef (Map JobId RunningJob)
  , fsHookFreshJobIds :: Fresh Int
  , fsHookLdPreloadPath :: FilePath
  , fsHookServerAddress :: FilePath
  }

data ProtocolError = ProtocolError String deriving (Show, Typeable)
instance E.Exception ProtocolError

serve :: FSHook -> Socket -> IO ()
serve fsHook conn = do
  helloLine <- SockBS.recv conn 1024
  case unprefixed (BS8.pack "HELLO, I AM: ") helloLine of
    Nothing -> E.throwIO $ ProtocolError $ "Bad hello message from connection: " ++ show helloLine
    Just pidJobId -> do
      runningJobs <- readIORef (fsHookRunningJobs fsHook)
      case M.lookup jobId runningJobs of
        Nothing -> do
          let jobIds = M.keys runningJobs
          E.throwIO $ ProtocolError $ "Bad slave id: " ++ show jobId ++ " mismatches all: " ++ show jobIds
        Just job -> handleJobConnection fullTidStr conn job
      where
        fullTidStr = BS8.unpack pidStr ++ ":" ++ BS8.unpack tidStr
        [pidStr, tidStr, jobId] = BS8.split ':' pidJobId

maxMsgSize :: Int
maxMsgSize = 8192

with :: (FSHook -> IO a) -> IO a
with body = do
  ldPreloadPath <- getLdPreloadPath
  putStrLn $ "Using fs_override.so from " ++ show ldPreloadPath
  pid <- Posix.getProcessID
  freshJobIds <- Fresh.new 0
  let serverFilename = "/tmp/fshook-" <> BS8.pack (show pid)
  withUnixSeqPacketListener serverFilename $ \listener -> do
    runningJobsRef <- newIORef M.empty
    let
      fsHook = FSHook
        { fsHookRunningJobs = runningJobsRef
        , fsHookFreshJobIds = freshJobIds
        , fsHookLdPreloadPath = ldPreloadPath
        , fsHookServerAddress = serverFilename
        }
    AsyncContext.new $ \ctx -> do
      _ <- AsyncContext.spawn ctx $ forever $ do
        (conn, _srcAddr) <- Sock.accept listener
        AsyncContext.spawn ctx $ serve fsHook conn
      body fsHook

{-# INLINE sendGo #-}
sendGo :: Socket -> IO ()
sendGo conn = void $ SockBS.send conn (BS8.pack "GO")

{-# INLINE handleJobMsg #-}
handleJobMsg :: String -> Socket -> RunningJob -> Protocol.Func -> IO ()
handleJobMsg _tidStr conn job msg =
  case msg of
    -- outputs
    Protocol.Open path Protocol.OpenWriteMode _ -> reportOutput path
    Protocol.Open path _ (Protocol.Create _) -> reportOutput path
    Protocol.Creat path _ -> reportOutput path
    Protocol.Rename a b -> reportOutput a >> reportOutput b
    Protocol.Unlink path -> reportOutput path
    Protocol.Truncate path _ -> reportOutput path
    Protocol.Chmod path _ -> reportOutput path
    Protocol.Chown path _ _ -> reportOutput path
    Protocol.MkNod path _ _ -> reportOutput path -- TODO: Special mkNod handling?
    Protocol.MkDir path _ -> reportOutput path
    Protocol.RmDir path -> reportOutput path

    -- I/O
    Protocol.SymLink target linkPath -> reportOutput linkPath >> reportInput AccessTypeFull target
    Protocol.Link src dest -> forwardExceptions $
      error $ unwords ["Hard links not supported:", show src, "->", show dest]
      -- TODO: Record the fact it's a link
      --reportOutput dest >> reportInput src

    -- inputs
    Protocol.Open path Protocol.OpenReadMode _creationMode -> reportInput AccessTypeFull path
    Protocol.Access path _mode -> reportInput AccessTypeModeOnly path
    Protocol.Stat path -> reportInput AccessTypeFull path
    Protocol.LStat path -> reportInput AccessTypeFull path
    Protocol.OpenDir path -> reportInput AccessTypeFull path
    Protocol.ReadLink path -> reportInput AccessTypeModeOnly path
  where
    handlers = jobHandlers job
    actDesc = BS8.pack (Protocol.showFunc msg) <> " done by " <> jobLabel job
    forwardExceptions =
      handleSync $ \e@E.SomeException {} -> E.throwTo (jobThreadId job) e
    reportInput accessType path
      | "/" `BS8.isPrefixOf` path =
        forwardExceptions $ handleInput handlers accessType actDesc path
      | otherwise = do
        forwardExceptions (handleDelayedInput handlers accessType actDesc path)
        sendGo conn
    reportOutput path =
      forwardExceptions $ handleOutput handlers actDesc path

withRegistered :: Ord k => IORef (Map k a) -> k -> a -> IO r -> IO r
withRegistered registry key val =
  E.bracket_ register unregister
  where
    register = atomicModifyIORef_ registry $ M.insert key val
    unregister = atomicModifyIORef_ registry $ M.delete key

handleJobConnection :: String -> Socket -> RunningJob -> IO ()
handleJobConnection tidStr conn job = do
  -- This lets us know for sure that by the time the slave dies,
  -- we've seen its connection
  connId <- Fresh.next $ jobFreshConnIds job
  tid <- myThreadId

  connFinishedMVar <- newEmptyMVar
  (`E.finally` putMVar connFinishedMVar ()) $
    withRegistered (jobActiveConnections job) connId (tid, connFinishedMVar) $ do
      sendGo conn
      recvLoop_ maxMsgSize
        (handleJobMsg tidStr conn job . Protocol.parseMsg) conn

mkEnvVars :: FSHook -> FilePath -> JobId -> Process.Env
mkEnvVars fsHook rootFilter jobId =
  (map . fmap) BS8.unpack $
  [ ("LD_PRELOAD", fsHookLdPreloadPath fsHook)
  , ("BUILDSOME_MASTER_UNIX_SOCKADDR", fsHookServerAddress fsHook)
  , ("BUILDSOME_JOB_ID", jobId)
  , ("BUILDSOME_ROOT_FILTER", rootFilter)
  ]

runCommand :: FSHook -> FilePath -> (Process.Env -> IO r) -> ByteString -> Handlers -> IO r
runCommand fsHook rootFilter cmd label handlers = do
  activeConnections <- newIORef M.empty
  freshConnIds <- Fresh.new 0
  jobIdNum <- Fresh.next $ fsHookFreshJobIds fsHook
  tid <- myThreadId

  let jobId = BS8.pack ("cmd" ++ show jobIdNum)
      job = RunningJob
            { jobLabel = label
            , jobActiveConnections = activeConnections
            , jobFreshConnIds = freshConnIds
            , jobThreadId = tid
            , jobRootFilter = rootFilter
            , jobHandlers = handlers
            }
  -- Don't leak connections still running our handlers once we leave!
  let onActiveConnections f = mapM_ f . M.elems =<< readIORef activeConnections
  (`E.finally` onActiveConnections awaitConnection) $
    (`E.onException` onActiveConnections killConnection) $
    withRegistered (fsHookRunningJobs fsHook) jobId job $
    cmd (mkEnvVars fsHook rootFilter jobId)
  where
    killConnection (tid, _mvar) = killThread tid
    awaitConnection (_tid, mvar) = readMVar mvar

data CannotFindOverrideSharedObject = CannotFindOverrideSharedObject deriving (Show, Typeable)
instance E.Exception CannotFindOverrideSharedObject

getLdPreloadPath :: IO FilePath
getLdPreloadPath = do
  installedFilePath <- BS8.pack <$> (getDataFileName . BS8.unpack) fileName
  installedExists <- Posix.fileExist installedFilePath
  if installedExists
    then return installedFilePath
    else do
      argv0 <- getArgv0
      let nearExecPath = takeDirectory argv0 </> fileName
      nearExecExists <- Posix.fileExist nearExecPath
      if nearExecExists
        then return nearExecPath
        else E.throwIO CannotFindOverrideSharedObject
  where
    fileName = "fs_override.so"
