{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Buildsome.Db
  ( Db, with
  , registeredOutputsRef, leakedOutputsRef
  , InputDesc(..), FileDesc(..)
  , OutputDesc(..)
  , ExecutionLog(..), executionLog
  , FileContentDescCache(..), fileContentDescCache
  , Reason
  , IRef(..)
  ) where

import Control.Applicative ((<$>))
import Data.Binary (Binary(..))
import Data.ByteString (ByteString)
import Data.IORef
import Data.Map (Map)
import Data.Set (Set)
import Data.Time.Clock (DiffTime)
import GHC.Generics (Generic)
import Lib.Binary (encode, decode)
import Lib.BuildId (BuildId)
import Lib.Directory (catchDoesNotExist, createDirectories, makeAbsolutePath)
import Lib.FileDesc (FileContentDesc, FileModeDesc, FileStatDesc)
import Lib.FilePath (FilePath, (</>), (<.>))
import Lib.Makefile (TargetType(..), Target)
import Lib.StdOutputs (StdOutputs(..))
import Lib.TimeInstances ()
import Prelude hiding (FilePath)
import qualified Control.Exception as E
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as S
import qualified Database.Sophia as Sophia
import qualified System.Posix.ByteString as Posix

schemaVersion :: ByteString
schemaVersion = "schema.ver.6"

data Db = Db
  { dbSophia :: Sophia.Db
  , dbRegisteredOutputs :: IORef (Set FilePath)
  , dbLeakedOutputs :: IORef (Set FilePath)
  }

data FileContentDescCache = FileContentDescCache
  { fcdcModificationTime :: Posix.EpochTime
  , fcdcFileContentDesc :: FileContentDesc
  } deriving (Generic, Show)
instance Binary FileContentDescCache

type Reason = ByteString

data InputDesc = InputDesc
  { idModeAccess :: Maybe (Reason, FileModeDesc)
  , idStatAccess :: Maybe (Reason, FileStatDesc)
  , idContentAccess :: Maybe (Reason, FileContentDesc)
  } deriving (Generic)
instance Binary InputDesc

data FileDesc ne e
  = FileDescNonExisting ne
  | FileDescExisting e
  deriving (Generic)
instance (Binary ne, Binary e) => Binary (FileDesc ne e)

data OutputDesc = OutputDesc
  { odStatDesc :: FileStatDesc
  , odContentDesc :: Maybe FileContentDesc -- Nothing if directory
  } deriving (Generic)
instance Binary OutputDesc

data ExecutionLog = ExecutionLog
  { elBuildId :: BuildId
  , elInputsDescs :: Map FilePath (FileDesc Reason InputDesc)
  , elOutputsDescs :: Map FilePath (FileDesc () OutputDesc)
  , elStdoutputs :: StdOutputs ByteString
  , elSelfTime :: DiffTime
  } deriving (Generic)
instance Binary ExecutionLog

registeredOutputsRef :: Db -> IORef (Set FilePath)
registeredOutputsRef = dbRegisteredOutputs

leakedOutputsRef :: Db -> IORef (Set FilePath)
leakedOutputsRef = dbLeakedOutputs

setKey :: Binary a => Db -> ByteString -> a -> IO ()
setKey db key val = Sophia.setValue (dbSophia db) key $ encode val

getKey :: Binary a => Db -> ByteString -> IO (Maybe a)
getKey db key = fmap decode <$> Sophia.getValue (dbSophia db) key

deleteKey :: Db -> ByteString -> IO ()
deleteKey db = Sophia.delValue (dbSophia db)

with :: FilePath -> (Db -> IO a) -> IO a
with rawDbPath body = do
  dbPath <- makeAbsolutePath rawDbPath
  createDirectories dbPath
  Sophia.withEnv $ \env -> do
    Sophia.openDir env Sophia.ReadWrite Sophia.AllowCreation (BS8.unpack (dbPath </> schemaVersion))
    Sophia.withDb env $ \db ->
      withIORefFile (dbPath </> "outputs") $ \registeredOutputs ->
      withIORefFile (dbPath </> "leaked_outputs") $ \leakedOutputs ->
      body (Db db registeredOutputs leakedOutputs)
  where
    withIORefFile path =
      E.bracket (newIORef =<< decodeFileOrEmpty path)
                (E.uninterruptibleMask_ . writeBack path)
    writeBack path ref = do
      BS8.writeFile (BS8.unpack (path <.> "tmp")) .
        BS8.unlines . S.toList =<< readIORef ref
      Posix.rename (path <.> "tmp") path
    decodeFileOrEmpty path = (S.fromList . BS8.lines <$> BS8.readFile (BS8.unpack path)) `catchDoesNotExist` return S.empty

data IRef a = IRef
  { readIRef :: IO (Maybe a)
  , writeIRef :: a -> IO ()
  , delIRef :: IO ()
  }

mkIRefKey :: Binary a => ByteString -> Db -> IRef a
mkIRefKey key db = IRef
  { readIRef = getKey db key
  , writeIRef = setKey db key
  , delIRef = deleteKey db key
  }

executionLog :: Target -> Db -> IRef ExecutionLog
executionLog target = mkIRefKey targetKey
  where
    targetKey = MD5.hash $ targetCmds target -- TODO: Canonicalize commands (whitespace/etc)

fileContentDescCache :: FilePath -> Db -> IRef FileContentDescCache
fileContentDescCache = mkIRefKey
