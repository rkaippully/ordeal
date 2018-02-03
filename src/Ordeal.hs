{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Ordeal
    ( launchStackGhci
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.Chan  (newChan, readChan)
import qualified Control.Concurrent.MVar  as MVar
import           Control.Monad            (forever, unless, void)
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid              (mempty)
#endif
import           Data.Monoid              ((<>))
import qualified Data.Text                as Text
import qualified Data.Text.IO             as TextIO
import           System.Environment       (getArgs)
import           System.FSNotify          (Debounce (..), WatchConfig (..),
                                           defaultConfig, watchTreeChan,
                                           withManagerConf)
import           System.IO                (BufferMode (..), hSetBuffering,
                                           stdin, stdout)
import           System.Posix.Process     (getProcessGroupIDOf)
import           System.Posix.Signals     (Signal, sigINT, signalProcessGroup)
import           System.Posix.Types       (CPid (..))
import           System.Process           (CmdSpec (..), CreateProcess (..),
                                           ProcessHandle, StdStream (..),
                                           createProcess)
import           System.Process.Internals (ProcessHandle__ (..),
                                           withProcessHandle)

getPid :: ProcessHandle -> IO (Maybe CPid)
getPid ph = withProcessHandle ph go
  where
    go ph_ = case ph_ of
               OpenHandle x   -> return $ Just x
               ClosedHandle _ -> return Nothing

signalProcessHandle :: ProcessHandle -> Signal -> IO ()
signalProcessHandle pHandle signal = do
    -- TODO: FIX ME
    Just pid <- getPid pHandle
    pgId <- getProcessGroupIDOf pid
    signalProcessGroup signal pgId

processSpec :: String -> [String] -> CreateProcess
processSpec cmd args = CreateProcess {
    cmdspec         = RawCommand cmd args
  , cwd             = Nothing
  , env             = Nothing
  , std_in          = CreatePipe
  , std_out         = CreatePipe
  , std_err         = Inherit
  , close_fds       = False
  , create_group    = True
  , delegate_ctlc   = False
#if MIN_VERSION_process(1,3,0)
  , detach_console     = False
  , create_new_console = False
  , new_session        = False
#endif
#if MIN_VERSION_process(1,4,0)
  , child_group     = Nothing
  , child_user      = Nothing
#endif
#if MIN_VERSION_process(1,5,0)
  , use_process_jobs = False
#endif
  }

launchStackGhci :: IO ()
launchStackGhci = do
    let defaultreplCommand = ("stack", ["ghci"])
    args <- getArgs
    let (cmd, cmdArgs) =
            case args of
                [] ->
                    defaultreplCommand
                x:xs -> (x,xs)
    (Just stdinH, Just stdoutH, _err, processHandle) <- createProcess (processSpec cmd cmdArgs)

    command <- TextIO.readFile ".ordeal"

    hSetBuffering stdin     NoBuffering
    hSetBuffering stdout    NoBuffering
    hSetBuffering stdinH    NoBuffering
    hSetBuffering stdoutH   NoBuffering

    readVar <- MVar.newEmptyMVar
    let inPipe  = forever (TextIO.hGetChunk stdin   >>= TextIO.hPutStr stdinH)
    let outPipe = forever (do
                            t <- TextIO.hGetChunk stdoutH
                            MVar.putMVar readVar t
                            TextIO.hPutStr stdout t)

    let prompt = "ghci> "

    let reloadAction _ = do
            void (Async.async (signalProcessHandle processHandle sigINT))
            TextIO.hPutStrLn stdinH ":reload"
            TextIO.hPutStrLn stdinH command

    withManagerConf (defaultConfig {confDebounce = Debounce 0.5}) $ \mgr -> do
      chan <- newChan
      void $ watchTreeChan
        mgr
        "."
        (const True)
        chan

      void (Async.async (forever (readChan chan >>= reloadAction)))

      a <- Async.async (Async.concurrently inPipe outPipe)
      let looper acc = do
            t <- MVar.takeMVar readVar
            let newAcc = acc <> t
            unless (Text.isSuffixOf prompt newAcc) (looper newAcc)
      looper mempty
      void (Async.async (forever (MVar.takeMVar readVar)))
      TextIO.hPutStrLn stdinH command
      TextIO.hPutStrLn stdout command
      void (Async.wait a)
