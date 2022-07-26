module Test.Utils (startColyseusServer, stopColyseusServer) where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Loops (untilM_)
import Data.String (Pattern(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Buffer as Buffer
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding(..))
import Node.Stream as Stream

startColyseusServer ∷ Aff Unit
startColyseusServer = do
  void $ try stopColyseusServer
  execInTheBackground
    "colysus-server"
    "npm"
    [ "run", "server:start" ]
    (String.contains $ Pattern "Listening on ws://localhost:2567")

stopColyseusServer ∷ Aff Unit
stopColyseusServer =
  exec "npm run server:stop"

exec ∷ String → Aff Unit
exec command = liftEffect do
  Console.info command
  stdout ← ChildProcess.execSync
    command
    ChildProcess.defaultExecSyncOptions
  Console.info =<< Buffer.toString UTF8 stdout

execInTheBackground ∷ String → String → Array String → (String -> Boolean) -> Aff Unit
execInTheBackground name command arguments readyPredicate = do
  isReadyRef <- liftEffect $ Ref.new false
  liftEffect $ launchAff_ $ runChildProcess isReadyRef
  -- requires negating the condition because of a bug in the library 
  (liftEffect $ not <$> Ref.read isReadyRef) # untilM_ do
    Console.info $ "Waiting for process '" <> name <> "' to start..."
    delay $ Milliseconds 500.0
  where
  runChildProcess ref = liftEffect do
    let
      commandString = command <> " " <> show arguments
      printInfo msg = Console.info $ "[" <> name <> "] " <> msg
      printError msg = Console.error $ "[" <> name <> "] " <> msg

    Console.info
      $ "\n--- "
          <> name
          <> " ---\n"
          <> commandString
          <> "\n---\n"

    childProcess ← ChildProcess.spawn
      command
      arguments
      (ChildProcess.defaultSpawnOptions { detached = true })

    ChildProcess.onExit childProcess \exit →
      printInfo $
        "child process ("
          <> commandString
          <> ") exited: "
          <> show exit

    Stream.onDataString (ChildProcess.stdout childProcess) UTF8 \s -> do
      printInfo s
      if (readyPredicate s) then Ref.write true ref else pure unit

    Stream.onData
      (ChildProcess.stderr childProcess)
      (Buffer.toString UTF8 >=> printError)

