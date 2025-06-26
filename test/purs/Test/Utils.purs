module Test.Utils (startColyseusServer, stopColyseusServer) where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Loops (untilM_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Buffer as Buffer
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.EventEmitter (on_)
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
  stdout ← CP.execSync command
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

    childProcess ← CP.spawn'
      command
      arguments
      (_ {detached = Just true})

    childProcess # on_ CP.exitH \exit → printInfo $
        "child process ("
          <> commandString
          <> ") exited: "
          <> show exit

    let 
      stdout = CP.stdout childProcess

    Stream.setEncoding stdout UTF8

    stdout # on_ Stream.dataHStr \s -> do
      printInfo s
      if (readyPredicate s) then Ref.write true ref else pure unit

    let 
      stderr = CP.stderr childProcess
    
    stderr # on_ Stream.dataH (Buffer.toString UTF8 >=> printError)

