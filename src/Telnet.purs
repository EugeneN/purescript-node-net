module Main where

import Debug.Trace
import Node.ReadLine
import Node.Net.Socket
import Control.Monad.Eff
import Data.Either
import Data.Maybe
import Control.Apply

import qualified Node.Yargs.Setup as Y
import qualified Node.Yargs.Applicative as Y

foreign import data Process :: !
foreign import exit
  """
  function exit(c) {
    return function() { process.exit(c); };
  }""" :: forall eff. Number -> Eff (process :: Process | eff) Unit


main = Y.runY setup $ telnet
             <$> Y.yarg "h" ["host"] (Just "Host") (Right "The host is required") false
             <*> Y.yarg "p" ["port"] (Just "Port") (Left 23) false
  where setup = Y.usage "$0 -h <host> -p <port>"


telnet :: forall eff. String -> Number -> Eff (socketio :: SocketIO, console :: Console |eff) Unit
telnet host port = do
  sock <- createConnection defaultTCPOptions{host=host, port=port}
  setNoDelay true sock
  onError trace sock
  onEnd (trace "connection closed" *> exit 0) sock
  onData trace sock

  interface <- createInterface process.stdin process.stdout noCompletion
  setPrompt "" 0 interface
  setLineHandler (\l -> write (l++"\n") (return unit) sock) interface
  return unit
