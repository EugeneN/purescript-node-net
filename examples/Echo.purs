module Main where

import Debug.Trace
import Node.Net.Socket
import Control.Monad.Eff
import Data.Either
import Data.Maybe
import Control.Apply

import qualified Node.Yargs.Setup as Y
import qualified Node.Yargs.Applicative as Y

main = Y.runY setup $ echo
             <$> Y.yarg "h" ["host"] (Just "Host") (Left "localhost") false
             <*> Y.yarg "p" ["port"] (Just "Port") (Left 7777) false
  where setup = Y.usage "$0 -h <host> -p <port>"


echo :: forall eff. String -> Number -> Eff (socketio :: SocketIO | eff) Unit
echo host port = do
  sock <- createServer defaultServerOptions
  onListening (trace "listening") sock
  listenTCP host port sock
  onConnection handle sock
    where handle :: forall eff. Socket -> Eff (socketio :: SocketIO, trace :: Trace | eff) Unit
          handle conn = do
            trace $ "connection: " ++ show conn
            onError (trace <<< show) conn
            onEnd (trace "connection closed") conn
            onData (\d -> write d ignore conn *> return unit) conn
            return unit
          ignore = do
            return unit
