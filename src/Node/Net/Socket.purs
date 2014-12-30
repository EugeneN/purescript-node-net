module Node.Net.Socket where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Data.Function
import Data.Maybe

foreign import data SocketIO :: !
foreign import data Socket :: *

type Port = Number
type Host = String

type SocketEff eff = Eff (socketio :: SocketIO | eff)
type CbEff eff = Eff (|eff)


type ConnectionOptions opts = {allowHalfOpen :: Boolean | opts}
type TCPOptions = ConnectionOptions (port :: Port, host :: Host, localAddress :: Host)
type UNIXOptions = ConnectionOptions (path :: String)
type ServerOptions = ConnectionOptions ()



defaultTCPOptions :: TCPOptions
defaultTCPOptions = {port: 0, host: "", localAddress: "", allowHalfOpen: false}

defaultUNIXOptions :: UNIXOptions
defaultUNIXOptions = {path: "", allowHalfOpen: false}

defaultServerOptions :: ServerOptions
defaultServerOptions = {allowHalfOpen: false}


instance showSocket :: Show Socket where
  show s | localPort s == Nothing = "(n/a)"
  show s = sla ++ ":" ++ slp ++ "<>" ++ sra ++ ":" ++ srp
    where showAddress (Just a) = a
          showAddress Nothing = "(n/a)"
          showPort (Just a) = show a
          showPort Nothing = "(n/a)"
          sla = showAddress $ localAddress s
          slp = showPort $ localPort s
          sra = showAddress $ remoteAddress s
          srp = showPort $ remotePort s

foreign import onEventImpl
  """
  function onEventImpl(ev, cb, s) {
    return function() {
      s.on(ev, function(a){ cb(''+a)(); });
    };
  }""" :: forall eff eff2. Fn3 String (String -> CbEff eff2 Unit) Socket (SocketEff eff Unit)
onEvent :: forall eff eff2. String -> (String -> CbEff eff2 Unit) -> Socket -> SocketEff eff Unit
onEvent = runFn3 onEventImpl

onEvent0 e cb = onEvent e (\_ -> cb)

onConnect = onEvent0 "connect"
onData = onEvent "data"
onEnd = onEvent0 "end"


foreign import onErrorImpl
  """
  function onErrorImpl(cb, s) {
    return function() {
      s.on('error', function(err){ cb(err)(); });
    };
  }""" :: forall eff eff2. Fn2 (Error -> CbEff eff2 Unit) Socket (SocketEff eff Unit)
onError :: forall eff eff2. (Error -> CbEff eff2 Unit) -> Socket -> SocketEff eff Unit
onError = runFn2 onErrorImpl


foreign import onConnectionImpl
  """
  function onConnectionImpl(cb, s) {
    return function() {
      s.on('connection',function(o){ cb(o)(); });
    };
  }""" :: forall eff eff2. Fn2 (Socket -> CbEff eff2 Unit) Socket (SocketEff eff Unit)
onConnection :: forall eff eff2. (Socket -> CbEff eff2 Unit) -> Socket -> SocketEff eff Unit
onConnection = runFn2 onConnectionImpl

onListening = onEvent0 "listening"
onClose = onEvent0 "connect"
onTimeout = onEvent0 "timeout"
onDrain = onEvent0 "drain"


foreign import localAddressImpl
  """
  function localAddressImpl(just,nothing,s) {
    var a = s.localAddress;
    return a? just(a) : nothing;
  }""" :: forall a. Fn3 (a -> Maybe a) (Maybe a) Socket (Maybe Host)
localAddress :: Socket -> Maybe Host
localAddress = runFn3 localAddressImpl Just Nothing

foreign import localPortImpl
  """
  function localPortImpl(just, nothing, s) {
    var p = s.localPort;
    return typeof(p) === 'number'? just(p) : nothing;
  }""" :: forall a. Fn3 (a -> Maybe a) (Maybe a) Socket (Maybe Port)
localPort :: Socket -> Maybe Port
localPort = runFn3 localPortImpl Just Nothing

foreign import remoteAddressImpl
  """
  function remoteAddressImpl(just,nothing,s) {
    var a = s.remoteAddress;
    return a? just(a) : nothing;
  }""" :: forall a. Fn3 (a -> Maybe a) (Maybe a) Socket (Maybe Host)
remoteAddress :: Socket -> Maybe Host
remoteAddress = runFn3 remoteAddressImpl Just Nothing

foreign import remotePortImpl
  """
  function remotePortImpl(just, nothing, s) {
    var p = s.remotePort;
    return p? just(p) : nothing;
  }""" :: forall a. Fn3 (a -> Maybe a) (Maybe a) Socket (Maybe Port)
remotePort :: Socket -> Maybe Port
remotePort = runFn3 remotePortImpl Just Nothing

foreign import createServer
  """
  function createServer(o) {
    var net = require('net');
    return function() { return net.createServer(o); };
  }""" :: forall eff opts. ServerOptions -> SocketEff eff Socket

foreign import createConnection
  """
  function createConnection(o) {
    var net = require('net');
    return function() { return net.createConnection(o); };
  }""" :: forall eff opts. ConnectionOptions opts -> SocketEff eff Socket

foreign import writeImpl
  """
  function writeImpl(d,cb,s) {
    return function() { return s.write(d,cb); };
  }""" :: forall eff eff2. Fn3 String (CbEff eff2 Unit) Socket (SocketEff eff Boolean)
write :: forall eff eff2. String -> (CbEff eff2 Unit) -> Socket -> SocketEff eff Boolean
write = runFn3 writeImpl

foreign import end
  """
  function end(s) {
    return function() { s.end(); };
  }""" :: forall eff. Socket -> SocketEff eff Unit

foreign import destroy
  """
  function destroy(s) {
    return function() { s.destroy(); };
  }""" :: forall eff. Socket -> SocketEff eff Unit

foreign import setNoDelayImpl
  """
  function setNoDelayImpl(f,s) {
      return function() { s.setNoDelay(f); };
  }""" :: forall eff. Fn2 Boolean Socket (SocketEff eff Unit)
setNoDelay :: forall eff. Boolean -> Socket -> SocketEff eff Unit
setNoDelay = runFn2 setNoDelayImpl


foreign import bytesRead
  """
  function bytesRead(s) {
    return function() { return s.bytesRead; };
  }""" :: forall eff. Socket -> SocketEff eff Number

foreign import bytesWritten
  """
  function bytesWritten(s) {
    return function() { return s.bytesWritten; };
  }""" :: forall eff. Socket -> SocketEff eff Number


foreign import listenTCPImpl
  """
  function listenTCPImpl(h,p,s) {
    return function() { s.listen(p,h); };
  }""" :: forall eff. Fn3 Host Port Socket (SocketEff eff Unit)
listenTCP :: forall eff. Host -> Port -> Socket -> SocketEff eff Unit
listenTCP = runFn3 listenTCPImpl

foreign import listenUnixImpl
  """
  function listenUnixImpl(p,s) {
    return function() { s.listen(p); };
  }""" :: forall eff. Fn2 String Socket (SocketEff eff Unit)
listenUnix :: forall eff. String -> Socket -> SocketEff eff Unit
listenUnix = runFn2 listenUnixImpl
