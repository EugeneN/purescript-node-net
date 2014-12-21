module Node.Net.Socket where

import Control.Monad.Eff
import Data.Function
import Data.Maybe

foreign import data SocketIO :: !
foreign import data Socket :: *

type Port = Number
type Host = String

type SocketEff eff = Eff (socketio :: SocketIO | eff)
type CbEff eff = Eff (|eff)

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
      s.on(ev,function(a){ cb(''+a)(); });
    };
  }""" :: forall eff eff2. Fn3 String (String -> CbEff eff2 Unit) Socket (SocketEff eff Unit)
onEvent :: forall eff eff2. String -> (String -> CbEff eff2 Unit) -> Socket -> SocketEff eff Unit
onEvent = runFn3 onEventImpl

onEvent0 e cb = onEvent e (\_ -> cb)


onConnect = onEvent0 "connect"
onData = onEvent "data"
onError = onEvent "error"
onEnd = onEvent0 "end"


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
    return p? just(p) : nothing;
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

foreign import createConnection
  """
  function createConnection(o) {
    var net = require('net');
    return function() { return net.createConnection(o); };
  }""" :: forall eff opts. {|opts} -> SocketEff eff Socket

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
