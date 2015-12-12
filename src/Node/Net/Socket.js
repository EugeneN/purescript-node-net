// module Node.Net.Socket

exports.onEventImpl = function(ev, cb, s) {
  return function() {
    s.on(ev, function(a){ cb(''+a)(); });
  };
}

exports.onErrorImpl = function(cb, s) {
  return function() {
    s.on('error', function(err){ cb(err)(); });
  };
}

exports.onConnectionImpl = function(cb, s) {
  return function() {
    s.on('connection',function(o){ cb(o)(); });
  };
}

exports.localAddressImpl = function(just,nothing,s) {
  var a = s.localAddress;
  return a? just(a) : nothing;
}

exports.localPortImpl = function(just, nothing, s) {
  var p = s.localPort;
  return typeof(p) === 'number'? just(p) : nothing;
}

exports.remoteAddressImpl = function(just,nothing,s) {
  var a = s.remoteAddress;
  return a? just(a) : nothing;
}

exports.remotePortImpl = function(just, nothing, s) {
  var p = s.remotePort;
  return p? just(p) : nothing;
}

exports.createServer = function(o) {
  var net = require('net');
  return function() { return net.createServer(o); };
}

exports.createConnection = function(o) {
  return function() {
    var net = require('net');
    return net.createConnection(o);
  };
}

exports.writeImpl = function(d,cb,s) {
  return function() { return s.write(d,cb); };
}

exports.end = function(s) {
  return function() { s.end(); };
}

exports.destroy = function(s) {
  return function() { s.destroy(); };
}

exports.setNoDelayImpl = function(f,s) {
    return function() { s.setNoDelay(f); };
}

exports.bytesRead = function(s) {
  return function() { return s.bytesRead; };
}

exports.bytesWritten = function(s) {
  return function() { return s.bytesWritten; };
}

exports.listenTCPImpl = function(h,p,s) {
  return function() { s.listen(p,h); };
}

exports.listenUnixImpl = function(p,s) {
  return function() { s.listen(p); };
}
