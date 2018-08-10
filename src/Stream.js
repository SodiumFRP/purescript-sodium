
"use strict";

const Sodium = require("sodiumjs");

const Stream = Sodium.Stream;
const StreamSink = Sodium.StreamSink;
const Vertex = Sodium.Vertex;

// Stream Sink

exports.newStreamSinkImpl = function(m) {
    return new StreamSink(m);
}

exports.sendImpl = function(streamSink, a) {
    console.log("SENDING [" + a + "]");
    streamSink.send(a);
}

exports.toStreamImpl = function(streamSink) {
    return streamSink;
}

// Stream 

exports.newStreamImpl = function(v) {
    return new StreamSink(v);
}

// Stream
exports.mapStream = function (f) {
    return function(s) {
        return s.map(f);
    }
}


exports.listenImpl = function(stream, listener) {
    stream.listen(function(value) { console.log("GOT [" + value + "]")});
    var unlistener = stream.listen(listener);

    return function() {
        console.log("UNLISTENING");
        unlistener();
    }
}



