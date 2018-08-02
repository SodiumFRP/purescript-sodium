
"use strict";

const Sodium = require("sodiumjs");

const Stream = Sodium.Stream;
const StreamSink = Sodium.StreamSink;

exports.mapStream = function (f) {
    return function(s) {
        return s.map(f);
    }
}

exports.newStreamSinkImpl = function(m) {
    return new StreamSink(m);
}

exports.sendImpl = function(stream, a) {
    console.log("SENDING [" + a + "]");
    stream.send(a);
}

exports.listenImpl = function(stream, listener) {
    stream.listen(function(value) { console.log("GOT [" + value + "]")});
    var unlistener = stream.listen(listener);

    return function() {
        console.log("UNLISTENING");
        unlistener();
    }
}
