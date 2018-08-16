"use strict";

const Sodium = require("sodiumjs");

const Stream = Sodium.Stream;
const StreamSink = Sodium.StreamSink;
const StreamLoop = Sodium.StreamLoop;
const Vertex = Sodium.Vertex;

// Stream Sink

exports.newStreamSinkImpl = function(mergeFn) {
    return new StreamSink(mergeFn);
}

exports.sendImpl = function(a, streamSink) {
    //console.log("SENDING [" + a + "]");
    streamSink.send(a);
}

exports.toStreamImpl = function(streamSink) {
    return streamSink;
}

// Stream 

exports.newStreamImpl = function(v) {
    return new StreamSink(v);
}


exports.mapToImpl = function(x, s) {
    return s.mapTo(x);
}

exports.orElseImpl = function(other, s) {
    return s.orElse(other);
}

exports.mergeImpl = function(f, other, s) {
    //flipped so that other's events are on the left
    return other.merge(s, f);
}

exports.filterImpl = function(f, s) {
    return s.filter(f); 
}

exports.gateImpl = function (c, s) {
    return s.gate(c);
}



