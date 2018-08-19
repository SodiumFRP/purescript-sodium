"use strict";
const Sodium = require("sodiumjs");

const Tuple2 = Sodium.Tuple2;

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

exports.snapshot1Impl = function (c, s) {
    return s.snapshot1(c);
}

exports.snapshotImpl = function (fn, c, s) {
    return s.snapshot(c, fn);
}

exports.snapshot3Impl = function (fn, c1, c2, s) {
    return s.snapshot3(c1, c2, fn);
}

exports.snapshot4Impl = function (fn, c1, c2, c3, s) {
    return s.snapshot4(c1, c2, c3, fn);
}

exports.snapshot5Impl = function (fn, c1, c2, c3, c4, s) {
    return s.snapshot5(c1, c2, c3, c4, fn);
}

exports.snapshot6Impl = function (fn, c1, c2, c3, c4, c5, s) {
    return s.snapshot6(c1, c2, c3, c4, c5, fn);
}

exports.holdImpl = function (x, s) {
    return s.hold(x);
}

exports.collectImpl = function (f, state, s) {
    return s.collect(state, function(x, state) {
        var record = f(x, state);
        return new Tuple2(record.value, record.state);
    });
}

exports.accumImpl = function (f, state, s) {
    return s.accum(state, f);
}

exports.onceImpl = function(s) {
    return s.once();
}

exports.loopStreamImpl = function(s, sLoop) {
    sLoop.loop(s);
}
