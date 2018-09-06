"use strict";
const Sodium = require("sodiumjs");

const Tuple2 = Sodium.Tuple2;

exports.mapToImpl = function(x, s) {
    return s.mapTo(x);
}

exports.orElseImpl = function(s, other) {
    return s.orElse(other);
}

exports.mergeImpl = function(f, s, other) {
    return s.merge(other, f);
}

exports.filterImpl = function(f, s) {
    return s.filter(f); 
}

exports.gateImpl = function (s, c) {
    return s.gate(c);
}

exports.snapshot1Impl = function (s, c) {
    return s.snapshot1(c);
}

exports.snapshotImpl = function (fn, s, c) {
    return s.snapshot(c, fn);
}

exports.snapshot3Impl = function (fn, s, c1, c2) {
    return s.snapshot3(c1, c2, fn);
}

exports.snapshot4Impl = function (fn, s, c1, c2, c3) {
    return s.snapshot4(c1, c2, c3, fn);
}

exports.snapshot5Impl = function (fn, s, c1, c2, c3, c4) {
    return s.snapshot5(c1, c2, c3, c4, fn);
}

exports.snapshot6Impl = function (fn, s, c1, c2, c3, c4, c5) {
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

