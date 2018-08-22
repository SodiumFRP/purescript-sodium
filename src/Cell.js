"use strict";

const Sodium = require("sodiumjs");

exports.sampleImpl = function(c) {
    return c.sample();
}

exports.loopCellImpl = function(c, cLoop) {
    cLoop.loop(c);
}

exports.liftImpl = function (fn, c1, c) {
    return c.lift(c1, fn);
}

exports.lift3Impl = function (fn, c1, c2, c) {
    return c.lift3(c1, c2, fn);
}
exports.lift4Impl = function (fn, c1, c2, c3, c) {
    return c.lift4(c1, c2, c3, fn);
}
exports.lift5Impl = function (fn, c1, c2, c3, c4, c) {
    return c.lift5(c1, c2, c3, c4, fn);
}
exports.lift6Impl = function (fn, c1, c2, c3, c4, c5, c) {
    return c.lift6(c1, c2, c3, c4, c5, fn);
}

exports.switchCImpl = function(c) {
    return Sodium.Cell.switchC(c);
}

exports.switchSImpl = function(c) {
    return Sodium.Cell.switchS(c);
}

