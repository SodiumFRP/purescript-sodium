"use strict";

const Sodium = require("sodiumjs");

const lambda1 = Sodium.lambda1;

exports.sodiumLambda1Impl = function(f, d) {
    return lambda1(f, d);
};

exports.sodiumStreamMapImpl = function(l, s) {
    return s.map(l);
}

exports.sodiumCellMapImpl = function(l, c) {
    return c.map(l);
}
