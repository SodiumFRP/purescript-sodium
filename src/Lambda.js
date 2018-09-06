"use strict";

const Sodium = require("sodiumjs");

const lambda1 = Sodium.lambda1;
const lambda2 = Sodium.lambda2;

//Stream

exports.mapLambda1StreamImpl = function (f, deps, s) {
    return s.map(lambda1(f, deps));
}

exports.snapshotLambdaImpl = function (f, deps, s, c) {
    return s.snapshot(c, lambda2(f, deps));
}

exports.snapshot3LambdaImpl = function (f, deps, s, c1, c2) {
    return s.snapshot3(c1, c2, lambda3(f, deps));
}

exports.snapshot4LambdaImpl = function (f, deps, s, c1, c2, c3) {
    return s.snapshot4(c1, c2, c3, lambda4(f, deps));
}

exports.snapshot5LambdaImpl = function (f, deps, s, c1, c2, c3, c4) {
    return s.snapshot5(c1, c2, c3, c4, lambda5(f, deps));
}

exports.snapshot6LambdaImpl = function (f, deps, s, c1, c2, c3, c4, c5) {
    return s.snapshot6(c1, c2, c3, c4, c5, lambda6(f, deps));
}

//Cell
//
exports.mapLambda1CellImpl = function (f, deps, c) {
    return c.map(lambda1(f, deps));
}

exports.liftLambdaImpl = function (fn, deps, c1, c) {
    return c.lift(c1, lambda2(fn, deps));
}

exports.lift3LambdaImpl = function (fn, deps, c1, c2, c) {
    return c.lift3(c1, c2, lambda3(fn, deps));
}
exports.lift4LambdaImpl = function (fn, deps, c1, c2, c3, c) {
    return c.lift4(c1, c2, c3, lambda4(fn, deps));
}
exports.lift5LambdaImpl = function (fn, deps, c1, c2, c3, c4, c) {
    return c.lift5(c1, c2, c3, c4, lambda5(fn, deps));
}
exports.lift6LambdaImpl = function (fn, deps, c1, c2, c3, c4, c5, c) {
    return c.lift6(c1, c2, c3, c4, c5, lambda6(fn, deps));
}
