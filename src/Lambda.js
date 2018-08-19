"use strict";

const Sodium = require("sodiumjs");

const lambda1 = Sodium.lambda1;
const lambda2 = Sodium.lambda2;

//Stream

exports.mapLambda1StreamImpl = function (f, deps, s) {
    return s.map(lambda1(f, deps));
}

exports.snapshotLambdaImpl = function (f, deps, c, s) {
    return s.snapshot(c, lambda2(f, deps));
}

exports.snapshot3LambdaImpl = function (f, deps, c1, c2, s) {
    return s.snapshot3(c1, c2, lambda3(f, deps));
}

exports.snapshot4LambdaImpl = function (f, deps, c1, c2, c3, s) {
    return s.snapshot4(c1, c2, c3, lambda4(f, deps));
}

exports.snapshot5LambdaImpl = function (f, deps, c1, c2, c3, c4, s) {
    return s.snapshot5(c1, c2, c3, c4, lambda5(f, deps));
}

exports.snapshot6LambdaImpl = function (f, deps, c1, c2, c3, c4, c5, s) {
    return s.snapshot6(c1, c2, c3, c4, c5, lambda6(f, deps));
}
