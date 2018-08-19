"use strict";

const Sodium = require("sodiumjs");

const lambda1 = Sodium.lambda1;
const lambda2 = Sodium.lambda2;
const lambda3 = Sodium.lambda3;
const lambda4 = Sodium.lambda4;
const lambda5 = Sodium.lambda5;
const lambda6 = Sodium.lambda6;

exports.sodiumLambda1Impl = function(f, d) {
    return lambda1(f, d);
};

exports.sodiumLambda2Impl = function(f, d) {
    return lambda2(f, d);
};

exports.sodiumLambda3Impl = function(f, d) {
    return lambda3(f, d);
};

exports.sodiumLambda4Impl = function(f, d) {
    return lambda4(f, d);
};

exports.sodiumLambda5Impl = function(f, d) {
    return lambda5(f, d);
};

exports.sodiumLambda6Impl = function(f, d) {
    return lambda6(f, d);
};

exports.sodiumStreamMapImpl = function(l, s) {
    return s.map(l);
}

exports.sodiumCellMapImpl = function(l, c) {
    return c.map(l);
}
