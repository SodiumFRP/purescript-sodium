"use strict";

const Sodium = require("sodiumjs");

const lambda1 = Sodium.lambda1;

//Stream

exports.mapLambda1StreamImpl = function (f, d, s) {
    return s.map(lambda1(f, d));
}
