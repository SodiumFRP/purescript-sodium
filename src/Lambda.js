"use strict";

const Sodium = require("sodiumjs");


const Cell = Sodium.Cell;
const CellSink = Sodium.CellSink;
const CellLoop = Sodium.CellLoop;

const Stream = Sodium.Stream;
const StreamSink = Sodium.StreamSink;
const StreamLoop = Sodium.StreamLoop;

//Stream

exports.mapLambda1StreamImpl = function (f, d, s) {
    return s.map(Sodium.lambda1(f, d));
}
