
"use strict";

const Sodium = require("sodiumjs");

const Cell = Sodium.Cell;
const CellSink = Sodium.CellSink;
const CellLoop = Sodium.CellLoop;

// Cell
exports.newCellImpl = function(x, s) {
    return new Cell(x, s);
}



exports.sampleImpl = function(c) {
    return c.sample();
}

// Cell Sink
exports.newCellSinkImpl = function(x, mergeFn) {
    return new Cell(x, mergeFn);
}


exports.toCellImpl = function(cellSink) {
    return cellSink;
}
