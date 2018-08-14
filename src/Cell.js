
"use strict";

const Sodium = require("sodiumjs");

const Cell = Sodium.Cell;
const CellSink = Sodium.CellSink;
const CellLoop = Sodium.CellLoop;

// Cell
exports.newCellImpl = function(x, s) {
    return new Cell(x, s);
}

exports.mapImpl = function (f, c) {
    return c.map(f);
}

exports.listenImpl = function(c, listener) {
    //c.listen(function(value) { console.log("GOT [" + value + "]")});
    var unlistener = c.listen(listener);

    return function() {
        //console.log("UNLISTENING");
        unlistener();
    }
}

// Cell Sink
exports.newCellSinkImpl = function(x, mergeFn) {
    return new Cell(x, mergeFn);
}


exports.toCellImpl = function(cellSink) {
    return cellSink;
}
