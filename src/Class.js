const Sodium = require("sodiumjs");

const Cell = Sodium.Cell;
const CellSink = Sodium.CellSink;
const CellLoop = Sodium.CellLoop;

const Stream = Sodium.Stream;
const StreamSink = Sodium.StreamSink;
const StreamLoop = Sodium.StreamLoop;

//Stream


exports.emptyImpl = function() {
    return new Stream();
}

exports.newStreamImpl = function() {
    return new StreamSink();
}

exports.newStreamLoopImpl = function() {
    return new StreamLoop();
}

exports.newStreamSinkImpl = function(mergeFn) {
    return new StreamSink(mergeFn);
}

exports.mapStreamImpl = function (f, s) {
    return s.map(f);
}


exports.listenStreamImpl = function(stream, listener) {
    //stream.listen(function(value) { console.log("GOT [" + value + "]")});
    var unlistener = stream.listen(listener);

    return function() {
        //console.log("UNLISTENING");
        unlistener();
    }
}


exports.sendStreamImpl = function(a, streamSink) {
    //console.log("SENDING [" + a + "]");
    streamSink.send(a);
}

exports.concatStreamImpl = function(other, s) {
   return s.merge(other, function(left, right) {
       //TODO: should test if left is a semigroup and concat that...?
       return left;
   });
}

//Cell

exports.newCellImpl = function(x, s) {
    return new Cell(x, s);
}

exports.newCellLoopImpl = function() {
    return new CellLoop();
}

exports.newCellSinkImpl = function(x, mergeFn) {
    return new CellSink(x, mergeFn);
}

exports.mapCellImpl = function (f, c) {
    return c.map(f);
}

exports.listenCellImpl = function(c, listener) {
    //c.listen(function(value) { console.log("GOT [" + value + "]")});
    var unlistener = c.listen(listener);

    return function() {
        //console.log("UNLISTENING");
        unlistener();
    }
}


exports.sendCellImpl = function(a, cellSink) {
    //console.log("SENDING [" + a + "]");
    cellSink.send(a);
}

