const Sodium = require("sodiumjs");

const Cell = Sodium.Cell;
const CellSink = Sodium.CellSink;
const CellLoop = Sodium.CellLoop;

const Stream = Sodium.Stream;
const StreamSink = Sodium.StreamSink;
const StreamLoop = Sodium.StreamLoop;

exports.loopCellImpl_ = function(c, cLoop) {
    cLoop.loop(c);
}

exports.loopStreamImpl_ = function(s, sLoop) {
    sLoop.loop(s);
}

exports.runTransactionImpl_ = function(fn) {
    return Sodium.Transaction.run(fn);
}

// Constructors

exports.newStreamImpl = function() {
    return new StreamSink();
}

exports.newCellImpl = function(x, s) {
    return new Cell(x, s);
}

exports.newStreamLoopImpl = function() {
    return new StreamLoop();
}

exports.newCellLoopImpl = function() {
    return new CellLoop();
}

exports.newStreamSinkImpl = function(mergeFn) {
    return new StreamSink(mergeFn);
}

exports.newCellSinkImpl = function(x, mergeFn) {
    return new CellSink(x, mergeFn);
}

// Listen
exports.listenStreamImpl = function(stream, listener) {
    //stream.listen(function(value) { console.log("GOT [" + value + "]")});
    var unlistener = stream.listen(listener);

    return function() {
        //console.log("UNLISTENING");
        unlistener();
    }
}

exports.listenCellImpl = function(c, listener) {
    //c.listen(function(value) { console.log("GOT [" + value + "]")});
    var unlistener = c.listen(listener);

    return function() {
        //console.log("UNLISTENING");
        unlistener();
    }
}

// Send
exports.sendStreamImpl = function(a, streamSink) {
    //console.log("SENDING [" + a + "]");
    streamSink.send(a);
}

exports.sendCellImpl = function(a, cellSink) {
    //console.log("SENDING [" + a + "]");
    cellSink.send(a);
}

//Categories
//To satisfy category laws, re-use fantasy-land implementations
//Additional benefit is potential for runtime checking via Z

//Stream
exports.mapStreamImpl = function (f, s) {
    return s['fantasy-land/map'](f);
}

exports.concatStreamImpl = function(other, s) {
    return s['fantasy-land/concat'] (other);
}


//Cell
exports.mapCellImpl = function (f, c) {
    return c['fantasy-land/map'](f);
}

exports.applyImpl = function(cf, c) {
    return c['fantasy-land/ap'](cf);
}

exports.bindImpl = function(c, f) {
    return c['fantasy-land/chain'](f);
}
