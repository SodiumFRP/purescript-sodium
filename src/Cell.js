
"use strict";

exports.sampleImpl = function(c) {
    return c.sample();
}

exports.loopCellImpl = function(c, cLoop) {
    cLoop.loop(c);
}
