"use strict";

const Sodium = require("sodiumjs");

exports.runTransaction = function(fn) {
    return Sodium.Transaction.run(fn);
}
