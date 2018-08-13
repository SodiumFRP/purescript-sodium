"use strict";

const Sodium = require("sodiumjs");

exports.runTransactionImpl = function(fn) {
    return Sodium.Transaction.run(fn);
}
