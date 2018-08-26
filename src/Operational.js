"use strict";

const Sodium = require("sodiumjs");
const Operational = Sodium.Operational;

exports.updatesImpl = function(c) {
    return Operational.updates(c);
}

exports.valueImpl = function(c) {
    return Operational.value(c);
}

exports.deferImpl = function(s) {
    return Operational.defer(s);
}

exports.splitImpl = function(sa) {
    return Operational.split(sa);
}


