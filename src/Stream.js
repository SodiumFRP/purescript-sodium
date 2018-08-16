"use strict";



exports.mapToImpl = function(x, s) {
    return s.mapTo(x);
}

exports.orElseImpl = function(other, s) {
    return s.orElse(other);
}

exports.mergeImpl = function(f, other, s) {
    //flipped so that other's events are on the left
    return other.merge(s, f);
}

exports.filterImpl = function(f, s) {
    return s.filter(f); 
}

exports.gateImpl = function (c, s) {
    return s.gate(c);
}



