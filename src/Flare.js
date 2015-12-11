// module Flare

exports.foldp_ = function(f) {
  return function(seed) {
    var acc = seed;
    return function(x) {
      acc = f(x)(acc);
      return acc;
    };
  };
};

// vim: ts=2:sw=2
