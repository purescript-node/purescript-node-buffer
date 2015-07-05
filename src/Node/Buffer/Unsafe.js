/* global exports */
/* global Buffer */
"use strict";

// module Node.Buffer.Unsafe

exports.slice = function (start) {
  return function (end) {
    return function (buff) {
      return buff.slice(start, end);
    };
  };
};
