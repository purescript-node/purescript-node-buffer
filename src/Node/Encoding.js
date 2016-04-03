/* global exports */
/* global Buffer */
"use strict";

// module Node.Encoding

exports.byteLengthImpl = function (str) {
  return function (enc) {
    return Buffer.byteLength(str, enc);
  };
};
