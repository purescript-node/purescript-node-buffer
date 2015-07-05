/* global exports */
/* global Buffer */
"use strict";

// module Node.Encoding

exports.byteLength = function (str) { 
  return function (enc) { 
    return Buffer.byteLength(str, enc); 
  };
};
