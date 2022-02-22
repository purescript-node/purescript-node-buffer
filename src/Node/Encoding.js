/* global Buffer */
"use strict";

export function byteLengthImpl(str) {
  return function (enc) {
    return Buffer.byteLength(str, enc);
  };
}
