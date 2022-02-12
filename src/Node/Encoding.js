/* global Buffer */
"use strict";

export function byteLengthImpl(str) {
  return enc => {
    return Buffer.byteLength(str, enc);
  };
}
