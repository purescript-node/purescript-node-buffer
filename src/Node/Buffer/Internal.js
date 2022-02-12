/* global Buffer */
"use strict";

export function copyAll(a) {
  return function () {
    return Buffer.from(a);
  };
}

export function writeInternal(ty) {
  return function (value) {
    return function (offset) {
      return function (buf) {
        return function () {
          buf["write" + ty](value, offset);
        };
      };
    };
  };
}

export function writeStringInternal(encoding) {
  return function (offset) {
    return function (length) {
      return function (value) {
        return function (buff) {
          return function () {
            return buff.write(value, offset, length, encoding);
          };
        };
      };
    };
  };
}

export function setAtOffset(value) {
  return function (offset) {
    return function (buff) {
      return function () {
        buff[offset] = value;
      };
    };
  };
}

export function copy(srcStart) {
  return function (srcEnd) {
    return function (src) {
      return function (targStart) {
        return function (targ) {
          return function () {
            return src.copy(targ, targStart, srcStart, srcEnd);
          };
        };
      };
    };
  };
}

export function fill(octet) {
  return function (start) {
    return function (end) {
      return function (buf) {
        return function () {
          buf.fill(octet, start, end);
        };
      };
    };
  };
}
