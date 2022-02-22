/* global Buffer */
"use strict";

export const showImpl = require("util").inspect;

export function eqImpl(a) {
  return function (b) {
    return a.equals(b);
  };
}

export function compareImpl(a) {
  return function (b) {
    return a.compare(b);
  };
}

export function create(size) {
  return Buffer.alloc(size);
}

export function fromArray(octets) {
  return Buffer.from(octets);
}

export function size(buff) {
  return buff.length;
}

export function toArray(buff) {
  var json = buff.toJSON();
  return json.data || json;
}

export function toArrayBuffer(buff) {
  return buff.buffer.slice(buff.byteOffset, buff.byteOffset + buff.byteLength);
}

export function fromArrayBuffer(ab) {
  return Buffer.from(ab);
}

export function fromStringImpl(str) {
  return function (encoding) {
    return Buffer.from(str, encoding);
  };
}

export function readImpl(ty) {
  return function (offset) {
    return function (buf) {
      return buf["read" + ty](offset);
    };
  };
}

export function readStringImpl(enc) {
  return function (start) {
    return function (end) {
      return function (buff) {
        return buff.toString(enc, start, end);
      };
    };
  };
}

export function getAtOffsetImpl(just) {
  return function (nothing) {
    return function (offset) {
      return function (buff) {
        var octet = buff[offset];
        return octet == null ? nothing : just(octet);
      };
    };
  };
}

export function toStringImpl(enc) {
  return function (buff) {
    return buff.toString(enc);
  };
}

export function slice(start) {
  return function (end) {
    return function (buff) {
      return buff.slice(start, end);
    };
  };
}

export function concat(buffs) {
  return Buffer.concat(buffs);
}

export function concatToLength(buffs) {
  return function (totalLength) {
    return Buffer.concat(buffs, totalLength);
  };
}
