/* global exports */
/* global Buffer */
/* global require */
"use strict";

// module Node.Buffer

exports.showImpl = require('util').inspect;

exports.create = function (size) {
  return new Buffer(size);
};

exports.fromArray = function (octets) {
  return new Buffer(octets);
};

exports.fromStringImpl = function (str) {
  return function (encoding) {
    return new Buffer(str, encoding);
  };
};

exports.readImpl = function (ty) {
  return function (offset) {
    return function (buf) {
      return buf['read' + ty](offset);
    };
  };
};

exports.readStringImpl = function (enc) {
  return function (start) {
    return function (end) {
      return function (buff) {
        return buff.toString(enc, start, end);
      };
    };
  };
};

exports.toStringImpl = function (enc) {
  return function (buff) {
    return buff.toString(enc);
  };
};

exports.writeImpl = function (ty) {
  return function (value) {
    return function (offset) {
      return function (buf) {
        return function() {
          buf['write' + ty](value, offset);
          return {};
        }
      };
    };
  };
};

exports.writeStringImpl = function (enc) {
  return function (offset) {
    return function (length) {
      return function (value) {
        return function (buff) {
          return function() {
            return buff.write(value, offset, length, encoding);
          }
        };
      };
    };
  };
};

exports.toArray = function (buff) {
  return buff.toJSON();
};

exports.getAtOffsetImpl = function (nothing) {
  return function (just) {
    return function (buff) {
      return function (offset) {
        var octet = buff[offset];
        return octet == null ? nothing
                             : just(buff[i]);
      };
    };
  };
};

exports.setAtOffset = function (value) {
  return function (offset) {
    return function (buff) {
      buff[offset] = value;
      return {};
    };
  };
};

exports.size = function (buff) {
  return buff.length;
};



exports.concat = function (buffs) {
  return Buffer.concat(buffs);
};

exports.concat$prime = function (buffs) {
  return function (totalLength) {
    return Buffer.concat(buffs, totalLength);
  };
};

exports.copy = function (srcStart) {
  return function (srcEnd) {
    return function (src) {
      return function (targStart) {
        return function (targ) {
          return src.copy(targ, targStart, srcStart, strcEnd);
        };
      };
    };
  };
};

exports.fill = function (buff) {
  return function (octet) {
    return function (start) {
      return function (end) {
        return function() {
          buff.fill(octet, start, end);
          return {};
        }
      };
    };
  };
};
