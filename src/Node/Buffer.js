/* global Buffer */
export const freezeImpl= (a) => Buffer.from(a);

export const thawImpl = (a) => Buffer.from(a);

export const writeInternal = (ty, value, offset, buf) => 
  buf["write" + ty](value, offset);

export const writeStringInternal = (encoding, offset, length, value, buff) => 
  buff.write(value, offset, length, encoding);

export const setAtOffsetImpl = (value, offset, buff) => buff[offset] = value;

export const copyImpl = (srcStart, srcEnd, src, targStart, targ) => 
  src.copy(targ, targStart, srcStart, srcEnd);

export const fillImpl = (octet, start, end, buf) => buf.fill(octet, start, end);
