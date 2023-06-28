import buffer from "node:buffer";

export const inspectMaxBytes = () => buffer.constants.INSPECT_MAX_LENGTH;
export const maxLength = buffer.constants.MAX_LENGTH;
export const maxStringLength = buffer.constants.MAX_STRING_LENGTH;
