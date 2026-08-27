import * as idbKeyval from 'idb-keyval';

export const getValueByKeyImpl = idbKeyval.get;

export const setKeyValueImpl = idbKeyval.set;

export const delKeyValueImpl = idbKeyval.del;

export function clear () { idbKeyval.clear(); }