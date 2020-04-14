import { array } from 'fp-ts/lib/Array';
import { Eq } from 'fp-ts/lib/Eq';
import { Magma } from 'fp-ts/lib/Magma';
import { deleteAt, fromFoldable, insertAt, lookup, toArray } from 'fp-ts/lib/Map';
import { Ord, ordString } from 'fp-ts/lib/Ord';
import { Option, none, some } from 'fp-ts/lib/Option';
import { Lens } from 'monocle-ts';

interface LRUValue<V> {
  _accessTime: number;
  _value: V;
}

interface LRUCache<K, V> {
  _cache: Map<K, LRUValue<V>>;
  _capacity: number;
  _currentLength: number;
  _maxReadTime: number;
}

const emptyVal: LRUCache<any, any> = {
  _capacity: 0,
  _maxReadTime: 0,
  _currentLength: 0,
  _cache: new Map<any, LRUValue<any>>(),
};

// TODO: Make this more typesafe.
// const capacity:<k,v>(cache: LRUCache<k,v>) => Lens<LRUCache<any, any>, number> = Lens.fromProp<LRUCache<any, any>>()('_capacity');
export const capacity = Lens.fromProp<LRUCache<any, any>>()('_capacity');
export const maxReadTime = Lens.fromProp<LRUCache<any, any>>()('_maxReadTime');
export const currentLength = Lens.fromProp<LRUCache<any, any>>()('_currentLength');
export const cache = Lens.fromProp<LRUCache<any, any>>()('_cache');
export const accessTime = Lens.fromProp<LRUValue<any>>()('_accessTime');
export const value = Lens.fromProp<LRUValue<any>>()('_value');

export function makeSizedLRU<K, V>(size: number): LRUCache<K, V> {
  const myLens = capacity.set(size);
  return myLens(emptyVal);
}

export function mapWithKeyFunc<K, V>(
  acc: Option<[K, LRUValue<V>]>,
  pair: [K, LRUValue<V>],
  _currIndex: number,
  _array: [K, LRUValue<V>][],
): Option<[K, LRUValue<V>]> {
  const v = pair[1];
  if (acc._tag === 'None') {
    return some(pair);
  }
  const vP = acc.value[1];
  if (vP._accessTime < v._accessTime) {
    return some(pair);
  }
  return some(acc.value);
}

export function removeOldestKey<K>(O: Ord<K>): <V>(inputMap: Map<K, LRUValue<V>>) => Map<K, LRUValue<V>> {
  const mToA = toArray(O);
  const mapDel = deleteAt(O);
  return (inputMap) => {
    const mapArray = mToA(inputMap);
    // let altMaybePair = mapArray.reduce(mapWithKeyFunc, none);
    // console.log(altMaybePair);
    // TODO: change this to mapWithKeyFunc. As of right now, it doesn't typecheck, which is mysterious to me.
    const oldestMaybePair = mapArray.reduce(
      (
        prevVal: Option<[any, LRUValue<any>]>,
        currVal: [any, LRUValue<any>],
        _currIndex: number,
        _array: [any, LRUValue<any>][],
      ): Option<[any, LRUValue<any>]> => {
        const v = currVal[1];
        if (prevVal._tag === 'None') {
          return some(currVal);
        }
        const vP = prevVal.value[1];
        if (vP._accessTime > v._accessTime) {
          return some(currVal);
        }
        return prevVal;
      },
      none,
    );
    // let oldestMaybePair = none;
    if (oldestMaybePair._tag === 'None') {
      return inputMap;
    }
    const result = mapDel(oldestMaybePair.value[0])(inputMap);
    return result;
  };
}

export function insertIntoCache<K>(O: Ord<K>): <V>(key: K, value: V, inputCache: LRUCache<K, V>) => LRUCache<K, V> {
  const mapDel = deleteAt(O);
  const mapIns = insertAt(O);
  const mapLookup = lookup(O);
  const removeOK = removeOldestKey(O);
  return (key, val, inputCache) => {
    const currCap = capacity.get(inputCache);
    const currLen = currentLength.get(inputCache);
    const insertTime = maxReadTime.get(inputCache);
    const readTimeLens = maxReadTime.set(insertTime + 1);
    const lruMaybe = mapLookup(key, cache.get(inputCache));
    const newNode: LRUValue<typeof val> = {
      _value: val,
      _accessTime: insertTime + 1,
    };
    let resultCache = readTimeLens(inputCache);

    if (lruMaybe._tag === 'Some') {
      // Key exists, we're overwriting a value, so just delete the old one and insert the new one.
      resultCache = cache.modify(mapDel(key))(resultCache);
      resultCache = cache.modify(mapIns(key, newNode))(resultCache);
      return resultCache;
    }

    if (currLen + 1 <= currCap) {
      resultCache = currentLength.modify((x) => {
        return x + 1;
      })(resultCache);
      resultCache = cache.modify(mapIns(key, newNode))(resultCache);
      return resultCache;
    }
    resultCache = cache.modify(removeOK)(resultCache);
    resultCache = cache.modify(mapIns(key, newNode))(resultCache);
    return resultCache;
  };
}

export function updateTimestamp<K>(O: Eq<K>): <V>(key: K, inputCache: LRUCache<K, V>) => LRUCache<K, V> {
  const mapLookup = lookup(O);
  const mapDel = deleteAt(O);
  const mapIns = insertAt(O);
  return (key, inputCache) => {
    const mapMaybe = mapLookup(key, inputCache._cache);
    if (mapMaybe._tag === 'None') {
      return inputCache;
    }
    const insertTime = maxReadTime.get(inputCache);
    const readTimeLens = maxReadTime.set(insertTime + 1);
    const accessTimeLens = accessTime.set(insertTime + 1);
    let resultCache = readTimeLens(inputCache);
    const val = accessTimeLens(mapMaybe.value);
    resultCache = cache.modify(mapDel(key))(resultCache);
    resultCache = cache.modify(mapIns(key, val))(resultCache);
    return resultCache;
  };
}

export function getFromCache<K>(O: Eq<K>): <V>(key: K, inputCache: LRUCache<K, V>) => [Option<V>, LRUCache<K, V>] {
  const lookupFunc = lookup(O);
  const uTime = updateTimestamp(O);
  return (key, inputCache) => {
    const lruMaybe = lookupFunc(key, inputCache._cache);
    switch (lruMaybe._tag) {
      case 'None':
        return [none, inputCache];
      case 'Some':
        const val = lruMaybe.value._value;
        const newCache = uTime(key, inputCache);
        return [some(val), newCache];
    }
  };
}

// tslint:disable-next-line:no-console
const log = console.log;

export function tests() {
  let myLRU: LRUCache<string, string> = makeSizedLRU(5);
  const iic = insertIntoCache(ordString);
  const maps: [string, string][] = [
    ['Adam', 'McCullough'],
    ['Evelyn', 'Paladin'],
    ['Adam', 'McCullough'],
    ['Strix', 'Nuisance'],
    ['Paulton', 'Drama'],
    ['Diath', 'Awesome'],
    ['Bump', 'Evelyn'],
  ];
  log('Array:');
  log(maps);
  log('Initialize map.');
  myLRU = maps.reduce((lru, pair) => iic(pair[0], pair[1], lru), myLRU);
  log('Print first iteration');
  log(myLRU);
  log('Inserted extra value.');
  myLRU = iic('key', 'value', myLRU);
  log(myLRU);
  log('Inserted last value.');
  myLRU = iic('key1', 'value1', myLRU);
  log(myLRU);
}

const magmaNumber: Magma<number> = {
  concat: (x, y) => x + y,
};

export function main() {
  const tArr = toArray(ordString);
  const sample = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  const fFold = fromFoldable(ordString, magmaNumber, array);
  const sampleMap: Map<string, number> = fFold([
    ['hello', 5],
    ['goodbye', 7],
    ['dude', 4],
  ]);
  const sampleToArr: [string, number][] = tArr(sampleMap);
  const answer = sample.reduce((b: string, a: number) => {
    return b + a.toString(10);
  }, '');
  const message = 'Hello, world!';
  const myCache = makeSizedLRU(10);

  log(answer);
  log(sampleMap);
  log(sampleToArr);
  log(message);
  log('Ahoy!');
  log('Cache size:');
  log(myCache._capacity);
  tests();
}

main();
