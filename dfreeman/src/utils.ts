export function unreachable(value: never): never {
  throw new Error('Internal error: hit allegedly-unreachable code. ' + value);
}
