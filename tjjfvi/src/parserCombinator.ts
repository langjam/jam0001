
type Loc = {
    str: string,
    ind: number,
  }

export type Parsing<R> = Generator<[R, Loc], void>
// Allows () => Parser<R> for deferring variable references on circular parsers
export type Parser<R> = ((loc: Loc) => Parsing<R>) | (() => Parser<R>)

export const callParser = <R, >(func: Parser<R>, loc: Loc): Parsing<R> => {
  let g = func(loc)
  while(typeof g === "function")
    g = g(loc)
  return g
}

export const or = <A extends Parser<any>[]>(...children: A): Parser<A extends Parser<infer R>[] ? R : never> =>
  function*(loc){
    for(const child of children)
      yield* callParser(child, loc)
  }

export const concat2 = <A, B>(a: Parser<A>, b: Parser<B>): Parser<[A, B]> => function*(loc){
  for(const [valueA, loc2] of callParser(a, loc))
    for(const [valueB, loc3] of callParser(b, loc2))
      yield [[valueA, valueB], loc3]
}

// eslint-disable-next-line max-len
export const concat = <A extends Parser<any>[]>(...args: A): Parser<{ [K in keyof A]: A[K] extends Parser<infer R> ? R : never }> =>
  map(args.reduce(concat2), x => args.slice(2).reduce(acc => [...acc[0], acc[1]], x)) as any

export const multiple = <R, >(child: Parser<R>, min = 0, max = Infinity): Parser<R[]> => function*(loc){
  if(min === 0)
    yield [[], loc]
  let queue = [{ parsing: callParser(child, loc), count: 1, results: [] as R[] }]
  let current: typeof queue[0] | undefined
  while((current = queue.shift())) {
    let { parsing, count, results } = current
    for(let value of parsing) {
      if(count >= min)
        yield [[...results, value[0]], value[1]]
      if(count < max)
        queue.push({ parsing: callParser(child, value[1]), count: count + 1, results: [...results, value[0]] })
    }
  }
}

export const end: Parser<null> = function*(loc){
  if(loc.ind === loc.str.length)
    yield [null, loc]
}

export const optional = <R, D>(child: Parser<R>, defaultValue: D): Parser<R | D> => function*(loc){
  yield [defaultValue, loc]
  yield* callParser(child, loc)
}
export const constant = <R = null>(value: R = null as any): Parser<R> => function*(loc){
  yield [value, loc]
}

export const string = <S extends string>(match: S): Parser<S> => function*(loc){
  if(loc.str.slice(loc.ind, loc.ind + match.length) === match)
    yield [match, { ind: loc.ind + match.length, str: loc.str }]
}

export const regex = (r: RegExp): Parser<RegExpExecArray> => function*(loc){
  let match = new RegExp(r.source, (r.flags ?? "") + "y").exec(loc.str.slice(loc.ind))
  if(match)
    yield [match, { str: loc.str, ind: loc.ind + match[0].length }]
}

export const map = <U, T>(f: Parser<T>, func: (x: T) => U): Parser<U> => function*(loc){
  for(const [value, loc2] of callParser(f, loc))
    yield [func(value), loc2]
}

export const parse = <R, >(parser: Parser<R>, str: string): R | null => {
  for(const [value, loc] of callParser(parser, { str, ind: 0 }))
    if(loc.ind === str.length)
      return value
  return null
}

export const surround = <R, >(before: Parser<unknown> | null, parser: Parser<R>, after: Parser<unknown> | null) =>
  map(concat(before ?? constant(), parser, after ?? constant()), ([, v]) => v)

export const prefix = <R, >(before: Parser<unknown>, parser: Parser<R>) =>
  surround(before, parser, null)

export const suffix = <R, >(parser: Parser<R>, after: Parser<unknown>) =>
  surround(null, parser, after)

export const join = <R, >(parser: Parser<R>, joiner: Parser<unknown>, allowTrailing = true) => optional(map(concat(
  multiple(suffix(parser, joiner)),
  multiple(parser, allowTrailing ? 0 : 1, 1),
), ([x, y]) => [...x, ...y]), [] as R[])

const _ws = regex(/\s*/)
export const wsBefore = <R, >(child: Parser<R>) => prefix(_ws, child)
export const wsAfter = <R, >(child: Parser<R>) => suffix(child, _ws)
export const ws = <R, >(child: Parser<R>) => surround(_ws, child, _ws)
export const whitespaceBefore = wsBefore
export const whitespaceAfter = wsAfter
export const whitespace = ws
