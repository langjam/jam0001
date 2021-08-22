import { Combinator, Push } from 'feldspar';
import { Continuation, ParseState, success } from 'feldspar/dist/parser/result';
import { Loc } from './ast';

export type NodeDetails = { source: string; loc: Loc };

class NodeCombinator<T, U> extends Combinator<U> {
  public constructor(
    private readonly combinator: Combinator<T>,
    private readonly construct: (details: NodeDetails, input: T) => U
  ) {
    super();
  }

  public children(): ReadonlyArray<Combinator> {
    return [this.combinator];
  }

  public parse(state: ParseState, push: Push, cont: Continuation<U>): void {
    let startToken = state.tokens[state.index];
    this.combinator.parse(state, push, (result) => {
      if (!result.success) {
        cont(result);
      } else {
        let endToken = result.state.tokens[result.state.index - 1];
        let endOffset = endToken.offset + endToken.content.length;
        let loc = { offset: startToken.offset, length: endOffset - startToken.offset };
        let source = state.source.slice(loc.offset, endOffset);
        cont(success(this.construct({ loc, source }, result.value), result.state));
      }
    });
  }
}

export function node<T, U>(
  factory: (details: NodeDetails, input: T) => U,
  combinator: Combinator<T>
): Combinator<U> {
  return new NodeCombinator(combinator, factory);
}
