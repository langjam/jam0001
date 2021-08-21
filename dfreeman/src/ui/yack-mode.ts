import { Parser, TokenizerState } from 'feldspar';
import { LineToken, LineTokensResult, TextMode, Tokenizer } from './ace';

const TokenMap: Record<string, string> = {
  DefKeyword: 'keyword',
  DataKeyword: 'keyword',
  FunKeyword: 'keyword',
  EndKeyword: 'keyword',
  MatchKeyword: 'keyword',
  Number: 'constant.numeric',
  Boolean: 'constant.language',
  Equals: 'keyword.operator',
  Pipe: 'keyword.operator',
  RArrow: 'keyword.operator',
  Identifier: 'identifier',
  MetaIdentifier: 'heading',
  CommentLine: 'comment',
  CommentExample: 'heading',
  CommentExampleEnd: 'heading',
  CommentExampleDescription: 'comment',
};

export class YackMode extends TextMode {
  public constructor(parser: Parser) {
    super();
    this.$tokenizer = new YackTokenizer(parser);
  }
}

export class YackTokenizer implements Tokenizer {
  private statesByName = new Map<string, TokenizerState>();

  public constructor(private readonly parser: Parser) {}

  public getLineTokens(line: string, initialState = '<INITIAL>'): LineTokensResult {
    try {
      let state = this.hydrateState(initialState);
      let result = this.parser.tokenize(line + '\n', state);

      let tokens: Array<LineToken> = [];
      let offset = 0;
      for (let token of result.tokens) {
        if (token.offset > offset) {
          tokens.push({ type: 'comment', value: line.slice(offset, token.offset) });
        }

        tokens.push({
          type: TokenMap[token.kind] ?? 'comment',
          value: token.content.replace(/\n$/, ''),
        });

        offset = token.offset + token.content.length;
      }

      if (line.length > offset) {
        tokens.push({ type: 'comment', value: line.slice(offset, line.length) });
      }

      return { tokens, state: this.dehydrateState(result.state) };
    } catch {
      return {
        tokens: [{ type: 'comment', value: line }],
        state: '<INITIAL>',
      };
    }
  }

  private dehydrateState(stack: ReadonlyArray<TokenizerState>): string {
    let names = ['<INITIAL>'];
    for (let state of stack) {
      let existing = this.statesByName.get(state.name);
      if (!existing) {
        this.statesByName.set(state.name, state);
      } else if (existing !== state) {
        throw new Error(`Internal error: duplicate state name '${state.name}'`);
      }

      names.push(state.name);
    }

    return names.join('||');
  }

  private hydrateState(stateString: string): Array<TokenizerState> {
    let stack: Array<TokenizerState> = [];
    for (let name of stateString.split('||')) {
      if (name !== '<INITIAL>') {
        let state = this.statesByName.get(name);
        if (!state) {
          throw new Error('Internal error: unrecognized editor tokenizer state');
        }
        stack.push(state);
      }
    }
    return stack;
  }
}
