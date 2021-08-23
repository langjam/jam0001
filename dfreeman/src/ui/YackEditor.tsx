import React from 'react';
import { TokenizerState } from 'feldspar';
import { parser } from '../parser';
import { LineToken, LineTokensResult, TextMode, Tokenizer } from './ace';
import { IAceEditorProps } from 'react-ace';
import ReactAce from 'react-ace/lib/ace';

import 'ace-builds/src-noconflict/theme-tomorrow_night_eighties';

export const YackEditor: React.FC<IAceEditorProps> = (props) => {
  return (
    <ReactAce
      mode={YackMode}
      theme="tomorrow_night_eighties"
      showPrintMargin={false}
      highlightActiveLine={false}
      {...props}
      style={{ width: '100%', height: '100%', ...props.style }}
      setOptions={{ useSoftTabs: true, tabSize: 2, ...props.setOptions }}
    />
  );
};

const TokenMap: Record<string, string> = {
  DefKeyword: 'keyword',
  DataKeyword: 'keyword',
  FunKeyword: 'keyword',
  EndKeyword: 'keyword',
  MatchKeyword: 'keyword',
  Void: 'constant.language',
  Number: 'constant.numeric',
  Boolean: 'constant.language',
  Equals: 'keyword.operator',
  BinOp: 'keyword.operator',
  Pipe: 'keyword.operator',
  RArrow: 'keyword.operator',
  Identifier: 'identifier',
  MetaIdentifier: 'heading',
  CommentText: 'comment',
  CommentExample: 'heading',
  CommentExampleEnd: 'heading',
  CommentEmbedStart: 'comment',
  CommentEmbedEnd: 'comment',
};

class YackModeInstance extends TextMode {
  public constructor() {
    super();
    this.$tokenizer = new YackTokenizer();
  }
}

class YackTokenizer implements Tokenizer {
  private statesByName = new Map<string, TokenizerState>();

  public constructor() {}

  public getLineTokens(line: string, initialState = '<INITIAL>'): LineTokensResult {
    try {
      let state = this.hydrateState(initialState);
      let result = parser.tokenize(line + '\n', state);

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
        tokens: [{ type: 'text', value: line }],
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

export const YackMode = new YackModeInstance();
