import Ace from 'ace-builds';

export type LineToken = {
  type: string;
  value: string;
};

export type LineTokensResult = {
  state: string;
  tokens: Array<LineToken>;
};

export interface Tokenizer {
  getLineTokens(line: string, initialState?: string): LineTokensResult;
}

export interface TextMode {
  $tokenizer: Tokenizer;
}

export const TextMode = Ace.require('ace/mode/text').Mode as {
  new (): TextMode;
};
