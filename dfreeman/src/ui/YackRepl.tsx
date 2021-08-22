import React, { useRef } from 'react';
import type { Ace } from 'ace-builds';
import { Evaluator, Value, VoidValue } from '../evaluator';
import { YackEditor, YackMode } from './YackEditor';
import { parser } from '../parser';
import { ParseError } from 'feldspar';

export type Input = { kind: 'Input'; value: string };
export type Failure = { kind: 'Failure'; message: string };
export type ReplMessage = Value | Input | Failure;

export const YackRepl: React.FC<{
  evaluator: Evaluator;
  messages: Array<ReplMessage>;
  pushMessage: (message: ReplMessage) => void;
}> = ({ evaluator, messages, pushMessage }) => {
  let editor = useRef<Ace.Editor>();

  return (
    <>
      <div>
        {messages.map((value, index) => (
          <div key={index}>
            <YackMessage message={value} />
          </div>
        ))}
      </div>
      <div style={{ display: 'flex', flexFlow: 'row' }}>
        <span style={{ color: 'green' }}>❯</span>
        <YackEditor
          minLines={1}
          maxLines={10}
          showGutter={false}
          highlightActiveLine={false}
          onLoad={(e) => (editor.current = e)}
          commands={[
            {
              bindKey: { win: 'Enter', mac: 'Enter' },
              name: 'submit',
              exec(editor) {
                let source = editor.getValue();
                try {
                  let ast = parser.parse(source)!;
                  pushMessage({ kind: 'Input', value: source });

                  if (ast.kind === 'expression') {
                    pushMessage(evaluator.evaluate(ast.value));
                  } else {
                    evaluator.execute(ast.value);
                    pushMessage(new VoidValue());
                  }
                  editor.setValue('');
                } catch (error) {
                  if (error instanceof ParseError && error.found.kind === '<EOF>') {
                    editor.insert('\n');
                    return;
                  }

                  pushMessage({ kind: 'Failure', message: error.message });
                  editor.setValue('');
                }
              },
            },
          ]}
        />
      </div>
      <div style={{ flex: 1 }} onClick={() => editor?.current?.focus()}></div>
    </>
  );
};

const YackText: React.FC<{ code: string }> = ({ code }) => {
  let state: string | undefined;
  let lineTokens = code.split('\n').map((line) => {
    let result = YackMode.$tokenizer.getLineTokens(line, state);
    state = result.state;
    return result.tokens;
  });

  return (
    <div style={{ whiteSpace: 'pre' }} className="ace_editor ace-tomorrow-night-eighties">
      {lineTokens.flatMap((tokens, outer) => {
        return [
          ...tokens.map((token, inner) => (
            <span
              key={`${outer}:${inner}`}
              className={token.type
                .split('.')
                .map((tag) => `ace_${tag}`)
                .join(' ')}
            >
              {token.value}
            </span>
          )),
          <br key={`${outer}:newline`} />,
        ];
      })}
    </div>
  );
};

const YackMessage: React.FC<{ message: ReplMessage }> = ({ message }) => {
  if (message.kind === 'Boolean' || message.kind === 'Number' || message.kind === 'Input') {
    return <YackText code={String(message.value)} />;
  } else if (message.kind === 'Void') {
    return <></>;
  } else if (message.kind === 'NativeFunction') {
    return <YackText code={`fun ${message.name}(${message.params.join(', ')})`} />;
  } else if (message.kind === 'Function') {
    return <YackText code={message.source} />;
  } else if (message.kind === 'Failure') {
    return <span>{message.message}</span>;
  }
  return <>unsupported value</>;
};
