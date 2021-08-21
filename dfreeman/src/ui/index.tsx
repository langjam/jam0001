import React, { useMemo, useRef, useState } from 'react';
import AceEditor, { IAceEditorProps } from 'react-ace';
import { render } from 'react-dom';
import { stripIndent } from 'common-tags';
import { ParseError } from 'feldspar';
import debounce from 'lodash.debounce';
import { parser } from '../parser';
import { YackMode } from './yack-mode';
import { Evaluator, Value } from '../evaluator';

import 'ace-builds/src-noconflict/theme-tomorrow_night_eighties';
import { Ace } from 'ace-builds';

const mode = new YackMode(parser);
const editorProps: Partial<IAceEditorProps> = {
  style: { width: '100%', height: '100%' },
  mode,
  theme: 'tomorrow_night_eighties',
  setOptions: { useSoftTabs: true, tabSize: 2 },
};

const Pane: React.FC<{ flex: string }> = ({ flex, children }) => {
  return (
    <div style={{ flex, margin: '20px', display: 'flex' }} className="ace-tomorrow-night-eighties">
      <div
        style={{ display: 'flex', flexFlow: 'column', flex: 1, boxShadow: '3px 3px 3px 3px black' }}
      >
        {children}
      </div>
    </div>
  );
};

type Input = { kind: 'Input'; value: string };
type Failure = { kind: 'Failure'; message: string };
type Message = Value | Input | Failure;

const Repl: React.FC<{ evaluator: Evaluator }> = ({ evaluator }) => {
  let [messages, setMessages] = useState<Array<Message>>([]);
  let editor = useRef<Ace.Editor>();

  return (
    <>
      {messages.map((value, index) => (
        <div key={index}>
          <YackMessage message={value} />
        </div>
      ))}
      <div style={{ display: 'flex', flexFlow: 'row' }}>
        <span style={{ color: 'green' }}>❯</span>
        <AceEditor
          {...editorProps}
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
                let input = { kind: 'Input', value: source } as const;
                try {
                  let ast = parser.parse(source)!;

                  if (ast.kind === 'expression') {
                    let value = evaluator.evaluate(ast.value);
                    setMessages((messages) => [...messages, input, value]);
                  } else {
                    evaluator.execute(ast.value);
                    setMessages((messages) => [...messages, input]);
                  }
                  editor.setValue('');
                } catch (error) {
                  if (error instanceof ParseError && error.found.kind === '<EOF>') {
                    editor.insert('\n');
                    return;
                  }

                  let failure = { kind: 'Failure', message: error.message } as const;
                  setMessages((messages) => [...messages, input, failure]);
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
    let result = mode.$tokenizer.getLineTokens(line, state);
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

const YackMessage: React.FC<{ message: Message }> = ({ message }) => {
  if (message.kind === 'Boolean' || message.kind === 'Number' || message.kind === 'Input') {
    return <YackText code={String(message.value)} />;
  } else if (message.kind === 'Void') {
    return <></>;
  } else if (message.kind === 'Function') {
    return <YackText code={message.source} />;
  } else if (message.kind === 'Failure') {
    return <span>{message.message}</span>;
  }
  return <>unsupported value</>;
};

const Playground: React.FC = () => {
  let evaluator = new Evaluator();
  let [source, setSource] = useState(stripIndent`
    ###
    Ok I can write a reasonable amount of stuff here.

    @example#first the number two
      2
    @end

    @example#second the number 100
      100
    @end
    ###
    def simpleTestValue = fun(a)
      fun(_)
        a
      end
    end

    def isEmpty = fun(list)
      match list
        | Cons(_, _) -> false
        | Nil() -> true
      end
    end

    def isOneTwoThree = fun(list)
      match list
        | Cons(1, Cons(2, Cons(3, Nil()))) -> true
        | _ -> false
      end
    end

    ###
    A list represents some things.

    @example#oneTwoThree
      Cons(1, Cons(2, Cons(3, Nil())))
    @end
    ###
    data List = Nil() | Cons(head, tail)

    def list = Cons(1, Cons(2, Nil()))
    def ref = isOneTwoThree(List#oneTwoThree)
  `);

  let doUpdateSource = (source: string) => {
    evaluator.reset();
    setSource(source);
    try {
      let ast = parser.parse(source)!;
      if (ast.kind !== 'script') return;

      evaluator.execute(ast.value);
    } catch (error) {
      // TODO
    }
  };

  let updateSource = useMemo(() => debounce(doUpdateSource, 250), []);

  updateSource(source);

  return (
    <>
      <Pane flex="1">
        <AceEditor {...editorProps} value={source} onChange={updateSource} />
      </Pane>
      <Pane flex="1">
        <Repl evaluator={evaluator} />
      </Pane>
    </>
  );
};

render(<Playground />, document.getElementById('application')!);
