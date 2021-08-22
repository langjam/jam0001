import React, { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { render } from 'react-dom';
import debounce from 'lodash.debounce';
import { parser } from '../parser';
import { Evaluator } from '../evaluator';
import { buildNativeDefinitions } from '../evaluator/native-functions';
import { ReplMessage, YackRepl } from './YackRepl';
import { YackEditor } from './YackEditor';
import { Sidebar } from './Sidebar';
import { Example, Examples } from './examples';

const Pane: React.FC<{ flex: string }> = ({ flex, children }) => {
  return (
    <div
      style={{ flex, marginRight: '10px', marginBottom: '10px', display: 'flex' }}
      className="ace-tomorrow-night-eighties"
    >
      <div
        style={{
          display: 'flex',
          flexFlow: 'column',
          flex: 1,
          boxShadow: '1px 1px 8px 4px #0006',
          border: '1px solid #292929',
        }}
      >
        {children}
      </div>
    </div>
  );
};

const Playground: React.FC = () => {
  let [messages, setMessages] = useState<Array<ReplMessage>>([]);
  let [example, setExample] = useState(Examples[1] as Example);
  let [source, setSource] = useState('');

  let pushMessage = (message: ReplMessage) => setMessages((messages) => [...messages, message]);
  let evaluator = useMemo(
    () =>
      new Evaluator(
        buildNativeDefinitions({ log: (value) => pushMessage({ kind: 'log', value }) })
      ),
    []
  );
  let doUpdateSource = useCallback(
    (source: string) => {
      evaluator.reset();
      setSource(source);

      Object.assign(window, { source, evaluator, parser });

      try {
        let ast = parser.parse(source)!;
        if (ast.kind !== 'script') return;

        evaluator.execute(ast.value);
      } catch (error) {
        // TODO
        console.error(error);
      }
    },
    [evaluator]
  );

  let updateSource = useMemo(() => debounce(doUpdateSource, 250), []);
  let scrollContainer = useRef<HTMLDivElement>(null);

  useEffect(() => {
    doUpdateSource(example.source);

    setMessages([]);
    for (let replInput of example.repl ?? []) {
      pushMessage({ kind: 'in', text: replInput });
      try {
        let ast = parser.parse(replInput)!;
        if (ast.kind === 'expression') {
          pushMessage({ kind: 'out', value: evaluator.evaluate(ast.value) });
        } else {
          evaluator.execute(ast.value);
        }
      } catch (error) {
        pushMessage({ kind: 'error', message: error.message });
      }
    }
  }, [example]);

  useEffect(() => scrollContainer.current?.scrollTo(0, 999999999), [messages.length]);

  return (
    <div
      style={{
        display: 'flex',
        flexFlow: 'row',
        flex: 1,
        paddingTop: '10px',
        paddingLeft: '10px',
        backgroundColor: '#555',
      }}
    >
      <Pane flex="1 0 200px">
        <Sidebar examples={Examples} selected={example} onSelect={setExample} />
      </Pane>
      <Pane flex="10">
        <YackEditor value={source} onChange={updateSource} />
      </Pane>
      <Pane flex="10">
        <YackRepl
          evaluator={evaluator}
          messages={messages}
          pushMessage={pushMessage}
          ref={scrollContainer}
        />
      </Pane>
    </div>
  );
};

render(<Playground />, document.getElementById('application')!);
