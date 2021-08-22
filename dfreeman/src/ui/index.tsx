import React, { useCallback, useEffect, useMemo, useState } from 'react';
import { render } from 'react-dom';
import { stripIndent } from 'common-tags';
import debounce from 'lodash.debounce';
import { parser } from '../parser';
import { Evaluator } from '../evaluator';
import { buildNativeDefinitions } from '../evaluator/native-functions';
import { ReplMessage, YackRepl } from './YackRepl';
import { YackEditor } from './YackEditor';

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
      try {
        let ast = parser.parse(source)!;
        if (ast.kind !== 'script') return;

        evaluator.execute(ast.value);
      } catch (error) {
        // TODO
      }
    },
    [evaluator]
  );

  useEffect(() => doUpdateSource(source), []);

  let updateSource = useMemo(() => debounce(doUpdateSource, 250), []);

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
      <Pane flex="1">
        <YackEditor value={source} onChange={updateSource} />
      </Pane>
      <Pane flex="1">
        <YackRepl evaluator={evaluator} messages={messages} pushMessage={pushMessage} />
      </Pane>
    </div>
  );
};

render(<Playground />, document.getElementById('application')!);
