import React, { useCallback, useEffect, useMemo, useRef, useState } from 'react';
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

    For example, this is the number two:
    @example#first
      2
    @end

    And this is a hundred:
    @example#second
      100
    @end
    ###
    def fn = fun(a)
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
    A list represents an ordered collection of items.

    Since Yack is completely dynamically typed, lists can be
    pretty free-form. They don't need to contain a single type
    of value, and in fact they don't even need to include a
    terminating {Nil()} value.

    @example#oneTwoThree
      Cons(1,
        Cons(2,
          Cons(3, Nil())))
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
        console.log(parser.tokenize(source));
        console.error(error);
      }
    },
    [evaluator]
  );

  let updateSource = useMemo(() => debounce(doUpdateSource, 250), []);
  let scrollContainer = useRef<HTMLDivElement>(null);

  useEffect(() => doUpdateSource(source), []);
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
      <Pane flex="1">
        <YackEditor value={source} onChange={updateSource} />
      </Pane>
      <Pane flex="1">
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
