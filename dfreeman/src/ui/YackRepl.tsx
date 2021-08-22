import React, { useRef } from 'react';
import type { Ace } from 'ace-builds';
import { Evaluator, Value, VoidValue } from '../evaluator';
import { YackEditor, YackMode } from './YackEditor';
import { parser } from '../parser';
import { ParseError } from 'feldspar';
import { unreachable } from '../utils';

export type ReplMessage =
  | { kind: 'log'; value: Value }
  | { kind: 'in'; text: string }
  | { kind: 'out'; value: Value }
  | { kind: 'error'; message: string };

export const YackRepl: React.FC<{
  evaluator: Evaluator;
  messages: Array<ReplMessage>;
  pushMessage: (message: ReplMessage) => void;
}> = ({ evaluator, messages, pushMessage }) => {
  let editor = useRef<Ace.Editor>();

  return (
    <>
      <div style={{ marginTop: '-1px' }}>
        {messages.map((value, index) => (
          <div key={index}>
            <ReplRow message={value} />
          </div>
        ))}
      </div>
      <div
        style={{ display: 'flex', flexFlow: 'row', paddingTop: '3px', borderTop: '1px solid #444' }}
      >
        <Marker kind="keyword" content="❯" />
        <YackEditor
          minLines={1}
          maxLines={10}
          showGutter={false}
          onLoad={(e) => (editor.current = e)}
          style={{ flex: 1, marginTop: '2px', maxHeight: '200px' }}
          commands={[
            {
              bindKey: { win: 'Enter', mac: 'Enter' },
              name: 'submit',
              exec(editor) {
                let source = editor.getValue();
                let pushedSource = false;
                try {
                  let ast = parser.parse(source)!;
                  pushMessage({ kind: 'in', text: source });
                  pushedSource = true;

                  if (ast.kind === 'expression') {
                    pushMessage({ kind: 'out', value: evaluator.evaluate(ast.value) });
                  } else {
                    evaluator.execute(ast.value);
                    pushMessage({ kind: 'out', value: new VoidValue() });
                  }
                  editor.setValue('');
                } catch (error) {
                  if (error instanceof ParseError && error.found.kind === '<EOF>') {
                    editor.insert('\n');
                    return;
                  }

                  if (!pushedSource) {
                    pushMessage({ kind: 'in', text: source });
                  }

                  pushMessage({ kind: 'error', message: error.message });
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

const Marker: React.FC<{ kind: string; content: string }> = ({ kind, content }) => {
  return (
    <span
      className="ace-tomorrow-night-eighties"
      style={{
        fontSize: '14px',
        width: '20px',
        height: '20px',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
      }}
    >
      <span className={`ace_${kind}`} style={{ padding: '0 2px 0 4px' }}>
        {content}
      </span>
    </span>
  );
};

const ReplRow: React.FC<{ message: ReplMessage }> = ({ message, ...props }) => {
  return (
    <div style={{ padding: '3px 0', display: 'flex', borderTop: '1px solid #333' }}>
      {(() => {
        if (message.kind === 'in') {
          return (
            <>
              <Marker kind="comment" content="❯" />
              <div style={{ paddingLeft: '4px' }}>
                <YackCode style={{ display: 'block' }} source={message.text} />
              </div>
            </>
          );
        } else if (message.kind === 'out' || message.kind === 'log') {
          return (
            <>
              <Marker kind="comment" content={message.kind === 'out' ? '❮' : ''} />
              <div style={{ paddingLeft: '4px' }}>
                <YackLoggedValue value={message.value} />
              </div>
            </>
          );
        } else if (message.kind === 'error') {
          return (
            <>
              <Marker kind="variable" content="⚠" />
              <div style={{ padding: '2px 0 0 4px' }} className="ace_editor">
                {message.message}
              </div>
            </>
          );
        }

        unreachable(message);
      })()}
    </div>
  );
};

const YackLoggedValue: React.FC<{ value: Value }> = ({ value, ...props }) => {
  if (value.kind === 'Comment') {
    return <>Special Comment Goes Here</>;
  } else if (value.kind === 'Function') {
    return <YackCode style={{ display: 'block' }} source={value.source} {...props} />;
  } else {
    return <YackValue value={value} {...props} />;
  }
};

const YackValue: React.FC<{ value: Value }> = ({ value, ...props }) => {
  if (value.kind === 'Boolean' || value.kind === 'Number') {
    return <YackCode source={String(value.value)} {...props} />;
  } else if (value.kind === 'Void') {
    return <YackCode source="void" {...props} />;
  } else if (value.kind === 'NativeFunction' || value.kind === 'Function') {
    return <YackCode source={`fun ${value.name}(${value.params.join(', ')})`} {...props} />;
  } else if (value.kind === 'Comment') {
    return <span {...props}>(Comment)</span>;
  } else if (value.kind === 'ADTInstance') {
    return (
      <span {...props}>
        <YackCode source={`${value.adtConstructor.name}(`} />
        {value.args.flatMap((arg, index) => {
          let result = [<YackValue value={arg} key={index} />];
          if (index > 0) {
            result.unshift(<YackCode source=", " key={`comma:${index}`} />);
          }
          return result;
        })}
        <YackCode source=")" />
      </span>
    );
  }

  unreachable(value);
};

const YackCode: React.FC<{ source: string; style?: React.CSSProperties }> = ({
  source,
  ...props
}) => {
  let state: string | undefined;
  let lineTokens = source.split('\n').map((line) => {
    let result = YackMode.$tokenizer.getLineTokens(line, state);
    state = result.state;
    return result.tokens;
  });

  return (
    <span
      className="ace_editor ace-tomorrow-night-eighties"
      {...props}
      style={{ whiteSpace: 'pre', marginTop: '2px', marginLeft: '1px', ...props.style }}
    >
      {lineTokens.flatMap((tokens, outer) => {
        let result = tokens.map((token, inner) => (
          <span
            key={`${outer}:${inner}`}
            className={token.type
              .split('.')
              .map((tag) => `ace_${tag}`)
              .join(' ')}
          >
            {token.value}
          </span>
        ));

        if (outer > 0) {
          result.unshift(<br key={`${outer}:newline`} />);
        }
        return result;
      })}
    </span>
  );
};
