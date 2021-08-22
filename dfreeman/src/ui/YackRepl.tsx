import React, { createContext, RefObject, useContext, useEffect, useRef } from 'react';
import type { Ace } from 'ace-builds';
import { CommentValue, Evaluator, ExampleSegmentValue, Value, VoidValue } from '../evaluator';
import { YackEditor, YackMode } from './YackEditor';
import { parser } from '../parser';
import { ParseError } from 'feldspar';
import { unreachable } from '../utils';
import { ExampleSegment } from '../parser/ast';

export type ReplMessage =
  | { kind: 'log'; value: Value }
  | { kind: 'in'; text: string }
  | { kind: 'out'; value: Value }
  | { kind: 'error'; message: string };

export type PushMessage = (message: ReplMessage) => void;

const MessageContext = createContext<PushMessage>(() => {});

export const YackRepl = React.forwardRef<
  HTMLDivElement,
  {
    evaluator: Evaluator;
    messages: Array<ReplMessage>;
    pushMessage: PushMessage;
  }
>(({ evaluator, messages, pushMessage }, ref) => {
  let editor = useRef<Ace.Editor>();

  return (
    <MessageContext.Provider value={pushMessage}>
      <div style={{ overflow: 'auto', display: 'flex', flexFlow: 'column', flex: 1 }} ref={ref}>
        <div style={{ marginTop: '-1px' }}>
          {messages.map((value, index) => (
            <div key={index}>
              <ReplRow message={value} />
            </div>
          ))}
        </div>
        <div
          style={{
            display: 'flex',
            flexFlow: 'row',
            paddingTop: '3px',
            borderTop: '1px solid #444',
          }}
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
      </div>
    </MessageContext.Provider>
  );
});

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
              <div style={{ paddingLeft: '4px', display: 'flex', flex: 1 }}>
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
    return <YackComment comment={value} {...props} />;
  } else if (value.kind === 'Function') {
    return <YackCode style={{ display: 'block' }} source={value.source} {...props} />;
  } else {
    return <YackValue value={value} {...props} />;
  }
};

const YackValue: React.FC<{ value: Value }> = ({ value, ...props }) => {
  const pushMessage = useContext(MessageContext);

  if (value.kind === 'Boolean' || value.kind === 'Number') {
    return <YackCode source={String(value.value)} {...props} />;
  } else if (value.kind === 'Void') {
    return <YackCode source="void" {...props} />;
  } else if (value.kind === 'NativeFunction' || value.kind === 'Function') {
    return <YackCode source={`${value.name}(${value.params.join(', ')})`} {...props} />;
  } else if (value.kind === 'Comment') {
    return (
      <YackCode
        source={`${value.name}#`}
        style={{
          cursor: 'pointer',
          border: '1px solid #444',
          borderRadius: '2px',
          backgroundColor: '#444',
          margin: '0 -2px',
          padding: '0 2px',
        }}
        onClick={() => pushMessage({ kind: 'log', value })}
        {...props}
      />
    );
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

const YackCode: React.FC<{
  source: string;
  style?: React.CSSProperties;
  onClick?: React.MouseEventHandler;
}> = ({ source, ...props }) => {
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
      style={{ whiteSpace: 'pre', marginTop: '2px', ...props.style }}
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

const YackComment: React.FC<{ comment: CommentValue }> = ({ comment }) => {
  return (
    <div
      style={{
        padding: '1em',
        margin: '4px 4px 4px 0',
        borderRadius: '4px',
        backgroundColor: '#444',
      }}
      className="ace_editor"
    >
      <div style={{ fontSize: '130%', marginBottom: '8px' }}>{comment.name}</div>
      {comment.segments.map((segment, index) => {
        if (segment.kind === 'TextSegment') {
          return (
            <span key={index} style={{ whiteSpace: 'pre-wrap' }}>
              {segment.content}
            </span>
          );
        } else if (segment.kind === 'EmbedSegment') {
          return (
            <span
              className="ace-tomorrow-night-eighties"
              key={index}
              style={{
                display: 'inline-block',
                padding: '0 2px',
                margin: '0 -2px',
                borderRadius: '2px',
              }}
            >
              <YackValue value={segment.content.force()} key={index} />
            </span>
          );
        } else {
          return <YackCommentExample key={index} comment={comment} example={segment} />;
        }
      })}
    </div>
  );
};

const YackCommentExample: React.FC<{ comment: CommentValue; example: ExampleSegmentValue }> = ({
  example,
  comment,
  ...props
}) => {
  let pushMessage = useContext(MessageContext);
  let fullName = `${comment.name}#${example.name}`;

  return (
    <div
      {...props}
      style={{ marginTop: '1em', cursor: 'pointer' }}
      onClick={() => {
        pushMessage({ kind: 'in', text: fullName });
        pushMessage({ kind: 'out', value: example.content.force() });
      }}
    >
      <div style={{ display: 'flex' }}>
        <div
          className="ace-tomorrow-night-eighties"
          style={{ borderTopLeftRadius: '4px', borderTopRightRadius: '4px', padding: '.5em 1em 0' }}
        >
          <YackCode source={fullName} />
        </div>
      </div>
      <div
        className="ace-tomorrow-night-eighties"
        style={{
          borderRadius: '4px',
          borderTopLeftRadius: 0,
          padding: '1em',
        }}
      >
        <YackCode source={example.source} />
      </div>
    </div>
  );
};
