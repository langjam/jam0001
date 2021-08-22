import { CommentValue, Definition, NativeFunctionValue, Value, VoidValue } from '.';

export type NativeDefinitionsConfig = {
  log: (value: Value) => void;
};

export function buildNativeDefinitions(
  config: NativeDefinitionsConfig
): Record<string, Definition> {
  return {
    log: {
      meta: new CommentValue(
        [{ kind: 'TextSegment', content: 'Logs a value to the screen.' }],
        new Map()
      ),
      value: new NativeFunctionValue('log', ['message'], (_, [message]) => {
        config.log(message);
        return new VoidValue();
      }),
    },
  };
}
