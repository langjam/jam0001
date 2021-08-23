import { stripIndent } from 'common-tags';
import { Evaluator, NumberValue } from '.';
import { parser } from '../parser';
import { Identifier, Script } from '../parser/ast';

describe('Evaluator', () => {
  test('closures', () => {
    let source = stripIndent`
      def foo = fun(a)
        fun(_)
          a
        end
      end

      def bar = foo(123)(456)
    `;

    let ast = parser.parse(source)?.value as Script;
    let evaluator = new Evaluator();

    evaluator.execute(ast);

    let out = evaluator.evaluate(new Identifier('bar', false, null, { offset: 0, length: 3 }));

    expect(out).toEqual(new NumberValue(123));
  });
});
