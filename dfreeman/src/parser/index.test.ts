import { parser } from '.';
import { stripIndent } from 'common-tags';
import { NumberLiteral } from './ast';

test('parsing a comment', () => {
  let input = stripIndent`
    ###
    Hello this is a comment

    Some more stuff

    @example:foo
      2
    @end

    Ok done.
    ###
    def foo = 123
  `;

  let result: any;

  try {
    result = parser.parse(input);
    console.log(result);
  } catch (error) {
    console.log(error);
    console.log(JSON.stringify(input.slice(error.offset)));
  }

  expect(result).toMatchInlineSnapshot(`
Script {
  "declarations": Map {
    "foo" => Object {
      "declaration": DefDeclaration {
        "kind": "DefDeclaration",
        "loc": Object {
          "length": 13,
          "offset": 82,
        },
        "name": Identifier {
          "isMeta": false,
          "kind": "Identifier",
          "loc": Object {
            "length": 3,
            "offset": 86,
          },
          "metaValue": null,
          "value": "foo",
        },
        "value": NumberLiteral {
          "kind": "Number",
          "loc": Object {
            "length": 3,
            "offset": 92,
          },
          "value": 123,
        },
      },
      "meta": SemanticComment {
        "kind": "SemanticComment",
        "loc": Object {
          "length": 81,
          "offset": 0,
        },
        "segments": Array [
          Object {
            "content": "Hello this is a comment

Some more stuff

@example:foo
2
@end

Ok done.",
            "kind": "TextSegment",
          },
        ],
      },
      "value": NumberLiteral {
        "kind": "Number",
        "loc": Object {
          "length": 3,
          "offset": 92,
        },
        "value": 123,
      },
    },
  },
  "kind": "Script",
}
`);
});
