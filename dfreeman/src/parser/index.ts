import {
  alt,
  Combinator,
  defer,
  maybe,
  Parser,
  repeat,
  repeatOne,
  repeatOneSep,
  repeatSep,
  seq,
  state,
  token,
} from 'feldspar';
import {
  SemanticComment,
  Expression,
  Identifier,
  NumberLiteral,
  DefDeclaration,
  Script,
  FunctionExpression,
  CallExpression,
  ADTDeclaration,
  Pattern,
  ADTConstructorPattern,
  MatchExpression,
  BooleanLiteral,
} from './ast';
import { node } from './combinators';

////////////////////////////////////////////////////////////////
//// Basic tokens
////

const ws = token(/\s+/, { priority: 0, silent: true });
const ignoredComment = token(/#.*(?=\n|$)/, { priority: 0x100, silent: true });

const def = token(/def\b/, { kind: 'DefKeyword' });
const data = token(/data\b/, { kind: 'DataKeyword' });
const fun = token(/fun\b/, { kind: 'FunKeyword' });
const match = token(/match\b/, { kind: 'MatchKeyword' });
const end = token(/end\b/, { kind: 'EndKeyword' });
const eq = token('=', { kind: 'Equals' });
const rArrow = token('->', { kind: 'RArrow' });
const lParen = token('(', { kind: 'LParen' });
const rParen = token(')', { kind: 'RParen' });
const comma = token(',', { kind: 'Comma' });
const semicolon = token(';', { kind: 'Semicolon' });
const pipe = token('|', { kind: 'Pipe' });

const identifier = node(
  Identifier.fromToken,
  token(/[a-z_][a-z0-9_]*\b(?!#)/i, {
    kind: 'Identifier',
    priority: 0x100,
  })
);

const metaIdentifier = node(
  Identifier.fromToken,
  token(/[a-z_][a-z0-9_]*(?:#(?:[a-z][a-z0-9]*)?)/i, {
    kind: 'MetaIdentifier',
    priority: 0x100,
  })
);

const number = node(NumberLiteral.fromToken, token(/\d+/, { kind: 'Number' }));
const boolean = node(BooleanLiteral.fromToken, token(/true\b|false\b/, { kind: 'Boolean' }));

////////////////////////////////////////////////////////////////
//// Patterns
////

const pattern: Combinator<Pattern> = defer(() =>
  alt(identifier, number, boolean, constructorPattern)
);

const constructorPattern = node(
  ADTConstructorPattern.fromSequence,
  seq(identifier, lParen, repeatSep(pattern, comma), rParen)
);

////////////////////////////////////////////////////////////////
//// Expressions
////

const expression: Combinator<Expression> = defer((expr) =>
  alt(
    number,
    boolean,
    identifier,
    metaIdentifier,
    functionExpression,
    callExpression,
    matchExpression,
    seq(lParen, expr, rParen).map((seq) => seq[1])
  )
);

const functionExpression = node(
  FunctionExpression.fromSequence,
  seq(
    fun,
    maybe(identifier),
    seq(lParen, repeatSep(identifier, comma), rParen).map((seq) => seq[1]),
    repeatSep(expression, semicolon),
    end
  )
);

const callExpression = node(
  CallExpression.fromSequence,
  seq(expression, lParen, repeatSep(expression, comma), rParen)
);

const matchExpression = node(
  MatchExpression.fromSequence,
  seq(match, expression, repeatOne(seq(pipe, pattern, rArrow, expression)), end)
);

////////////////////////////////////////////////////////////////
//// Semantic Comments
////

const inComment = state('in-comment', { exclusive: true });
const inExampleCode = state('in-example-code');
const inExampleDescription = state('in-example-dscription', {
  exclusive: true,
});

const commentStart = token(/###\s*/, { kind: 'CommentStart', pushState: inComment });
const commentLine = token(/.*\n/, { kind: 'CommentLine', requiredState: inComment });
const commentEnd = token(/\s*###/, { kind: 'CommentEnd', popState: inComment, priority: 0 });
const commentExampleStart = token(/\s*@example#[a-zA-Z_][a-zA-Z0-9_]*\b/, {
  kind: 'CommentExample',
  requiredState: inComment,
  priority: 0,
  pushState: inExampleDescription,
});

const commentExampleDescription = token(/.*\n/, {
  kind: 'CommentExampleDescription',
  popState: inExampleDescription,
  pushState: inExampleCode,
});

const commentExampleEnd = token(/\s*@end/, {
  kind: 'CommentExampleEnd',
  popState: inExampleCode,
});

const commentExample = seq(
  commentExampleStart,
  commentExampleDescription,
  expression,
  commentExampleEnd
);
const comment = node(
  SemanticComment.fromSegments,
  seq(commentStart, repeat(alt(commentLine, commentExample)), commentEnd)
);

////////////////////////////////////////////////////////////////
//// Declarations
////

const defDeclaration = node(DefDeclaration.fromSequence, seq(def, identifier, eq, expression));

const adtDeclaration = node(
  ADTDeclaration.fromSequence,
  seq(
    data,
    identifier,
    eq,
    repeatOneSep(seq(identifier, lParen, repeatSep(identifier, comma), rParen), pipe)
  )
);

const declaration = alt(defDeclaration, adtDeclaration);

////////////////////////////////////////////////////////////////
//// Script
////

const script = repeat(seq(maybe(comment), declaration)).map(Script.fromSequence);

////////////////////////////////////////////////////////////////
//// Entry
////

const entry = alt(
  expression.map((value) => ({ kind: 'expression' as const, value })),
  script.map((value) => ({ kind: 'script' as const, value }))
);

export const parser = new Parser(entry, [ws, ignoredComment]);
