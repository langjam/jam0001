import { stripIndent } from 'common-tags';
import { Token } from 'feldspar';
import { unreachable } from '../utils';
import { NodeDetails } from './combinators';

export type Loc = Readonly<{ offset: number; length: number }>;

export type Declaration = DefDeclaration | ADTDeclaration;
export type Expression =
  | NumberLiteral
  | BooleanLiteral
  | VoidLiteral
  | BinOpExpression
  | FunctionExpression
  | CallExpression
  | Identifier
  | ADTConstructor
  | MatchExpression;

export type Pattern =
  | VoidLiteral
  | NumberLiteral
  | BooleanLiteral
  | Identifier
  | ADTConstructorPattern;

export class VoidLiteral {
  public readonly kind = 'Void';
  public constructor(public readonly loc: Loc) {}

  public static fromToken({ loc }: NodeDetails): VoidLiteral {
    return new VoidLiteral(loc);
  }
}

export class NumberLiteral {
  public readonly kind = 'Number';
  public constructor(public readonly value: number, public readonly loc: Loc) {}

  public static fromToken({ loc }: NodeDetails, token: Token): NumberLiteral {
    return new NumberLiteral(Number(token.content), loc);
  }
}

export class BooleanLiteral {
  public readonly kind = 'Boolean';
  public constructor(public readonly value: boolean, public readonly loc: Loc) {}

  public static fromToken({ loc }: NodeDetails, token: Token): BooleanLiteral {
    return new BooleanLiteral(token.content === 'true', loc);
  }
}

export class Identifier {
  public readonly kind = 'Identifier';
  public constructor(
    public readonly value: string,
    public readonly isMeta: boolean,
    public readonly metaValue: string | null,
    public readonly loc: Loc
  ) {}

  public toString(): string {
    if (this.isMeta) {
      return `${this.value}#${this.metaValue ?? ''}`;
    } else {
      return this.value;
    }
  }

  public static fromToken({ loc }: NodeDetails, token: Token): Identifier {
    let hashIndex = token.content.indexOf('#');
    let isMeta = hashIndex !== -1;
    let value = token.content;
    let metaValue = null;
    if (isMeta) {
      metaValue = value.slice(hashIndex + 1) || null;
      value = value.slice(0, hashIndex);
    }

    return new Identifier(value, isMeta, metaValue, loc);
  }
}

export enum BinOp {
  Add,
  Subtract,
  Modulo,
  Equals,
}

export class BinOpExpression {
  public readonly kind = 'BinOp';
  public constructor(
    public readonly lhs: Expression,
    public readonly op: BinOp,
    public readonly rhs: Expression,
    public readonly loc: Loc
  ) {}

  public static fromSequence(
    { loc }: NodeDetails,
    [lhs, op, rhs]: [Expression, Token, Expression]
  ): BinOpExpression {
    return new BinOpExpression(lhs, BinOpExpression.resolveOp(op.content), rhs, loc);
  }

  private static resolveOp(op: string): BinOp {
    switch (op) {
      case '+':
        return BinOp.Add;
      case '-':
        return BinOp.Subtract;
      case '%':
        return BinOp.Modulo;
      case '==':
        return BinOp.Equals;
    }

    throw new Error('Internal error: invalid binary operator ' + op);
  }
}

export class DefDeclaration {
  public readonly kind = 'DefDeclaration';
  public constructor(
    public readonly name: Identifier,
    public readonly value: Expression,
    public readonly loc: Loc
  ) {}

  public static fromSequence(
    { loc }: NodeDetails,
    [, name, , value]: [Token, Identifier, Token, Expression]
  ): DefDeclaration {
    return new DefDeclaration(name, value, loc);
  }
}

interface ScriptDeclaration {
  value: Expression | null;
  meta: SemanticComment | null;
}

export class Script {
  public readonly kind = 'Script';
  public constructor(public readonly declarations: Map<string, ScriptDeclaration>) {}

  public static fromSequence(input: Array<[SemanticComment | null, Declaration]>): Script {
    let declarations = new Map<string, ScriptDeclaration>();
    for (let [meta, declaration] of input) {
      if (declaration.kind === 'DefDeclaration') {
        let { name, value } = declaration;
        declarations.set(name.value, { meta, value });
      } else if (declaration.kind === 'ADTDeclaration') {
        let { name, constructors } = declaration;
        let identityConstructor: ADTConstructor | null = null;
        for (let constructor of constructors) {
          if (constructor.name.value === name.value) {
            identityConstructor = constructor;
          } else {
            declarations.set(constructor.name.value, { meta: null, value: constructor });
          }
        }
        declarations.set(name.value, { meta, value: identityConstructor });
      } else {
        unreachable(declaration);
      }
    }
    return new Script(declarations);
  }
}

export type TextSegment = { kind: 'TextSegment'; content: string };
export type EmbedSegment = { kind: 'EmbedSegment'; content: Expression };
export type ExampleSegment = {
  kind: 'ExampleSegment';
  name: string;
  source: string;
  value: Expression;
};

export type CommentSegment = TextSegment | EmbedSegment | ExampleSegment;
export class SemanticComment {
  public readonly kind = 'SemanticComment';
  public constructor(
    public readonly segments: ReadonlyArray<CommentSegment>,
    public readonly loc: Loc
  ) {}

  public static fromSegments(
    { loc }: NodeDetails,
    [, inputSegments]: [
      commentStart: Token,
      segments: Array<
        | Token
        | { expression: Expression }
        | {
            source: string;
            example: [exampleStart: Token, exampleValue: Expression, exampleEnd: Token];
          }
      >,
      commentEnd: Token
    ]
  ): SemanticComment {
    let segments: Array<CommentSegment> = [];
    let currentText: TextSegment | undefined;

    for (let segment of inputSegments) {
      if ('kind' in segment) {
        if (currentText) {
          currentText.content += segment.content;
        } else {
          currentText = { kind: 'TextSegment', content: segment.content };
        }
      } else {
        if (currentText) {
          segments.push(currentText);
          currentText = undefined;
        }

        if ('expression' in segment) {
          segments.push({ kind: 'EmbedSegment', content: segment.expression });
        } else {
          let [startToken, value, endToken] = segment.example;
          let name = startToken.content.slice(startToken.content.indexOf('#') + 1).trim();
          let source = stripIndent`${segment.source.slice(
            startToken.content.length,
            segment.source.length - endToken.content.length
          )}`;

          segments.push({ kind: 'ExampleSegment', name, value, source });
        }
      }
    }

    if (currentText) {
      segments.push(currentText);
    }

    return new SemanticComment(segments, loc);
  }
}

export class FunctionExpression {
  public readonly kind = 'Function';
  public constructor(
    public readonly source: string,
    public readonly name: string | null,
    public readonly params: ReadonlyArray<Identifier>,
    public readonly body: ReadonlyArray<Expression>,
    public readonly loc: Loc
  ) {}

  public static fromSequence(
    { source, loc }: NodeDetails,
    [, name, params, body]: [Token, Identifier | null, Array<Identifier>, Array<Expression>, Token]
  ): FunctionExpression {
    return new FunctionExpression(source, name?.value ?? null, params, body, loc);
  }
}

export class CallExpression {
  public readonly kind = 'Call';
  public constructor(
    public readonly callee: Expression,
    public readonly args: ReadonlyArray<Expression>,
    public readonly loc: Loc
  ) {}

  public static fromSequence(
    { loc }: NodeDetails,
    [callee, , args]: [Expression, Token, Array<Expression>, Token]
  ): CallExpression {
    return new CallExpression(callee, args, loc);
  }
}

export class ADTDeclaration {
  public readonly kind = 'ADTDeclaration';
  public constructor(
    public readonly name: Identifier,
    public readonly constructors: ReadonlyArray<ADTConstructor>,
    public readonly loc: Loc
  ) {}

  public static fromSequence(
    { loc }: NodeDetails,
    [, name, , variants]: [
      Token,
      Identifier,
      Token,
      Array<[Identifier, Token, Array<Identifier>, Token]>
    ]
  ): ADTDeclaration {
    let structuredVariants = variants.map(([name, , params, rParen]) => {
      return new ADTConstructor(name, params, {
        offset: name.loc.offset,
        length: rParen.offset + rParen.content.length - name.loc.offset,
      });
    });

    return new ADTDeclaration(name, structuredVariants, loc);
  }
}

export class ADTConstructor {
  public readonly kind = 'ADTConstructor';
  public constructor(
    public readonly name: Identifier,
    public readonly params: ReadonlyArray<Identifier>,
    public readonly loc: Loc
  ) {}
}

export class ADTConstructorPattern {
  public readonly kind = 'ADTConstructor';
  public constructor(
    public readonly name: Identifier,
    public readonly params: ReadonlyArray<Pattern>,
    public readonly loc: Loc
  ) {}

  public static fromSequence(
    { loc }: NodeDetails,
    [name, , args]: [Identifier, Token, Array<Pattern>, Token]
  ): ADTConstructorPattern {
    return new ADTConstructorPattern(name, args, loc);
  }
}

export type MatchArm = {
  pattern: Pattern;
  result: Expression;
};

export class MatchExpression {
  public readonly kind = 'Match';
  public constructor(
    public readonly subject: Expression,
    public readonly arms: ReadonlyArray<MatchArm>,
    public readonly loc: Loc
  ) {}

  public static fromSequence(
    { loc }: NodeDetails,
    [, subject, arms]: [Token, Expression, Array<[Token, Pattern, Token, Expression]>, Token]
  ): MatchExpression {
    let matchArms = arms.map(([, pattern, , result]) => ({ pattern, result }));
    return new MatchExpression(subject, matchArms, loc);
  }
}
