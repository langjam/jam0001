import { unreachable } from '../utils';
import {
  ADTConstructor,
  BooleanLiteral,
  CallExpression,
  CommentSegment,
  Expression,
  FunctionExpression,
  Identifier,
  Loc,
  MatchExpression,
  NumberLiteral,
  Pattern,
  Script,
  SemanticComment,
} from '../parser/ast';

export type Value =
  | BooleanValue
  | NumberValue
  | CommentValue
  | VoidValue
  | FunctionValue
  | NativeFunctionValue
  | ADTInstanceValue;

export class VoidValue {
  public readonly kind = 'Void';
}

export class CommentValue {
  public readonly kind = 'Comment';
  public constructor(
    public readonly segments: ReadonlyArray<CommentSegment>,
    public readonly examples: Map<string, Value>
  ) {}
}

export class NumberValue {
  public readonly kind = 'Number';
  public constructor(public readonly value: number) {}
}

export class BooleanValue {
  public readonly kind = 'Boolean';
  public constructor(public readonly value: boolean) {}
}

export class FunctionValue {
  public readonly kind = 'Function';
  public constructor(
    public readonly source: string,
    public readonly name: string | null,
    public readonly params: ReadonlyArray<string>,
    public readonly body: ReadonlyArray<Expression>,
    public readonly scope: Scope
  ) {}
}

export class NativeFunctionValue {
  public readonly kind = 'NativeFunction';
  public constructor(
    public readonly name: string,
    public readonly params: ReadonlyArray<string>,
    public readonly body: (
      this: NativeFunctionValue,
      scope: Scope,
      args: ReadonlyArray<Value>
    ) => Value
  ) {}
}

export class ADTInstanceValue {
  public readonly kind = 'ADTInstance';
  public constructor(
    public readonly adtConstructor: NativeFunctionValue,
    public readonly args: ReadonlyArray<Value>
  ) {}
}

export class Scope {
  private readonly values = new Map<string, Value>();
  private readonly meta = new Map<string, CommentValue>();

  public constructor(private readonly parent: Scope | null = null) {}

  public clear(): void {
    this.values.clear();
    this.meta.clear();
  }

  public define(name: string, value: Value): void {
    this.values.set(name, value);
  }

  public resolve(name: string): Value | undefined {
    return this.values.get(name) ?? this.parent?.resolve(name);
  }

  public defineMeta(name: string, value: CommentValue): void {
    this.meta.set(name, value);
  }

  public resolveMeta(name: string): CommentValue | undefined {
    return this.meta.get(name) ?? this.parent?.resolveMeta(name);
  }

  public resolveExample(name: string, specifier: string): Value | undefined {
    return this.resolveMeta(name)?.examples.get(specifier);
  }
}

export type Definition = {
  value?: Value;
  meta?: CommentValue;
};

export class Evaluator {
  private readonly globalScope = new Scope();

  public constructor(
    private readonly definitions: Record<string, { value?: Value; meta?: CommentValue }> = {}
  ) {
    this.registerDefinitions();
  }

  public reset(): void {
    this.globalScope.clear();
    this.registerDefinitions();
  }

  public execute(script: Script): void {
    for (let [name, { meta, value }] of script.declarations.entries()) {
      if (value) {
        this.globalScope.define(name, this.evaluate(value, this.globalScope));
      }

      if (meta) {
        this.globalScope.defineMeta(name, this.evaluateComment(meta, this.globalScope));
      }
    }
  }

  public evaluate(expression: Expression | SemanticComment, scope = this.globalScope): Value {
    if (expression.kind === 'Number') {
      return this.evaluateNumber(expression);
    } else if (expression.kind === 'Boolean') {
      return this.evaluateBoolean(expression);
    } else if (expression.kind === 'Identifier') {
      return this.evaluateIdentifier(expression, scope);
    } else if (expression.kind === 'SemanticComment') {
      return this.evaluateComment(expression, scope);
    } else if (expression.kind === 'Function') {
      return this.evaluateFunction(expression, scope);
    } else if (expression.kind === 'Call') {
      return this.evaluateCall(expression, scope);
    } else if (expression.kind == 'ADTConstructor') {
      return this.evaluateConstructor(expression);
    } else if (expression.kind === 'Match') {
      return this.evaluateMatch(expression, scope);
    }

    return unreachable(expression);
  }

  private registerDefinitions(): void {
    for (let [name, { value, meta }] of Object.entries(this.definitions)) {
      if (value) {
        this.globalScope.define(name, value);
      }

      if (meta) {
        this.globalScope.defineMeta(name, meta);
      }
    }
  }

  private evaluateNumber(expr: NumberLiteral): NumberValue {
    return new NumberValue(expr.value);
  }

  private evaluateBoolean(expr: BooleanLiteral): BooleanValue {
    return new BooleanValue(expr.value);
  }

  private evaluateIdentifier(expr: Identifier, scope: Scope): Value {
    let value = expr.isMeta
      ? expr.metaValue
        ? scope.resolveExample(expr.value, expr.metaValue)
        : scope.resolveMeta(expr.value)
      : scope.resolve(expr.value);

    if (!value) {
      throw new ReferenceError(expr);
    }

    return value;
  }

  private evaluateComment(expr: SemanticComment, scope: Scope): CommentValue {
    let examples = new Map<string, Value>();
    for (let segment of expr.segments) {
      if (segment.kind === 'ExampleSegment') {
        examples.set(segment.name, this.evaluate(segment.value, scope));
      }
    }
    return new CommentValue(expr.segments, examples);
  }

  private evaluateFunction(expr: FunctionExpression, scope: Scope): FunctionValue {
    let params = expr.params.map((identifier) => identifier.value);
    return new FunctionValue(expr.source, expr.name, params, expr.body, scope);
  }

  private evaluateCall(expr: CallExpression, outerScope: Scope): Value {
    let callee = this.evaluate(expr.callee, outerScope);
    if (callee.kind !== 'Function' && callee.kind !== 'NativeFunction') {
      throw new TypeError(expr.callee.loc, `Tried to call a non-function value`);
    }

    if (expr.args.length !== callee.params.length) {
      throw new TypeError(expr.loc, `Tried to call a function with the wrong number of arguments`);
    }

    let args = expr.args.map((arg) => this.evaluate(arg, outerScope));
    if (callee.kind === 'NativeFunction') {
      return callee.body(outerScope, args);
    }

    let innerScope = new Scope(callee.scope);
    if (callee.name) {
      innerScope.define(callee.name, callee);
    }

    for (let i = 0; i < args.length; i++) {
      innerScope.define(callee.params[i], args[i]);
    }

    let value: Value | undefined;
    for (let expression of callee.body) {
      value = this.evaluate(expression, innerScope);
    }
    return value ?? new VoidValue();
  }

  private evaluateConstructor(expr: ADTConstructor): NativeFunctionValue {
    return new NativeFunctionValue(
      expr.name.value,
      expr.params.map((param) => param.value),
      function (scope, args) {
        return new ADTInstanceValue(this, args);
      }
    );
  }

  private evaluateMatch(expr: MatchExpression, outerScope: Scope): Value {
    let subject = this.evaluate(expr.subject, outerScope);
    for (let arm of expr.arms) {
      let bindings = this.considerPattern(arm.pattern, subject, outerScope);
      if (bindings) {
        let innerScope = new Scope(outerScope);
        for (let [name, value] of bindings) {
          innerScope.define(name, value);
        }
        return this.evaluate(arm.result, innerScope);
      }
    }
    throw new MatchError(expr.loc);
  }

  private considerPattern(
    pattern: Pattern,
    value: Value,
    scope: Scope
  ): Array<[string, Value]> | null {
    if (pattern.kind === 'Number' && value.kind === 'Number' && pattern.value === value.value) {
      return [];
    } else if (
      pattern.kind === 'Boolean' &&
      value.kind === 'Boolean' &&
      pattern.value === value.value
    ) {
      return [];
    } else if (pattern.kind === 'Identifier') {
      if (pattern.value === '_') {
        return [];
      } else {
        return [[pattern.value, value]];
      }
    } else if (pattern.kind === 'ADTConstructor' && value.kind === 'ADTInstance') {
      let patternConstructor = scope.resolve(pattern.name.value);
      if (
        patternConstructor === value.adtConstructor &&
        pattern.params.length === value.args.length
      ) {
        let bindings: Array<[string, Value]> = [];
        for (let i = 0; i < value.args.length; i++) {
          let newBindings = this.considerPattern(pattern.params[i], value.args[i], scope);
          if (newBindings === null) {
            return null;
          }
          bindings.push(...newBindings);
        }
        return bindings;
      }
    }
    return null;
  }
}

class ReferenceError extends Error {
  public readonly loc: Loc;

  public constructor(identifier: Identifier) {
    super(`Reference to unbound identifier ${identifier}`);
    this.loc = identifier.loc;
  }
}

class TypeError extends Error {
  public constructor(public readonly loc: Loc, message: string) {
    super(message);
  }
}

class MatchError extends Error {
  public constructor(public readonly loc: Loc) {
    super('Inexhaustive `match` statement failed');
  }
}
