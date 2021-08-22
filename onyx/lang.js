///<reference lib="es2020" />
// @ts-check

// handles variable shadowing (and changing variables in parent scopes) basically.
// almost like prototype except setting tries to set in prototype first
// if you want to set a variable in the current scope just do `scope.lookup[name] = value`
class Scope {
  constructor(parent=null, lookup={}) {
    this.parent = parent;
    this.lookup = lookup;
  }
  
  get(k) {
    if (k in this.lookup) { return this.lookup[k]; }
    else if (this.parent) { return this.parent.get(k); }
  }
  
  set(k, v) {
    if (k in this.lookup) { return (this.lookup[k] = v); }
    if (this.parent && this.parent.has(k)) { return this.parent.set(k, v); }
    else { return (this.lookup[k] = v); }
  }
  
  has(k) {
    return k in this.lookup || this.parent && this.parent.has(k);
  }
}

const re = new Map([
  // works best if it's ordered roughly from most to least common
  //  of course, as mentioned in the discord you can switch based on current character to make it even faster
  // y flag = sticky = we can match starting at a certain position
  ['whitespace', /\s+/y],
  ['bracket', /[\[\](){}]/y],
  // should keywords even be a thing tbh
  // stealing these from my other thing because iot's faster
  ['decimal', /\b\d+\.\d+\b/y],
  ['integer', /\b\d+\b/y],
  ['boolean', /\b(?:true|false)\b/y],
  ['null', /\b(?:null)\b/y],
  ['identifier', /\b[a-zA-Z_]\w*\b/y],
  ['string', /"(?:[^\\"]|\\.)*"/y],
  // note: temporarily removed ; and ,
  ['symbol', /[:]/y],
  // no bitwise i guess. also no +=
  ['operator', /(?:>=|==|<=|!=|&&|\|\|)|[<>=+\-*\/%!]/y],
]);

function lex(code) {
  let pos = 0;
  const result = [];
  // like in the parser, consumes a token, returns it and adds to position on success
  // on failure does (and returns) nothing
	function eat(regex) {
    // start matching from current position. required because we have multiple regexes
    regex.lastIndex = pos;
    const match = code.match(regex);
    if (match) {
    	return { value: match[0], start: pos, end: pos += match[0].length };
    } else {
    	return null;
    }
  }
  outer: while (pos < code.length) {
  	for (const [type, regex] of re) {
    	const match = eat(regex);
      if (match) {
        // throw away unwanted tokens
        // for the first example below we won't actually have text comments
        // more of a weird s-expression but not s-expression thing
        // uhm
        // is it working? who knows
      	if (type !== 'whitespace') {
          result.push({ ...match, type: type });
        }
        continue outer;
      }
    }
    throw new Error('unknown token ' + code.slice(pos));
  }
  return result;
}

// precedences
// we don't allow operators to be both prefix and infix
// because that is nasty
// on the down side, that means negative numbers must be, say, 0-100
const precs = {
  '!': 13,
  /* '**': 12, */
  '*': 11, '/': 11, '%': 11,
  '+': 10, '-': 10,
  '==': 9, '!=': 9, '<': 9, '<=': 9, '>': 9, '>=': 9,
  // todo: xor precedence
  /* '&': 6, */
  /* '|': 5, */
  '&&': 4,
  '||': 3,
  '=': 2,
  ',': 1,
  /* ';': 0, */
};
const isNonAssoc = { '=': true, '==': true, '!=': true, '<': true, '<=': true, '>': true, '>=': true };
const isPrefix = { '!': true };
const isRTL = {};

function parse(toks) {
  // current index in toks
  let pos = 0;
  // attempts to consume a token with the given value (string form)
  // on success, returns true and advances position in list of tokens
  // (not stream because i want to backtrack. not that i need to idk)
  // on failure, does nothing and returns false
  function eat(tok) {
    if (pos < toks.length && toks[pos].value === tok) {
      pos += 1;
      return true;
    } else {
    	return false;
    }
  }
  function eatType(type) {
    if (pos < toks.length && toks[pos].type === type) {
      pos += 1;
      // to make it conceptually simpler, we'll be using a lot of tokens directly in the parser
      return { ...toks[pos - 1], start: pos - 1, end: pos };
    } else {
    	return null;
    }
  }
  function directives() {
    const directives = [];
    let directive;
    while ((directive = firstClassComment())) {
      directives.push(directive);
    }
    return directives;
  }
  function type() {
    const start = pos;
    const name = eatType('identifier');
    if (!name) { pos = start; return; }
    return { type: 'type', name: name, syntaxType: 'type' };
    // todo: generics e.g. (an array of integers)
  }
  function argumentDeclarations() {
    const start = pos;
    let error = null;
    if (!eat('[')) { pos = start; return; }
    const args = [];
    while (true) {
      const name = eatType('identifier');
      if (!name) { break; }
      const dirs = directives();
      args.push({ ...name, syntaxType: 'function_parameter', directives: dirs, end: pos });
    }
    if (!eat(']')) { error = 'expected <code>]</code>'; }
    return { type: 'argument_declarations', arguments: args, start: start, end: pos, error: error };
  }
  function arguments_() {
    const start = pos;
    let error;
    if (!eat('[')) { pos = start; return; }
    const args = [];
    let expr;
    while ((expr = expression())) { args.push(expr); }
    if (!eat(']')) { error = 'expected <code>]</code>'; }
    return { type: 'arguments', arguments: args, error: error, start: start, end: pos };
  }
  function simpleExpression() {
    const start = pos;
    // absolutely do not use || like this irl
    const literal = eatType('string') || eatType('integer') || eatType('decimal') || eatType('boolean') || eatType('null');
    if (literal) {
      // const directives_ = directives();
      return literal;
    } else {
      // todo: destructure maybe idk
      let expr = eatType('identifier');
      if (expr) {
        while (true) {
          if (eat('.')) {
            let property = eatType('identifier');
            // todo: parse successfully but error on node?
            let error;
            if (!property) { error = 'expected property name'; }
            // rename type to `property` mostly for highlighting purposes
            expr = { type: 'access', target: expr, property: { ...property, syntaxType: 'property' } };
          } else {
            // [] is both array/dict access and call
            const args = arguments_();
            if (!args) { break; }
            expr = { type: 'function_call', target: expr.type === 'identifier' ? { ...expr, syntaxType: 'function' } : expr, arguments: args };
          }
        }
        return { ...expr, start: start, end: pos };
      }
      // todo: arrays
      // if (eat('{')) {
      //   //
      // }
    }
  }
  function expression() {
    const exprs = [];
    const ops = [];
    // if true, we're expecting prefix or number
    // if false, we're expecting infix
    let expectExpr = true;
    // creates a new expression with the operator and its operands
    function handleOp(op) {
      if (isPrefix[op.value]) {
        const operand = exprs.pop();
        exprs.push({ type: 'prefix', operator: op, operand: operand, start: operand.start, end: operand.end });
      } else {
        const right = exprs.pop();
        const left = exprs.pop();
        exprs.push({ type: 'infix', operator: op, left: left, right: right, start: left.start, end: right.end });
      }
    }
    while (true) {
      const beforeExpr = pos;
      let expr;
      if ((expr = simpleExpression())) {
        if (!expectExpr) {
          pos = beforeExpr;
          break;
        } else { exprs.push(expr); expectExpr = false; }
      } else {
        const op = eatType('operator');
        if (op) {
          if (expectExpr) {
            if (!isPrefix[op.value]) { throw new Error('prefix operator expected, infix operator `' + op.value + '` found'); }
          } else {
            if (isPrefix[op.value]) { throw new Error('infix operator expected, prefix operator `' + op.value + '` found'); }
            expectExpr = true;
          }
          const thisIsRTL = isRTL[op.value];
          const prec = precs[op.value];
          // note: we don't have any rtl but this would roughly be what it looks like
          while (ops.length !== 0 && (thisIsRTL ? prec < precs[ops[ops.length - 1].value] : prec <= precs[ops[ops.length - 1].value])) {
            handleOp(ops.pop());
          }
          const directives_ = directives();
          if (directives_.length) { exprs[exprs.length - 1].directives = directives_; }
          ops.push(op);
        } else { break; }
      }
    }
    while (ops.length) {
      handleOp(ops.pop());
    }
    const directives_ = directives();
    if (directives_.length) { exprs[exprs.length - 1].directives = directives_; }
    // todo: if there are exprs or ops remaining, error?
    // todo: handle directives?
    // return exprs[0];
    return exprs[0] ? { ...exprs[0], end: pos } : null;
  }
  function firstClassComment() {
    const start = pos;
    // basically just fail and reset when we don't get what we expect
    // nice error handling may come later™
    if (!eat('(')) { pos = start; return; }
    const directive = eatType('identifier');
    let directiveType = null;
    let value = null;
    let error = null;
    switch (directive && directive.value) {
      case null: { break; } // it wasn't an identifier. oh no
      case 'if': case 'unless': case 'while': case 'until': {
        directiveType = directive.value;
        const expr = expression();
        value = { condition: expr };
        if (!expr) { error = 'expected condition'; break; }
        break;
      }
      case 'of': {
        // function generic specifier
        directiveType = 'generic';
        const types = [];
        let type_;
        while ((type_ = type())) {
          types.push(type_);
        }
        value = { types: types };
        break;
      }
      case 'call': {
        directiveType = 'set';
        if (!eat('this')) {
          const text = [];
          while (pos < toks.length && toks[pos].value !== ':' && (pos + 1 >= toks.length || toks[pos + 1].value !== ')')) {
            text.push(toks[pos].value); pos += 1;
          }
          error = 'we only know how to `call this` - not `call ' + text.join(' ') + '`';
        }
        const name = eatType('identifier');
        if (!name) { error = '`call this` needs a name, not `' + toks[pos].type + '(' + toks[pos].value + ')`'; break; }
        value = { name: name };
        break;
      }
      case 'a': case 'an': {
        // specifies type
        directiveType = 'type';
        const type_ = type();
        if (!type_) { error = 'you need to specify a type when using the directive `' + directive.value + '`'; }
        value = { valueType: type_ };
        break;
      }
      case 'for': {
        const subdirective = eatType('identifier');
        switch (subdirective.value) {
          case 'example': {
            directiveType = 'example';
            const expr = expression();
            value = { expression: expr };
            break;
          }
          default: {
            const text = subdirective !== null ? [subdirective.value] : [];
            while (toks[pos].value !== ':') {
              text.push(toks[pos].value); pos += 1;
            }
            error = 'we only understand `for example`, not `for ' + text.join(' ') + '`';
            break;
          }
        }
        break;
      }
      case 'returns': {
        // specifies return type
        // bad naming convention, when i have a name conflict
        // i just rename one of the variables to something vaguely fitting
        directiveType = 'return_type';
        const subdirective = eatType('identifier');
        switch (subdirective.value) {
          case 'a': case 'an': {
            const type_ = type();
            if (!type_) { error = 'you need to specify a type when using the directive `' + directive.value + '`'; }
            value = { returnType: type_ };
            break;
          }
          default: {
            const text = subdirective !== null ? [subdirective.value] : [];
            while (toks[pos].value !== ':') {
              text.push(toks[pos].value); pos += 1;
            }
            error = 'we only understand `returns a` and `returns an`, not `returns ' + text.join(' ') + '`';
            break;
          }
        }
        break;
      }
      case 'return': {
      	directiveType = 'return';
        if (!eat('this')) {
          const text = [];
          while (pos < toks.length && toks[pos].value !== ':' && (pos + 1 >= toks.length || toks[pos + 1].value !== ')')) {
            text.push(toks[pos].value); pos += 1;
          }
          error = 'we only know how to `return this` - not `return ' + text.join(' ') + '`';
        }
        break;
      }
      case 'should': {
        const subdirective = eatType('identifier');
        switch (subdirective.value) {
          case 'be': case 'equal': {
            directiveType = 'assert_equal';
            const value_ = expression();
            if (!value_) {
              error = 'no expression to compare with';
              break;
            }
            value = { value: value_ };
            break;
          }
          case 'throw': {
            // i don't want exceptions tbh :(
        		/* let exceptionType = type(); */
            /* return { type: 'directive', dir: 'assert_throw', value: exceptionType }; */
            break;
          }
          // throw errors only when we know backtracking is useless
          default: {
            const text = subdirective !== null ? [subdirective.value] : [];
            while (pos < toks.length && toks[pos].value !== ':' && (pos + 1 >= toks.length || toks[pos + 1].value !== ')')) {
              text.push(toks[pos].value); pos += 1;
            }
            error = 'we only understand `should be`/`should equal` - not `should ' + text.join(' ') + '`';
            break;
          }
        }
        break;
      }
      default: {
        let level = 1;
        while (toks[pos].value !== ')') {
          if (eat('(')) { level += 1; continue; }
          if (eat(')')) {
            level -= 1;
            // un-eat it - this is the final paren
            if (level === 0) { pos -= 1; break; }
            else { continue; }
          }
          pos += 1;
          if (pos >= toks.length) { error = 'you forgot to close a `(` comment with a `)` at ' + toks[start].start + ':' + toks[toks.length - 1].end; break; }
        }
        // todo: turn this into multiple errors maybe
        if (!error) {
          error = 'we don\'t know what directive `' + directive.value + '` is';
        }
        break;
      }
    }
    if (value === undefined) { throw new Error('we don\'t know how to process directive `' + (directive && directive.value) + '`'); }
    let comment = [];
    if (eat(':')) {
      // start of actual ignored comment. eat all tokens until matching )
      // (we support balanced ()!)
      let level = 1;
      // our break condition is inside the loop
      while (true) {
        // todo: pair these up maybe
        // so they can be highlighted together
        if (eat('(')) { comment.push('('); level += 1; continue; }
        if (eat(')')) {
          level -= 1;
          // un-eat it - this is the final paren
          if (level === 0) { pos -= 1; break; }
          else { comment.push(')'); continue; }
        }
        comment.push(toks[pos].value);
        pos += 1;
        if (pos >= toks.length) { error = 'you forgot to close a `(` comment with a `)` at ' + toks[start].start + ':' + toks[toks.length - 1].end; break; }
      }
    }
    if (!eat(')')) { error = 'you need a `)` at ' + toks[pos].start; }
    // naturally, directives don't have their own directives
    return { type: 'directive', directive: directiveType, ...value, comment: comment.length ? comment.join(' ') : null, start: start, end: pos, error: error || null, directives: [] };
  }
  function statements() {
    const start = pos;
    let error = null;
    const dirs = [];
    let comment;
    while ((comment = firstClassComment())) {
      // it's a directive.
      if (comment.directive) { error = 'directives can\'t come before expressions in blocks'; }
      // else skip
      dirs.push(comment);
    }
  	const statements = [];
    let current;
    // note: for now all control flow will be via directives
    // so statements *are* expressions
    while ((current = function_() || expression() || block())) {
    	statements.push(current);
    }
    // no directives - trailing directive would just be associated with the last statement
    return { type: 'statements', statements: statements, start: start, end: pos, directives: dirs, error: error };
  }
  function block() {
    const start = pos;
    let errors = [];
    if (!eat('{')) { pos = start; return; }
  	const result = statements();
    if (!result) { errors.push('expected statements (this shouldn\'t be possible)'); }
    if (!eat('}')) { errors.push('expected <code>}</code>'); }
    const dirs = directives();
    return { type: 'block', statements: result, start: start, end: pos, directives: dirs, error: errors.length ? errors.join(', ') : null };
  }
  function function_() {
    const start = pos;
    let errors = [];
    if (!eat('function')) { pos = start; return; }
    const name = eatType('identifier');
    if (!name) { errors.push('expected function name'); }
    const args = argumentDeclarations();
    if (!args) { errors.push('expected function input list (<code>[first second]</code>)'); }
    const dirs = directives();
    const body = block();
    if (!body) { errors.push('expected function body (a block)'); }
    dirs.push(...directives());
    return { type: 'function_definition', functionKeyword: { type: 'identifier', syntaxType: 'keyword', start: start, end: start + 1 }, name: { ...name, syntaxType: 'function' }, arguments: args, body: body, start: start, end: pos, error: errors.length ? errors.join(', ') : null, directives: dirs };
  }
 
  return statements();
}

// ok break isn't too natural language-y. might need to rename this
const $BREAK = Symbol('BREAK');
const $RETURN = Symbol('RETURN');
// cancels invocations of all later directives
const $SHORT_CIRCUIT = Symbol('SHORT_CIRCUIT');
// probably won't be used but might as well include it
const $THROW = Symbol('THROW');

function replacer(key, value) {
  if (typeof value === 'bigint') {
    return value.toString();
  } else {
    return value;
  }
}

// todo: directives are supposed to apply to the expressions (?) they are on
// todo: attach directives to the right place
// note: we could have nested { type: 'directive', inner: expr }
// but 
function applyDirectives(directives, fn) {
  if (!Array.isArray(directives)) { return fn; }
  for (const directive of directives) {
    const fn_ = fn;
    switch (directive.directive) {
      case 'if': { // if
        const condFn = functionize(directive.condition);
        fn = (scope) => { if (condFn(scope)) { return fn_(scope); } else { scope.set($SHORT_CIRCUIT, null); } }
        break;
      }
      case 'unless': { // unless
        const condFn = functionize(directive.condition);
        fn = (scope) => { if (!condFn(scope)) { return fn_(scope); } else { scope.set($SHORT_CIRCUIT, null); } }
        break;
      }
      case 'while': { // while
        const condFn = functionize(directive.condition);
        fn = (scope) => {
          const newScope = new Scope(scope);
          while (condFn(newScope)) {
            fn_(newScope);
            if (newScope.has($SHORT_CIRCUIT)) { return; }
            if (newScope.has($BREAK)) { return; }
            if (newScope.has($RETURN)) {
              if (newScope.parent !== null) { newScope.parent.set($RETURN, newScope.get($RETURN)); }
              else { throw new Error('cannot return from top level'); }
              return;
            }
          }
          scope.set($SHORT_CIRCUIT, null);
        }
        break;
      }
      case 'until': {
        const condFn = functionize(directive.condition);
        fn = (scope) => { // until
          const newScope = new Scope(scope);
          while (!condFn(newScope)) {
            fn_(newScope);
            if (newScope.has($SHORT_CIRCUIT)) { return; }
            if (newScope.has($BREAK)) { return; }
            if (newScope.has($RETURN)) {
              if (newScope.parent !== null) { newScope.parent.set($RETURN, newScope.get($RETURN)); }
              else { throw new Error('cannot return from top level'); }
              return;
            }
          }
          scope.set($SHORT_CIRCUIT, null);
        }
        break;
      }
      case 'break': {
        // TODO: this doesn't use fn_
        fn = (scope) => { // break
          scope.set($BREAK, null);
          scope.set($SHORT_CIRCUIT, null);
        }
        break;
      }
      case 'return': {
        fn = (scope) => { // return
          const val = fn_(scope);
          if (scope.has($SHORT_CIRCUIT)) { return val; }
          scope.set($RETURN, val);
          scope.set($SHORT_CIRCUIT, null);
        }
        break;
      }
      // if_not, while, while_not
      case 'set': {
        fn = (scope) => { // set
          const val = fn_(scope);
          if (scope.has($SHORT_CIRCUIT)) { return val; }
          return scope.set(directive.name.value, val);
        }
        break;
      }
      case 'example': { // example
        // do nothing for now
        break;
      }
      case 'assert_equal': {
        function deepEquals(a, b) {
          if (typeof a !== typeof b) { return false; }
          else if ((a === null) || (b === null)) { return a === b; }
          else if (typeof a !== 'object') { return a === b; }
          else {
            for (const k in { ...a, ...b }) {
              if (!(k in a) || !(k in b) || !deepEquals(a[k], b[k])) {
                return false;
              }
            }
            return true;
          }
        }
        const expectedFn = functionize(directive.value);
        fn = (scope) => { // assert_equal
          const actual = fn_(scope);
          if (scope.has($SHORT_CIRCUIT)) { return actual; }
          const expected = expectedFn(scope);
          const isEqual = deepEquals(actual, expected);
          console.error('assertion ' + (isEqual ? 'passed' : 'failed') + ': ' + JSON.stringify(actual, replacer) + (isEqual ? ' is ' : ' is not ') + JSON.stringify(expected, replacer) + '\n');
          return actual;
          // throw new Error('assertion error: `' + JSON.stringify(actual) + '` is not `' + JSON.stringify(expected) + '`');
        }
        break;
      }
      case 'return_type': {
        // impl
        break;
      }
      case null: {
        // do nothing
        break;
      }
      default: throw new Error('we don\'t know how to handle directive `' + directive.directive + '`');
    }
    // ehh... this isn't strictly right but oh well
    fn.node = fn_.node;
    fn.parent = fn_;
  }
  const final = (scope) => { // final
    const val = fn(scope);
    delete scope.lookup[$SHORT_CIRCUIT];
    return val;
  };
  final.node = fn.node;
  return final;
}

// not a good idea for real implementations
// but it's convenient and simple to understand (imo)
// will fill this in later
// ok actually
function functionize(node) {
  // if node is null, return a no-op
  let fn;
  // todo
  if (!node) { fn = (scope) => {} }
  else switch (node.type) {
    // copy-pasted because these shouldn't really differ much
    // todo: we will want to be able to convert from int literal to double
    // because literals don't/shouldn't actually have fixed types
    // not in this lanugage at least
    case 'integer': { const value = BigInt(node.value); fn = (scope) => value; break; }
    case 'decimal': { const value = Number(node.value); fn = (scope) => value; break; }
    // the conversion here is non-ideal. ideally id would throw on unknown value
    case 'boolean': { const value = node.value === 'true' ? true : false; fn = (scope) => value; break; }
    case 'null': { fn = (scope) => null; break; }
    case 'string': {
      // return string without quotes
      // and with any \<foo> replaced with just <foo>
      const value = node.value.slice(1, -1).replace(/\\(.)/g, '$1');
      fn = (scope) => value;
      break;
    }
    case 'identifier': {
      const name = node.value;
      fn = (scope) => scope.get(name);
      break;
    }
    case 'infix': { // { left: expression, right: expression }
      const leftFn = functionize(node.left), rightFn = functionize(node.right);
      switch (node.operator.value) {
        case '+': { fn = (scope) => leftFn(scope) + rightFn(scope); break; }
        case '-': { fn = (scope) => leftFn(scope) - rightFn(scope); break; }
        case '*': { fn = (scope) => leftFn(scope) * rightFn(scope); break; }
        case '/': { fn = (scope) => leftFn(scope) / rightFn(scope); break; }
        case '%': { fn = (scope) => leftFn(scope) % rightFn(scope); break; }
        // :D shortcircuiting for free
        case '&&': { fn = (scope) => leftFn(scope) && rightFn(scope); break; }
        case '||': { fn = (scope) => leftFn(scope) || rightFn(scope); break; }
        case '==': { fn = (scope) => leftFn(scope) === rightFn(scope); break; }
        case '!=': { fn = (scope) => leftFn(scope) !== rightFn(scope); break; }
        case '<': { fn = (scope) => leftFn(scope) < rightFn(scope); break; }
        case '<=': { fn = (scope) => leftFn(scope) <= rightFn(scope); break; }
        case '>': { fn = (scope) => leftFn(scope) > rightFn(scope); break; }
        case '>=': { fn = (scope) => leftFn(scope) >= rightFn(scope); break; }
        default: throw new Error('unrecognized in operator ' + node.operator.value);
      }
      break;
    }
    case 'prefix': { // { operand: expression }
      // TODO: naming
      const operandFn = functionize(node.operand);
      switch (node.operator.value) {
        case '!': { fn = (scope) => !operandFn(scope); break; }
        default: throw new Error('unrecognized prefix operator ' + node.operator.value);
      }
      break;
    }
    case 'access': { // { target: expression, property: identifier }
      const [targetFn] = [node.target].map(functionize);
      const prop = node.property.value;
      fn = (scope) => targetFn(scope)[prop];
      break;
    }
    case 'arguments': { // { arguments: expression[] }
      const argFns = node.arguments.map(functionize);
      // fn = (scope) => {
      //   let result = [];
      //   for (const argFn of argFns) {
      //     const result = argFn(scope);
      //     if (scope.has($THROW)) { if (scope.parent) { scope.parent.set($THROW, scope.get($THROW)); } return; }
      //     result.push(result);
      //   }
      //   return result;
      // }
      fn = (scope) => argFns.map((fn) => fn(scope));
      break;
    }
    case 'function_call': { // { target: expression, arguments: expression[] }
      const [targetFn, argsFn] = [node.target, node.arguments].map(functionize);
      fn = (scope) => targetFn(scope)(...argsFn(scope));
      break;
    }
    case 'statements': {
      const stmtFns = node.statements.map(functionize);
      fn = (scope) => {
        const newScope = new Scope(scope);
        for (const stmtFn of stmtFns) {
          stmtFn(newScope);
          if (newScope.has($BREAK)) { return newScope.get($BREAK); }
          if (newScope.has($RETURN)) {
          	if (newScope.parent !== null) { newScope.parent.set($RETURN, newScope.get($RETURN)); }
            else { throw new Error('cannot return from top level'); }
            return;
          }
        	// if (newScope.has($THROW)) { if (newScope.parent) { newScope.parent.set($THROW, newScope.get($THROW)); } return; }
        }
      };
      break;
    }
    case 'block': {
      fn = functionize(node.statements);
      break;
    }
    case 'function_definition': { // { arguments: identifier[], body: block }
      // todo: we don't execute args but we should still use the directives to typecheck
      // const argFns = node.args.map(functionize);
      const bodyFn = functionize(node.body);
      fn = (scope) => {
        scope.set(node.name.value, (...args) => {
          const newScope = new Scope(scope);
          for (let i = 0; i < args.length; i += 1) {
            newScope.set(node.arguments.arguments[i].value, args[i]);
          }
          bodyFn(newScope);
          // irl avoiding single line { foo } is nice
          if (newScope.has($RETURN)) { return newScope.get($RETURN); }
          // if (newScope.has($THROW)) { if (newScope.parent) { newScope.parent.set($THROW, newScope.get($THROW)); } return; }
        });
      };
      break;
    }
    default: throw new Error('unknown node type `' + node.type + '`');
  }
  fn.node = node;
  fn = applyDirectives(node.directives, fn);
  return fn;
}

function print(value) {
  console.log(value);
}

const BUILTINS_SCOPE = new Scope(null, {
  'print': print,
  'println': (value) => print(value + '\n'),
  // todo: varargs are kinda nasty so i'll think about it later
  /* 'concat': (...args) => args.join(''), */
});
