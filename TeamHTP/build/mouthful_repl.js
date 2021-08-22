//
//  Lark.js stand-alone parser
//===============================
"use strict";
/**
    This is the main entrypoint into the generated Lark parser.

  @param {object} options An object with the following optional properties:

      - transformer: an object of {rule: callback}, or an instance of Transformer
      - propagate_positions (bool): should all tree nodes calculate line/column info?
      - tree_class (Tree): a class that extends Tree, to be used for creating the parse tree.
      - debug (bool): in case of error, should the parser output debug info to the console?

  @returns {Lark} an object which provides the following methods:

    - parse
    - parse_interactive
    - lex

*/
function get_parser(options = {}) {
    if (options.transformer &&
        options.transformer.constructor.name === "object") {
        options.transformer = Transformer.fromObj(options.transformer);
    }
    return Lark._load_from_dict({ data: DATA, memo: MEMO, ...options });
}
const NO_VALUE = {};
class _Decoratable {
}
//
//   Implementation of Scanner + module emulation for Python's stdlib re
// -------------------------------------------------------------------------
const re = {
    escape(string) {
        // See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#escaping
        return string.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"); // $& means the whole matched string
    },
    compile(regex, flags) {
        // May throw re.error
        return new RegExp(regex, flags);
    },
    error: SyntaxError,
};
function _get_match(re_, regexp, s, flags) {
    const m = re_.compile(regexp, flags).exec(s);
    if (m != null)
        return m[0];
}
class Scanner {
    constructor(terminals, g_regex_flags, re_, use_bytes, match_whole = false) {
        this.terminals = terminals;
        this.g_regex_flags = g_regex_flags;
        this.re_ = re_;
        this.use_bytes = use_bytes;
        this.match_whole = match_whole;
        this.allowed_types = new Set(this.terminals.map((t) => t.name));
        this._regexps = this._build_mres(terminals);
    }
    _build_mres(terminals) {
        // TODO deal with priorities!
        let postfix = this.match_whole ? "$" : "";
        let patterns_by_flags = segment_by_key(terminals, (t) => t.pattern.flags.join(""));
        let regexps = [];
        for (let [flags, patterns] of patterns_by_flags) {
            const pattern = patterns
                .map((t) => `(?<${t.name}>${t.pattern.to_regexp() + postfix})`)
                .join("|");
            regexps.push(new RegExp(pattern, this.g_regex_flags + flags + "y"));
        }
        return regexps;
    }
    match(text, pos) {
        for (const re of this._regexps) {
            re.lastIndex = pos;
            let m = re.exec(text);
            if (m) {
                // Find group. Ugly hack, but javascript is forcing my hand.
                let group = null;
                for (let [k, v] of Object.entries(m.groups)) {
                    if (v) {
                        group = k;
                        break;
                    }
                }
                return [m[0], group];
            }
        }
    }
}
//
//  Start of library code
// --------------------------
import util from "util";
class ABC {
}
const NotImplemented = {};
function dict_items(d) {
    return Object.entries(d);
}
function dict_keys(d) {
    return Object.keys(d);
}
function dict_values(d) {
    return Object.values(d);
}
function dict_pop(d, key) {
    if (key === undefined) {
        key = Object.keys(d)[0];
    }
    let value = d[key];
    delete d[key];
    return value;
}
function dict_get(d, key, otherwise = null) {
    return d[key] || otherwise;
}
function dict_update(self, other) {
    if (self.constructor.name === "Map") {
        for (const [k, v] of dict_items(other)) {
            self.set(k, v);
        }
    }
    else {
        for (const [k, v] of dict_items(other)) {
            self[k] = v;
        }
    }
}
function make_constructor(cls) {
    return function () {
        return new cls(...arguments);
    };
}
function range(start, end) {
    if (end === undefined) {
        end = start;
        start = 0;
    }
    res = [];
    for (let i = start; i < end; i++)
        res.push(i);
    return res;
}
function format(s) {
    let counter = 0;
    let args = [...arguments].slice(1);
    return s.replace(/%([sr])/g, function () {
        const t = arguments[1];
        const item = args[counter++];
        if (t === "r") {
            return util
                ? util.inspect(item, false, null, true)
                : JSON.stringify(item, null, 0);
        }
        else {
            return item;
        }
    });
}
function union(setA, setB) {
    let _union = new Set(setA);
    for (const elem of setB) {
        _union.add(elem);
    }
    return _union;
}
function intersection(setA, setB) {
    let _intersection = new Set();
    for (const elem of setB) {
        if (setA.has(elem)) {
            _intersection.add(elem);
        }
    }
    return _intersection;
}
function dict(d) {
    return { ...d };
}
function bool(x) {
    return !!x;
}
function new_object(cls) {
    return Object.create(cls.prototype);
}
function copy(obj) {
    if (typeof obj == "object") {
        let empty_clone = Object.create(Object.getPrototypeOf(obj));
        return Object.assign(empty_clone, obj);
    }
    return obj;
}
function map_pop(key) {
    let value = this.get(key);
    this.delete(key);
    return value;
}
function hash(x) {
    return x;
}
function tuple(x) {
    return x;
}
function frozenset(x) {
    return new Set(x);
}
function is_dict(x) {
    return x && x.constructor.name === "Object";
}
function is_array(x) {
    return x && x.constructor.name === "Array";
}
function callable(x) {
    return typeof x === "function";
}
function* enumerate(it, start = 0) {
    // Taken from: https://stackoverflow.com/questions/34336960/what-is-the-es6-equivalent-of-python-enumerate-for-a-sequence
    let i = start;
    for (const x of it) {
        yield [i++, x];
    }
}
function any(lst) {
    for (const item of lst) {
        if (item) {
            return true;
        }
    }
    return false;
}
function all(lst) {
    for (const item of lst) {
        if (!item) {
            return false;
        }
    }
    return true;
}
function filter(pred, lst) {
    return lst.filter(pred || bool);
}
function partial(f) {
    let args = [...arguments].slice(1);
    return function () {
        return f(...args, ...arguments);
    };
}
class EOFError extends Error {
}
function last_item(a) {
    return a[a.length - 1];
}
function callable_class(cls) {
    return function () {
        let inst = new cls(...arguments);
        return inst.__call__.bind(inst);
    };
}
function list_repeat(list, count) {
    return Array.from({ length: count }, () => list).flat();
}
function isupper(a) {
    return /^[A-Z]*$/.test(a);
}
function rsplit(s, delimiter, limit) {
    const arr = s.split(delimiter);
    return limit ? arr.splice(-limit - 1) : arr;
}
function str_count(s, substr) {
    let re = new RegExp(substr, "g");
    return (s.match(re) || []).length;
}
function list_count(list, elem) {
    let count = 0;
    for (const e of list) {
        if (e === elem) {
            count++;
        }
    }
    return count;
}
function isSubset(subset, set) {
    for (let elem of subset) {
        if (!set.has(elem)) {
            return false;
        }
    }
    return true;
}
function* segment_by_key(a, key) {
    let buffer = [];
    let last_k = null;
    for (const item of a) {
        const k = key(item);
        if (last_k && k != last_k) {
            yield [last_k, buffer];
            buffer = [];
        }
        buffer.push(item);
        last_k = k;
    }
    yield [last_k, buffer];
}
// --------------------------
//  End of library code
//
//
// Exceptions
//
class LarkError extends Error {
}
class ConfigurationError extends LarkError {
}
function assert_config(value, options, msg = "Got %r, expected one of %s") {
    if (!options.includes(value)) {
        throw new ConfigurationError(format(msg, value, options));
    }
}
class GrammarError extends LarkError {
}
class ParseError extends LarkError {
}
class LexError extends LarkError {
}
/**
  UnexpectedInput Error.

    Used as a base class for the following exceptions:

    - ``UnexpectedToken``: The parser received an unexpected token
    - ``UnexpectedCharacters``: The lexer encountered an unexpected string

    After catching one of these exceptions, you may call the following helper methods to create a nicer error message.

*/
class UnexpectedInput extends LarkError {
    /**
      Returns a pretty string pinpointing the error in the text,
          with span amount of context characters around it.
  
          Note:
              The parser doesn't hold a copy of the text it has to parse,
              so you have to provide it again
  
    */
    get_context(text, span = 40) {
        let after, before;
        let pos = this.pos_in_stream;
        let start = max(pos - span, 0);
        let end = pos + span;
        if (!(text instanceof bytes)) {
            before = last_item(rsplit(text.slice(start, pos), "\n", 1));
            after = text.slice(pos, end).split("\n", 1)[0];
            return before + after + "\n" + " " * before.expandtabs().length + "^\n";
        }
        else {
            before = last_item(rsplit(text.slice(start, pos), "\n", 1));
            after = text.slice(pos, end).split("\n", 1)[0];
            return (before +
                after +
                "\n" +
                " " * before.expandtabs().length +
                "^\n").decode("ascii", "backslashreplace");
        }
    }
    /**
      Allows you to detect what's wrong in the input text by matching
          against example errors.
  
          Given a parser instance and a dictionary mapping some label with
          some malformed syntax examples, it'll return the label for the
          example that bests matches the current error. The function will
          iterate the dictionary until it finds a matching error, and
          return the corresponding value.
  
          For an example usage, see `examples/error_reporting_lalr.py`
  
          Parameters:
              parse_fn: parse function (usually ``lark_instance.parse``)
              examples: dictionary of ``{'example_string': value}``.
              use_accepts: Recommended to call this with ``use_accepts=True``.
                  The default is ``False`` for backwards compatibility.
  
    */
    match_examples(parse_fn, examples, token_type_match_fallback = false) {
        if (is_dict(examples)) {
            examples = dict_items(examples);
        }
        let candidate = [null, false];
        for (const [i, [label, example]] of enumerate(examples)) {
            for (const [j, malformed] of enumerate(example)) {
                try {
                    parse_fn(malformed);
                }
                catch (ut) {
                    if (ut instanceof UnexpectedInput) {
                        if (ut.state.eq(this.state)) {
                            if (ut.token === this.token) {
                                return label;
                            }
                            if (token_type_match_fallback) {
                                // Fallback to token types match
                                if (ut.token.type === this.token.type &&
                                    !last_item(candidate)) {
                                    candidate = [label, true];
                                }
                            }
                            if (candidate[0] === null) {
                                candidate = [label, false];
                            }
                        }
                    }
                    else {
                        throw ut;
                    }
                }
            }
        }
        return candidate[0];
    }
    _format_expected(expected) {
        let d;
        if (this._terminals_by_name) {
            d = this._terminals_by_name;
            expected = expected.map((t_name) => t_name in d ? d[t_name].user_repr() : t_name);
        }
        return format("Expected one of: \n\t* %s\n", expected.join("\n\t* "));
    }
}
class UnexpectedEOF extends UnexpectedInput {
    constructor(expected, state = null, terminals_by_name = null) {
        super();
        this.expected = expected;
        this.state = state;
        this.token = new Token("<EOF>", "");
        // , line=-1, column=-1, pos_in_stream=-1)
        this.pos_in_stream = -1;
        this.line = -1;
        this.column = -1;
        this._terminals_by_name = terminals_by_name;
    }
}
class UnexpectedCharacters extends UnexpectedInput {
    constructor({ seq, lex_pos, line, column, allowed = null, considered_tokens = null, state = null, token_history = null, terminals_by_name = null, considered_rules = null, } = {}) {
        super();
        // TODO considered_tokens and allowed can be figured out using state
        this.line = line;
        this.column = column;
        this.pos_in_stream = lex_pos;
        this.state = state;
        this._terminals_by_name = terminals_by_name;
        this.allowed = allowed;
        this.considered_tokens = considered_tokens;
        this.considered_rules = considered_rules;
        this.token_history = token_history;
        this.char = seq[lex_pos];
        // this._context = this.get_context(seq);
    }
}
/**
  An exception that is raised by the parser, when the token it received
    doesn't match any valid step forward.

    The parser provides an interactive instance through `interactive_parser`,
    which is initialized to the point of failture, and can be used for debugging and error handling.

    see: ``InteractiveParser``.

*/
class UnexpectedToken extends UnexpectedInput {
    constructor({ token, expected, considered_rules = null, state = null, interactive_parser = null, terminals_by_name = null, token_history = null, } = {}) {
        super();
        // TODO considered_rules and expected can be figured out using state
        this.line = (token && token["line"]) || "?";
        this.column = (token && token["column"]) || "?";
        this.pos_in_stream = (token && token["start_pos"]) || null;
        this.state = state;
        this.token = token;
        this.expected = expected;
        // XXX deprecate? `accepts` is better
        this._accepts = NO_VALUE;
        this.considered_rules = considered_rules;
        this.interactive_parser = interactive_parser;
        this._terminals_by_name = terminals_by_name;
        this.token_history = token_history;
    }
    get accepts() {
        if (this._accepts === NO_VALUE) {
            this._accepts =
                this.interactive_parser && this.interactive_parser.accepts();
        }
        return this._accepts;
    }
}
/**
  VisitError is raised when visitors are interrupted by an exception

    It provides the following attributes for inspection:
    - obj: the tree node or token it was processing when the exception was raised
    - orig_exc: the exception that cause it to fail

*/
class VisitError extends LarkError {
    constructor(rule, obj, orig_exc) {
        let message = format('Error trying to process rule "%s":\n\n%s', rule, orig_exc);
        super(message);
        this.obj = obj;
        this.orig_exc = orig_exc;
    }
}
//
// Utils
//
function classify(seq, key = null, value = null) {
    let k, v;
    let d = new Map();
    for (const item of seq) {
        k = key !== null ? key(item) : item;
        v = value !== null ? value(item) : item;
        if (d.has(k)) {
            d.get(k).push(v);
        }
        else {
            d.set(k, [v]);
        }
    }
    return d;
}
function _deserialize(data, namespace, memo) {
    let class_;
    if (is_dict(data)) {
        if ("__type__" in data) {
            // Object
            class_ = namespace[data["__type__"]];
            return class_.deserialize(data, memo);
        }
        else if ("@" in data) {
            return memo[data["@"]];
        }
        return Object.fromEntries(dict_items(data).map(([key, value]) => [
            key,
            _deserialize(value, namespace, memo),
        ]));
    }
    else if (is_array(data)) {
        return data.map((value) => _deserialize(value, namespace, memo));
    }
    return data;
}
/**
  Safe-ish serialization interface that doesn't rely on Pickle

    Attributes:
        __serialize_fields__ (List[str]): Fields (aka attributes) to serialize.
        __serialize_namespace__ (list): List of classes that deserialization is allowed to instantiate.
                                        Should include all field types that aren't builtin types.

*/
class Serialize {
    static deserialize(data, memo) {
        const cls = this;
        let namespace = (cls && cls["__serialize_namespace__"]) || [];
        namespace = Object.fromEntries(namespace.map((c) => [c.name, c]));
        let fields = cls && cls["__serialize_fields__"];
        if ("@" in data) {
            return memo[data["@"]];
        }
        let inst = new_object(cls);
        for (const f of fields) {
            if (data && f in data) {
                inst[f] = _deserialize(data[f], namespace, memo);
            }
            else {
                throw new KeyError("Cannot find key for class", cls, e);
            }
        }
        if ("_deserialize" in inst) {
            inst._deserialize();
        }
        return inst;
    }
}
/**
  A version of serialize that memoizes objects to reduce space
*/
class SerializeMemoizer extends Serialize {
    static get __serialize_fields__() {
        return ["memoized"];
    }
    constructor(types_to_memoize) {
        super();
        this.types_to_memoize = tuple(types_to_memoize);
        this.memoized = new Enumerator();
    }
    in_types(value) {
        return value instanceof this.types_to_memoize;
    }
    serialize() {
        return _serialize(this.memoized.reversed(), null);
    }
    static deserialize(data, namespace, memo) {
        const cls = this;
        return _deserialize(data, namespace, memo);
    }
}
//
// Tree
//
class Meta {
    constructor() {
        this.empty = true;
    }
}
/**
  The main tree class.

    Creates a new tree, and stores "data" and "children" in attributes of the same name.
    Trees can be hashed and compared.

    Parameters:
        data: The name of the rule or alias
        children: List of matched sub-rules and terminals
        meta: Line & Column numbers (if ``propagate_positions`` is enabled).
            meta attributes: line, column, start_pos, end_line, end_column, end_pos

*/
class Tree {
    constructor(data, children, meta = null) {
        this.data = data;
        this.children = children;
        this._meta = meta;
    }
    get meta() {
        if (this._meta === null) {
            this._meta = new Meta();
        }
        return this._meta;
    }
    repr() {
        return format("Tree(%r, %r)", this.data, this.children);
    }
    _pretty_label() {
        return this.data;
    }
    _pretty(level, indent_str) {
        if (this.children.length === 1 && !(this.children[0] instanceof Tree)) {
            return [
                indent_str * level,
                this._pretty_label(),
                "\t",
                format("%s", this.children[0]),
                "\n",
            ];
        }
        let l = [indent_str * level, this._pretty_label(), "\n"];
        for (const n of this.children) {
            if (n instanceof Tree) {
                l.push(...n._pretty(level + 1, indent_str));
            }
            else {
                l.push(...[indent_str * (level + 1), format("%s", n), "\n"]);
            }
        }
        return l;
    }
    /**
      Returns an indented string representation of the tree.
  
          Great for debugging.
  
    */
    pretty(indent_str = "  ") {
        return this._pretty(0, indent_str).join("");
    }
    eq(other) {
        if (other &&
            this &&
            other &&
            this &&
            other.children &&
            this.children &&
            other.data &&
            this.data) {
            return this.data === other.data && this.children === other.children;
        }
        else {
            return false;
        }
    }
    /**
      Depth-first iteration.
  
          Iterates over all the subtrees, never returning to the same node twice (Lark's parse-tree is actually a DAG).
  
    */
    iter_subtrees() {
        let queue = [this];
        let subtrees = new Map();
        for (const subtree of queue) {
            subtrees.set(subtree, subtree);
            queue.push(...[...subtree.children]
                .reverse()
                .filter((c) => c instanceof Tree && !subtrees.has(c))
                .map((c) => c));
        }
        queue = undefined;
        return [...subtrees.values()].reverse();
    }
    /**
      Returns all nodes of the tree that evaluate pred(node) as true.
    */
    find_pred(pred) {
        return filter(pred, this.iter_subtrees());
    }
    /**
      Returns all nodes of the tree whose data equals the given data.
    */
    find_data(data) {
        return this.find_pred((t) => t.data === data);
    }
    /**
      Return all values in the tree that evaluate pred(value) as true.
  
          This can be used to find all the tokens in the tree.
  
          Example:
              >>> all_tokens = tree.scan_values(lambda v: isinstance(v, Token))
  
    */
    *scan_values(pred) {
        for (const c of this.children) {
            if (c instanceof Tree) {
                for (const t of c.scan_values(pred)) {
                    yield t;
                }
            }
            else {
                if (pred(c)) {
                    yield c;
                }
            }
        }
    }
    /**
      Breadth-first iteration.
  
          Iterates over all the subtrees, return nodes in order like pretty() does.
  
    */
    *iter_subtrees_topdown() {
        let node;
        let stack = [this];
        while (stack.length) {
            node = stack.pop();
            if (!(node instanceof Tree)) {
                continue;
            }
            yield node;
            for (const n of [...node.children].reverse()) {
                stack.push(n);
            }
        }
    }
    copy() {
        return type(this)(this.data, this.children);
    }
    set(data, children) {
        this.data = data;
        this.children = children;
    }
}
//
// Visitors
//
/**
  When raising the Discard exception in a transformer callback,
    that node is discarded and won't appear in the parent.

*/
class Discard extends Error {
}
/**
  Transformers visit each node of the tree, and run the appropriate method on it according to the node's data.

    Methods are provided by the user via inheritance, and called according to ``tree.data``.
    The returned value from each method replaces the node in the tree structure.

    Transformers work bottom-up (or depth-first), starting with the leaves and ending at the root of the tree.
    Transformers can be used to implement map & reduce patterns. Because nodes are reduced from leaf to root,
    at any point the callbacks may assume the children have already been transformed (if applicable).

    ``Transformer`` can do anything ``Visitor`` can do, but because it reconstructs the tree,
    it is slightly less efficient.

    All these classes implement the transformer interface:

    - ``Transformer`` - Recursively transforms the tree. This is the one you probably want.
    - ``Transformer_InPlace`` - Non-recursive. Changes the tree in-place instead of returning new instances
    - ``Transformer_InPlaceRecursive`` - Recursive. Changes the tree in-place instead of returning new instances

    Parameters:
        visit_tokens (bool, optional): Should the transformer visit tokens in addition to rules.
                                       Setting this to ``False`` is slightly faster. Defaults to ``True``.
                                       (For processing ignored tokens, use the ``lexer_callbacks`` options)

    NOTE: A transformer without methods essentially performs a non-memoized partial deepcopy.

*/
class Transformer extends _Decoratable {
    static get __visit_tokens__() {
        return true;
    }
    // For backwards compatibility
    constructor(visit_tokens = true) {
        super();
        this.__visit_tokens__ = visit_tokens;
    }
    static fromObj(obj, ...args) {
        class _T extends this {
        }
        for (let [k, v] of Object.entries(obj)) {
            _T.prototype[k] = v;
        }
        return new _T(...args);
    }
    _call_userfunc(tree, new_children = null) {
        let f, wrapper;
        // Assumes tree is already transformed
        let children = new_children !== null ? new_children : tree.children;
        if (tree && tree.data && this && this[tree.data]) {
            f = this && this[tree.data];
            try {
                wrapper = (f && f["visit_wrapper"]) || null;
                if (wrapper !== null) {
                    return f.visit_wrapper(f, tree.data, children, tree.meta);
                }
                else {
                    return f(children);
                }
            }
            catch (e) {
                if (e instanceof GrammarError || e instanceof Discard) {
                    throw e;
                }
                else if (e instanceof Error) {
                    throw new VisitError(tree.data, tree, e);
                }
                else {
                    throw e;
                }
            }
        }
        else {
            return this.__default__(tree.data, children, tree.meta);
        }
    }
    _call_userfunc_token(token) {
        let f;
        if (token && token.type && this && this[token.type]) {
            f = this && this[token.type];
            try {
                return f(token);
            }
            catch (e) {
                if (e instanceof GrammarError || e instanceof Discard) {
                    throw e;
                }
                else if (e instanceof Error) {
                    throw new VisitError(token.type, token, e);
                }
                else {
                    throw e;
                }
            }
        }
        else {
            return this.__default_token__(token);
        }
    }
    *_transform_children(children) {
        for (const c of children) {
            try {
                if (c instanceof Tree) {
                    yield this._transform_tree(c);
                }
                else if (this.__visit_tokens__ && c instanceof Token) {
                    yield this._call_userfunc_token(c);
                }
                else {
                    yield c;
                }
            }
            catch (e) {
                if (e instanceof Discard) {
                    // pass
                }
                else {
                    throw e;
                }
            }
        }
    }
    _transform_tree(tree) {
        let children = [...this._transform_children(tree.children)];
        return this._call_userfunc(tree, children);
    }
    /**
      Transform the given tree, and return the final result
    */
    transform(tree) {
        return this._transform_tree(tree);
    }
    /**
      Default function that is called if there is no attribute matching ``data``
  
          Can be overridden. Defaults to creating a new copy of the tree node (i.e. ``return Tree(data, children, meta)``)
  
    */
    __default__(data, children, meta) {
        return new Tree(data, children, meta);
    }
    /**
      Default function that is called if there is no attribute matching ``token.type``
  
          Can be overridden. Defaults to returning the token as-is.
  
    */
    __default_token__(token) {
        return token;
    }
}
/**
  Same as Transformer, but non-recursive, and changes the tree in-place instead of returning new instances

    Useful for huge trees. Conservative in memory.

*/
class Transformer_InPlace extends Transformer {
    _transform_tree(tree) {
        // Cancel recursion
        return this._call_userfunc(tree);
    }
    transform(tree) {
        for (const subtree of tree.iter_subtrees()) {
            subtree.children = [...this._transform_children(subtree.children)];
        }
        return this._transform_tree(tree);
    }
}
/**
  Same as Transformer but non-recursive.

    Like Transformer, it doesn't change the original tree.

    Useful for huge trees.

*/
class Transformer_NonRecursive extends Transformer {
    transform(tree) {
        let args, size;
        // Tree to postfix
        let rev_postfix = [];
        let q = [tree];
        while (q.length) {
            const t = q.pop();
            rev_postfix.push(t);
            if (t instanceof Tree) {
                q.push(...t.children);
            }
        }
        // Postfix to tree
        let stack = [];
        for (const x of [...rev_postfix].reverse()) {
            if (x instanceof Tree) {
                size = x.children.length;
                if (size) {
                    args = stack.slice(-size);
                    stack.splice(-size);
                }
                else {
                    args = [];
                }
                stack.push(this._call_userfunc(x, args));
            }
            else if (this.__visit_tokens__ && x instanceof Token) {
                stack.push(this._call_userfunc_token(x));
            }
            else {
                stack.push(x);
            }
        }
        let [t] = stack;
        // We should have only one tree remaining
        return t;
    }
}
/**
  Same as Transformer, recursive, but changes the tree in-place instead of returning new instances
*/
class Transformer_InPlaceRecursive extends Transformer {
    _transform_tree(tree) {
        tree.children = [...this._transform_children(tree.children)];
        return this._call_userfunc(tree);
    }
}
// Visitors
class VisitorBase {
    _call_userfunc(tree) {
        const callback = this[tree.data];
        if (callback) {
            return callback(tree);
        }
        else {
            return this.__default__(tree);
        }
    }
    /**
      Default function that is called if there is no attribute matching ``tree.data``
  
          Can be overridden. Defaults to doing nothing.
  
    */
    __default__(tree) {
        return tree;
    }
    __class_getitem__(_) {
        return cls;
    }
}
/**
  Tree visitor, non-recursive (can handle huge trees).

    Visiting a node calls its methods (provided by the user via inheritance) according to ``tree.data``

*/
class Visitor extends VisitorBase {
    /**
      Visits the tree, starting with the leaves and finally the root (bottom-up)
    */
    visit(tree) {
        for (const subtree of tree.iter_subtrees()) {
            this._call_userfunc(subtree);
        }
        return tree;
    }
    /**
      Visit the tree, starting at the root, and ending at the leaves (top-down)
    */
    visit_topdown(tree) {
        for (const subtree of tree.iter_subtrees_topdown()) {
            this._call_userfunc(subtree);
        }
        return tree;
    }
}
/**
  Bottom-up visitor, recursive.

    Visiting a node calls its methods (provided by the user via inheritance) according to ``tree.data``

    Slightly faster than the non-recursive version.

*/
class Visitor_Recursive extends VisitorBase {
    /**
      Visits the tree, starting with the leaves and finally the root (bottom-up)
    */
    visit(tree) {
        for (const child of tree.children) {
            if (child instanceof Tree) {
                this.visit(child);
            }
        }
        this._call_userfunc(tree);
        return tree;
    }
    /**
      Visit the tree, starting at the root, and ending at the leaves (top-down)
    */
    visit_topdown(tree) {
        this._call_userfunc(tree);
        for (const child of tree.children) {
            if (child instanceof Tree) {
                this.visit_topdown(child);
            }
        }
        return tree;
    }
}
/**
  Interpreter walks the tree starting at the root.

    Visits the tree, starting with the root and finally the leaves (top-down)

    For each tree node, it calls its methods (provided by user via inheritance) according to ``tree.data``.

    Unlike ``Transformer`` and ``Visitor``, the Interpreter doesn't automatically visit its sub-branches.
    The user has to explicitly call ``visit``, ``visit_children``, or use the ``@visit_children_decor``.
    This allows the user to implement branching and loops.

*/
class Interpreter extends _Decoratable {
    visit(tree) {
        if (tree.data in this) {
            return this[tree.data](tree);
        }
        else {
            return this.__default__(tree);
        }
    }
    visit_children(tree) {
        return tree.children.map((child) => child instanceof Tree ? this.visit(child) : child);
    }
    __default__(tree) {
        return this.visit_children(tree);
    }
}
//
// Grammar
//
class Symbol extends Serialize {
    static get is_term() {
        return NotImplemented;
    }
    get is_term() {
        return this.constructor.is_term;
    }
    constructor(name) {
        super();
        this.name = name;
    }
    eq(other) {
        return this.is_term === other.is_term && this.name === other.name;
    }
    repr() {
        return format("%s(%r)", type(this).name, this.name);
    }
    static get fullrepr() {
        return property(__repr__);
    }
    get fullrepr() {
        return this.constructor.fullrepr;
    }
}
class Terminal extends Symbol {
    static get __serialize_fields__() {
        return ["name", "filter_out"];
    }
    static get is_term() {
        return true;
    }
    get is_term() {
        return this.constructor.is_term;
    }
    constructor(name, filter_out = false) {
        super();
        this.name = name;
        this.filter_out = filter_out;
    }
    get fullrepr() {
        return format("%s(%r, %r)", type(this).name, this.name, this.filter_out);
    }
}
class NonTerminal extends Symbol {
    static get __serialize_fields__() {
        return ["name"];
    }
    static get is_term() {
        return false;
    }
    get is_term() {
        return this.constructor.is_term;
    }
}
class RuleOptions extends Serialize {
    static get __serialize_fields__() {
        return [
            "keep_all_tokens",
            "expand1",
            "priority",
            "template_source",
            "empty_indices",
        ];
    }
    constructor(keep_all_tokens = false, expand1 = false, priority = null, template_source = null, empty_indices = []) {
        super();
        this.keep_all_tokens = keep_all_tokens;
        this.expand1 = expand1;
        this.priority = priority;
        this.template_source = template_source;
        this.empty_indices = empty_indices;
    }
    repr() {
        return format("RuleOptions(%r, %r, %r, %r)", this.keep_all_tokens, this.expand1, this.priority, this.template_source);
    }
}
/**

        origin : a symbol
        expansion : a list of symbols
        order : index of this expansion amongst all rules of the same name

*/
class Rule extends Serialize {
    static get __serialize_fields__() {
        return ["origin", "expansion", "order", "alias", "options"];
    }
    static get __serialize_namespace__() {
        return [Terminal, NonTerminal, RuleOptions];
    }
    constructor(origin, expansion, order = 0, alias = null, options = null) {
        super();
        this.origin = origin;
        this.expansion = expansion;
        this.alias = alias;
        this.order = order;
        this.options = options || new RuleOptions();
        this._hash = hash([this.origin, tuple(this.expansion)]);
    }
    _deserialize() {
        this._hash = hash([this.origin, tuple(this.expansion)]);
    }
    repr() {
        return format("Rule(%r, %r, %r, %r)", this.origin, this.expansion, this.alias, this.options);
    }
    eq(other) {
        if (!(other instanceof Rule)) {
            return false;
        }
        return this.origin === other.origin && this.expansion === other.expansion;
    }
}
//
// Lexer
//
// Lexer Implementation
class Pattern extends Serialize {
    static get raw() {
        return null;
    }
    get raw() {
        return this.constructor.raw;
    }
    static get type() {
        return null;
    }
    get type() {
        return this.constructor.type;
    }
    constructor(value, flags = [], raw = null) {
        super();
        this.value = value;
        this.flags = frozenset(flags);
        this.raw = raw;
    }
    repr() {
        return repr(this.to_regexp());
    }
    eq(other) {
        return (type(this) === type(other) &&
            this.value === other.value &&
            this.flags === other.flags);
    }
    to_regexp() {
        throw new NotImplementedError();
    }
    min_width() {
        throw new NotImplementedError();
    }
    max_width() {
        throw new NotImplementedError();
    }
    _get_flags(value) {
        return value;
    }
}
class PatternStr extends Pattern {
    static get __serialize_fields__() {
        return ["value", "flags"];
    }
    static get type() {
        return "str";
    }
    get type() {
        return this.constructor.type;
    }
    to_regexp() {
        return this._get_flags(re.escape(this.value));
    }
    get min_width() {
        return this.value.length;
    }
    static get max_width() {
        return this.min_width;
    }
    get max_width() {
        return this.constructor.max_width;
    }
}
class PatternRE extends Pattern {
    static get __serialize_fields__() {
        return ["value", "flags", "_width"];
    }
    static get type() {
        return "re";
    }
    get type() {
        return this.constructor.type;
    }
    to_regexp() {
        return this._get_flags(this.value);
    }
    _get_width() {
        if (this._width === null) {
            this._width = get_regexp_width(this.to_regexp());
        }
        return this._width;
    }
    get min_width() {
        return this._get_width()[0];
    }
    get max_width() {
        return this._get_width()[1];
    }
}
class TerminalDef extends Serialize {
    static get __serialize_fields__() {
        return ["name", "pattern", "priority"];
    }
    static get __serialize_namespace__() {
        return [PatternStr, PatternRE];
    }
    constructor(name, pattern, priority = 1) {
        super();
        this.name = name;
        this.pattern = pattern;
        this.priority = priority;
    }
    repr() {
        return format("%s(%r, %r)", type(this).name, this.name, this.pattern);
    }
    user_repr() {
        if (this.name.startsWith("__")) {
            // We represent a generated terminal
            return this.pattern.raw || this.name;
        }
        else {
            return this.name;
        }
    }
}
/**
  A string with meta-information, that is produced by the lexer.

    When parsing text, the resulting chunks of the input that haven't been discarded,
    will end up in the tree as Token instances. The Token class inherits from Python's ``str``,
    so normal string comparisons and operations will work as expected.

    Attributes:
        type: Name of the token (as specified in grammar)
        value: Value of the token (redundant, as ``token.value == token`` will always be true)
        start_pos: The index of the token in the text
        line: The line of the token in the text (starting with 1)
        column: The column of the token in the text (starting with 1)
        end_line: The line where the token ends
        end_column: The next column after the end of the token. For example,
            if the token is a single character with a column value of 4,
            end_column will be 5.
        end_pos: the index where the token ends (basically ``start_pos + len(token)``)

*/
class Token {
    constructor(type_, value, start_pos = null, line = null, column = null, end_line = null, end_column = null, end_pos = null) {
        this.type = type_;
        this.value = value;
        this.start_pos = start_pos;
        this.line = line;
        this.column = column;
        this.end_line = end_line;
        this.end_column = end_column;
        this.end_pos = end_pos;
    }
    update(type_ = null, value = null) {
        return Token.new_borrow_pos(type_ !== null ? type_ : this.type, value !== null ? value : this.value, this);
    }
    static new_borrow_pos(type_, value, borrow_t) {
        const cls = this;
        return new cls(type_, value, borrow_t.start_pos, borrow_t.line, borrow_t.column, borrow_t.end_line, borrow_t.end_column, borrow_t.end_pos);
    }
    repr() {
        return format("Token(%r, %r)", this.type, this.value);
    }
    eq(other) {
        if (other instanceof Token && this.type !== other.type) {
            return false;
        }
        return str.__eq__(this, other);
    }
    static get __hash__() {
        return str.__hash__;
    }
}
class LineCounter {
    constructor(newline_char) {
        this.newline_char = newline_char;
        this.char_pos = 0;
        this.line = 1;
        this.column = 1;
        this.line_start_pos = 0;
    }
    eq(other) {
        if (!(other instanceof LineCounter)) {
            return NotImplemented;
        }
        return (this.char_pos === other.char_pos &&
            this.newline_char === other.newline_char);
    }
    /**
      Consume a token and calculate the new line & column.
  
          As an optional optimization, set test_newline=False if token doesn't contain a newline.
  
    */
    feed(token, test_newline = true) {
        let newlines;
        if (test_newline) {
            newlines = str_count(token, this.newline_char);
            if (newlines) {
                this.line += newlines;
                this.line_start_pos =
                    this.char_pos + token.lastIndexOf(this.newline_char) + 1;
            }
        }
        this.char_pos += token.length;
        this.column = this.char_pos - this.line_start_pos + 1;
    }
}
class _UnlessCallback {
    constructor(scanner) {
        this.scanner = scanner;
    }
    __call__(t) {
        let _value;
        let res = this.scanner.match(t.value, 0);
        if (res) {
            [_value, t.type] = res;
        }
        return t;
    }
}
const UnlessCallback = callable_class(_UnlessCallback);
class _CallChain {
    constructor(callback1, callback2, cond) {
        this.callback1 = callback1;
        this.callback2 = callback2;
        this.cond = cond;
    }
    __call__(t) {
        let t2 = this.callback1(t);
        return this.cond(t2) ? this.callback2(t) : t2;
    }
}
const CallChain = callable_class(_CallChain);
function _create_unless(terminals, g_regex_flags, re_, use_bytes) {
    let s, unless;
    let tokens_by_type = classify(terminals, (t) => t.pattern.constructor.name);
    let embedded_strs = new Set();
    let callback = {};
    for (const retok of tokens_by_type.get('PatternRE') || []) {
        unless = [];
        for (const strtok of tokens_by_type.get('PatternStr') || []) {
            if (strtok.priority > retok.priority) {
                continue;
            }
            s = strtok.pattern.value;
            if (s === _get_match(re_, retok.pattern.to_regexp(), s, g_regex_flags)) {
                unless.push(strtok);
                if (isSubset(new Set(strtok.pattern.flags), new Set(retok.pattern.flags))) {
                    embedded_strs.add(strtok);
                }
            }
        }
        if (unless.length) {
            callback[retok.name] = new UnlessCallback(new Scanner(unless, g_regex_flags, re_, use_bytes, true));
        }
    }
    let new_terminals = terminals
        .filter((t) => !embedded_strs.has(t))
        .map((t) => t);
    return [new_terminals, callback];
}
/**
    Expressions that may indicate newlines in a regexp:
        - newlines (\n)
        - escaped newline (\\n)
        - anything but ([^...])
        - any-char (.) when the flag (?s) exists
        - spaces (\s)

  */
function _regexp_has_newline(r) {
    return (r.includes("\n") ||
        r.includes("\\n") ||
        r.includes("\\s") ||
        r.includes("[^") ||
        (r.includes("(?s") && r.includes(".")));
}
/**
  Lexer interface

    Method Signatures:
        lex(self, text) -> Iterator[Token]

*/
class Lexer {
    static get lex() {
        return NotImplemented;
    }
    get lex() {
        return this.constructor.lex;
    }
    make_lexer_state(text) {
        let line_ctr = new LineCounter("\n");
        return new LexerState(text, line_ctr);
    }
}
function sort_by_key_tuple(arr, key) {
    arr.sort((a, b) => {
        let ta = key(a);
        let tb = key(b);
        for (let i = 0; i < ta.length; i++) {
            if (ta[i] > tb[i]) {
                return 1;
            }
            else if (ta[i] < tb[i]) {
                return -1;
            }
        }
        return 0;
    });
}
class TraditionalLexer extends Lexer {
    constructor(conf) {
        super();
        let terminals = [...conf.terminals];
        this.re = conf.re_module;
        if (!conf.skip_validation) {
            // Sanitization
            for (const t of terminals) {
                try {
                    this.re.compile(t.pattern.to_regexp(), conf.g_regex_flags);
                }
                catch (e) {
                    if (e instanceof this.re.error) {
                        throw new LexError(format("Cannot compile token %s: %s", t.name, t.pattern));
                    }
                    else {
                        throw e;
                    }
                }
                if (t.pattern.min_width === 0) {
                    throw new LexError(format("Lexer does not allow zero-width terminals. (%s: %s)", t.name, t.pattern));
                }
            }
            if (!(new Set(conf.ignore) <= new Set(terminals.map((t) => t.name)))) {
                throw new LexError(format("Ignore terminals are not defined: %s", new Set(conf.ignore) - new Set(terminals.map((t) => t.name))));
            }
        }
        // Init
        this.newline_types = frozenset(terminals
            .filter((t) => _regexp_has_newline(t.pattern.to_regexp()))
            .map((t) => t.name));
        this.ignore_types = frozenset(conf.ignore);
        sort_by_key_tuple(terminals, (x) => [
            -x.priority,
            -x.pattern.max_width,
            -x.pattern.value.length,
            x.name,
        ]);
        this.terminals = terminals;
        this.user_callbacks = conf.callbacks;
        this.g_regex_flags = conf.g_regex_flags;
        this.use_bytes = conf.use_bytes;
        this.terminals_by_name = conf.terminals_by_name;
        this._scanner = null;
    }
    _build_scanner() {
        let terminals;
        [terminals, this.callback] = _create_unless(this.terminals, this.g_regex_flags, this.re, this.use_bytes);
        for (const [type_, f] of dict_items(this.user_callbacks)) {
            if (type_ in this.callback) {
                // Already a callback there, probably UnlessCallback
                this.callback[type_] = new CallChain(this.callback[type_], f, (t) => t.type === type_);
            }
            else {
                this.callback[type_] = f;
            }
        }
        this._scanner = new Scanner(terminals, this.g_regex_flags, this.re, this.use_bytes);
    }
    get scanner() {
        if (this._scanner === null) {
            this._build_scanner();
        }
        return this._scanner;
    }
    match(text, pos) {
        return this.scanner.match(text, pos);
    }
    *lex(state, parser_state) {
        try {
            while (true) {
                yield this.next_token(state, parser_state);
            }
        }
        catch (e) {
            if (e instanceof EOFError) {
                // pass
            }
            else {
                throw e;
            }
        }
    }
    next_token(lex_state, parser_state = null) {
        let allowed, res, t, t2, type_, value;
        let line_ctr = lex_state.line_ctr;
        while (line_ctr.char_pos < lex_state.text.length) {
            res = this.match(lex_state.text, line_ctr.char_pos);
            if (!res) {
                allowed = this.scanner.allowed_types - this.ignore_types;
                if (!allowed) {
                    allowed = new Set(["<END-OF-FILE>"]);
                }
                throw new UnexpectedCharacters({
                    seq: lex_state.text,
                    lex_pos: line_ctr.char_pos,
                    line: line_ctr.line,
                    column: line_ctr.column,
                    allowed: allowed,
                    token_history: lex_state.last_token && [lex_state.last_token],
                    state: parser_state,
                    terminals_by_name: this.terminals_by_name,
                });
            }
            let [value, type_] = res;
            if (!this.ignore_types.has(type_)) {
                t = new Token(type_, value, line_ctr.char_pos, line_ctr.line, line_ctr.column);
                line_ctr.feed(value, this.newline_types.has(type_));
                t.end_line = line_ctr.line;
                t.end_column = line_ctr.column;
                t.end_pos = line_ctr.char_pos;
                if (t.type in this.callback) {
                    t = this.callback[t.type](t);
                    if (!(t instanceof Token)) {
                        throw new LexError(format("Callbacks must return a token (returned %r)", t));
                    }
                }
                lex_state.last_token = t;
                return t;
            }
            else {
                if (type_ in this.callback) {
                    t2 = new Token(type_, value, line_ctr.char_pos, line_ctr.line, line_ctr.column);
                    this.callback[type_](t2);
                }
                line_ctr.feed(value, this.newline_types.has(type_));
            }
        }
        // EOF
        throw new EOFError(this);
    }
}
class LexerState {
    constructor(text, line_ctr, last_token = null) {
        this.text = text;
        this.line_ctr = line_ctr;
        this.last_token = last_token;
    }
    eq(other) {
        if (!(other instanceof LexerState)) {
            return NotImplemented;
        }
        return (this.text === other.text &&
            this.line_ctr === other.line_ctr &&
            this.last_token === other.last_token);
    }
}
class ContextualLexer extends Lexer {
    constructor({ conf, states, always_accept = [] } = {}) {
        super();
        let accepts, key, lexer, lexer_conf;
        let terminals = [...conf.terminals];
        let terminals_by_name = conf.terminals_by_name;
        let trad_conf = copy(conf);
        trad_conf.terminals = terminals;
        let lexer_by_tokens = new Map();
        this.lexers = {};
        for (let [state, accepts] of dict_items(states)) {
            key = frozenset(accepts);
            if (lexer_by_tokens.has(key)) {
                lexer = lexer_by_tokens.get(key);
            }
            else {
                accepts = union(new Set(accepts), [
                    ...new Set(conf.ignore),
                    ...new Set(always_accept),
                ]);
                lexer_conf = copy(trad_conf);
                lexer_conf.terminals = [...accepts]
                    .filter((n) => n in terminals_by_name)
                    .map((n) => terminals_by_name[n]);
                lexer = new TraditionalLexer(lexer_conf);
                lexer_by_tokens.set(key, lexer);
            }
            this.lexers[state] = lexer;
        }
        this.root_lexer = new TraditionalLexer(trad_conf);
    }
    make_lexer_state(text) {
        return this.root_lexer.make_lexer_state(text);
    }
    *lex(lexer_state, parser_state) {
        let last_token, lexer, token;
        try {
            while (true) {
                lexer = this.lexers[parser_state.position];
                yield lexer.next_token(lexer_state, parser_state);
            }
        }
        catch (e) {
            if (e instanceof EOFError) {
                // pass
            }
            else if (e instanceof UnexpectedCharacters) {
                // In the contextual lexer, UnexpectedCharacters can mean that the terminal is defined, but not in the current context.
                // This tests the input against the global context, to provide a nicer error.
                try {
                    last_token = lexer_state.last_token;
                    // Save last_token. Calling root_lexer.next_token will change this to the wrong token
                    token = this.root_lexer.next_token(lexer_state, parser_state);
                    throw new UnexpectedToken({
                        token: token,
                        expected: e.allowed,
                        state: parser_state,
                        token_history: [last_token],
                        terminals_by_name: this.root_lexer.terminals_by_name,
                    });
                }
                catch (e) {
                    if (e instanceof UnexpectedCharacters) {
                        throw e;
                    }
                    else {
                        throw e;
                    }
                }
            }
            else {
                throw e;
            }
        }
    }
}
/**
  A thread that ties a lexer instance and a lexer state, to be used by the parser
*/
class LexerThread {
    constructor(lexer, text) {
        this.lexer = lexer;
        this.state = lexer.make_lexer_state(text);
    }
    lex(parser_state) {
        return this.lexer.lex(this.state, parser_state);
    }
}
//
// Common
//
class LexerConf extends Serialize {
    static get __serialize_fields__() {
        return ["terminals", "ignore", "g_regex_flags", "use_bytes", "lexer_type"];
    }
    static get __serialize_namespace__() {
        return [TerminalDef];
    }
    constructor({ terminals, re_module, ignore = [], postlex = null, callbacks = null, g_regex_flags = '', skip_validation = false, use_bytes = false, } = {}) {
        super();
        this.terminals = terminals;
        this.terminals_by_name = Object.fromEntries(this.terminals.map((t) => [t.name, t]));
        this.ignore = ignore;
        this.postlex = postlex;
        this.callbacks = callbacks || {};
        this.g_regex_flags = g_regex_flags;
        this.re_module = re_module;
        this.skip_validation = skip_validation;
        this.use_bytes = use_bytes;
        this.lexer_type = null;
    }
    _deserialize() {
        this.terminals_by_name = Object.fromEntries(this.terminals.map((t) => [t.name, t]));
    }
}
class ParserConf extends Serialize {
    static get __serialize_fields__() {
        return ["rules", "start", "parser_type"];
    }
    constructor(rules, callbacks, start) {
        super();
        this.rules = rules;
        this.callbacks = callbacks;
        this.start = start;
        this.parser_type = null;
    }
}
//
// Parse Tree Builder
//
class _ExpandSingleChild {
    constructor(node_builder) {
        this.node_builder = node_builder;
    }
    __call__(children) {
        if (children.length === 1) {
            return children[0];
        }
        else {
            return this.node_builder(children);
        }
    }
}
const ExpandSingleChild = callable_class(_ExpandSingleChild);
class _PropagatePositions {
    constructor(node_builder, node_filter = null) {
        this.node_builder = node_builder;
        this.node_filter = node_filter;
    }
    __call__(children) {
        let first_meta, last_meta, res_meta;
        let res = this.node_builder(children);
        if (res instanceof Tree) {
            // Calculate positions while the tree is streaming, according to the rule:
            // - nodes start at the start of their first child's container,
            //   and end at the end of their last child's container.
            // Containers are nodes that take up space in text, but have been inlined in the tree.
            res_meta = res.meta;
            first_meta = this._pp_get_meta(children);
            if (first_meta !== null) {
                if (!("line" in res_meta)) {
                    // meta was already set, probably because the rule has been inlined (e.g. `?rule`)
                    res_meta.line =
                        (first_meta && first_meta["container_line"]) || first_meta.line;
                    res_meta.column =
                        (first_meta && first_meta["container_column"]) || first_meta.column;
                    res_meta.start_pos =
                        (first_meta && first_meta["container_start_pos"]) ||
                            first_meta.start_pos;
                    res_meta.empty = false;
                }
                res_meta.container_line =
                    (first_meta && first_meta["container_line"]) || first_meta.line;
                res_meta.container_column =
                    (first_meta && first_meta["container_column"]) || first_meta.column;
            }
            last_meta = this._pp_get_meta([...children].reverse());
            if (last_meta !== null) {
                if (!("end_line" in res_meta)) {
                    res_meta.end_line =
                        (last_meta && last_meta["container_end_line"]) ||
                            last_meta.end_line;
                    res_meta.end_column =
                        (last_meta && last_meta["container_end_column"]) ||
                            last_meta.end_column;
                    res_meta.end_pos =
                        (last_meta && last_meta["container_end_pos"]) || last_meta.end_pos;
                    res_meta.empty = false;
                }
                res_meta.container_end_line =
                    (last_meta && last_meta["container_end_line"]) || last_meta.end_line;
                res_meta.container_end_column =
                    (last_meta && last_meta["container_end_column"]) ||
                        last_meta.end_column;
            }
        }
        return res;
    }
    _pp_get_meta(children) {
        for (const c of children) {
            if (this.node_filter !== null && !this.node_filter(c)) {
                continue;
            }
            if (c instanceof Tree) {
                if (!c.meta.empty) {
                    return c.meta;
                }
            }
            else if (c instanceof Token) {
                return c;
            }
        }
    }
}
const PropagatePositions = callable_class(_PropagatePositions);
function make_propagate_positions(option) {
    if (callable(option)) {
        return partial({
            unknown_param_0: PropagatePositions,
            node_filter: option,
        });
    }
    else if (option === true) {
        return PropagatePositions;
    }
    else if (option === false) {
        return null;
    }
    throw new ConfigurationError(format("Invalid option for propagate_positions: %r", option));
}
class _ChildFilter {
    constructor(to_include, append_none, node_builder) {
        this.node_builder = node_builder;
        this.to_include = to_include;
        this.append_none = append_none;
    }
    __call__(children) {
        let filtered = [];
        for (const [i, to_expand, add_none] of this.to_include) {
            if (add_none) {
                filtered.push(...list_repeat([null], add_none));
            }
            if (to_expand) {
                filtered.push(...children[i].children);
            }
            else {
                filtered.push(children[i]);
            }
        }
        if (this.append_none) {
            filtered.push(...list_repeat([null], this.append_none));
        }
        return this.node_builder(filtered);
    }
}
const ChildFilter = callable_class(_ChildFilter);
/**
  Optimized childfilter for LALR (assumes no duplication in parse tree, so it's safe to change it)
*/
class _ChildFilterLALR extends _ChildFilter {
    __call__(children) {
        let filtered = [];
        for (const [i, to_expand, add_none] of this.to_include) {
            if (add_none) {
                filtered.push(...list_repeat([null], add_none));
            }
            if (to_expand) {
                if (filtered.length) {
                    filtered.push(...children[i].children);
                }
                else {
                    // Optimize for left-recursion
                    filtered = children[i].children;
                }
            }
            else {
                filtered.push(children[i]);
            }
        }
        if (this.append_none) {
            filtered.push(...list_repeat([null], this.append_none));
        }
        return this.node_builder(filtered);
    }
}
const ChildFilterLALR = callable_class(_ChildFilterLALR);
/**
  Optimized childfilter for LALR (assumes no duplication in parse tree, so it's safe to change it)
*/
class _ChildFilterLALR_NoPlaceholders extends _ChildFilter {
    constructor(to_include, node_builder) {
        super();
        this.node_builder = node_builder;
        this.to_include = to_include;
    }
    __call__(children) {
        let filtered = [];
        for (const [i, to_expand] of this.to_include) {
            if (to_expand) {
                if (filtered.length) {
                    filtered.push(...children[i].children);
                }
                else {
                    // Optimize for left-recursion
                    filtered = children[i].children;
                }
            }
            else {
                filtered.push(children[i]);
            }
        }
        return this.node_builder(filtered);
    }
}
const ChildFilterLALR_NoPlaceholders = callable_class(_ChildFilterLALR_NoPlaceholders);
function _should_expand(sym) {
    return !sym.is_term && sym.name.startsWith("_");
}
function maybe_create_child_filter(expansion, keep_all_tokens, ambiguous, _empty_indices) {
    let empty_indices, s;
    // Prepare empty_indices as: How many Nones to insert at each index?
    if (_empty_indices.length) {
        s = _empty_indices.map((b) => (0 + b).toString()).join("");
        empty_indices = s.split("0").map((ones) => ones.length);
    }
    else {
        empty_indices = list_repeat([0], expansion.length + 1);
    }
    let to_include = [];
    let nones_to_add = 0;
    for (const [i, sym] of enumerate(expansion)) {
        nones_to_add += empty_indices[i];
        if (keep_all_tokens || !(sym.is_term && sym.filter_out)) {
            to_include.push([i, _should_expand(sym), nones_to_add]);
            nones_to_add = 0;
        }
    }
    nones_to_add += empty_indices[expansion.length];
    if (_empty_indices.length ||
        to_include.length < expansion.length ||
        any(to_include.map(([i, to_expand, _]) => to_expand))) {
        if ((_empty_indices.length || ambiguous).length) {
            return partial(ambiguous ? ChildFilter : ChildFilterLALR, to_include, nones_to_add);
        }
        else {
            // LALR without placeholders
            return partial(ChildFilterLALR_NoPlaceholders, to_include.map(([i, x, _]) => [i, x]));
        }
    }
}
/**
  Deal with the case where we're expanding children ('_rule') into a parent but the children
       are ambiguous. i.e. (parent->_ambig->_expand_this_rule). In this case, make the parent itself
       ambiguous with as many copies as their are ambiguous children, and then copy the ambiguous children
       into the right parents in the right places, essentially shifting the ambiguity up the tree.
*/
class _AmbiguousExpander {
    constructor(to_expand, tree_class, node_builder) {
        this.node_builder = node_builder;
        this.tree_class = tree_class;
        this.to_expand = to_expand;
    }
    __call__(children) {
        let to_expand;
        function _is_ambig_tree(t) {
            return "data" in t && t.data === "_ambig";
        }
        // -- When we're repeatedly expanding ambiguities we can end up with nested ambiguities.
        //    All children of an _ambig node should be a derivation of that ambig node, hence
        //    it is safe to assume that if we see an _ambig node nested within an ambig node
        //    it is safe to simply expand it into the parent _ambig node as an alternative derivation.
        let ambiguous = [];
        for (const [i, child] of enumerate(children)) {
            if (_is_ambig_tree(child)) {
                if (i in this.to_expand) {
                    ambiguous.push(i);
                }
                to_expand = enumerate(child.children)
                    .filter(([j, grandchild]) => _is_ambig_tree(grandchild))
                    .map(([j, grandchild]) => j);
                child.expand_kids_by_index(...to_expand);
            }
        }
        if (!ambiguous) {
            return this.node_builder(children);
        }
        let expand = enumerate(children).map(([i, child]) => ambiguous.includes(i) ? iter(child.children) : repeat(child));
        return this.tree_class("_ambig", product(zip(...expand)).map((f) => this.node_builder([...f[0]])));
    }
}
const AmbiguousExpander = callable_class(_AmbiguousExpander);
function maybe_create_ambiguous_expander(tree_class, expansion, keep_all_tokens) {
    let to_expand = enumerate(expansion)
        .filter(([i, sym]) => keep_all_tokens ||
        (!(sym.is_term && sym.filter_out) && _should_expand(sym)))
        .map(([i, sym]) => i);
    if (to_expand.length) {
        return partial(AmbiguousExpander, to_expand, tree_class);
    }
}
/**

    Propagate ambiguous intermediate nodes and their derivations up to the
    current rule.

    In general, converts

    rule
      _iambig
        _inter
          someChildren1
          ...
        _inter
          someChildren2
          ...
      someChildren3
      ...

    to

    _ambig
      rule
        someChildren1
        ...
        someChildren3
        ...
      rule
        someChildren2
        ...
        someChildren3
        ...
      rule
        childrenFromNestedIambigs
        ...
        someChildren3
        ...
      ...

    propagating up any nested '_iambig' nodes along the way.

*/
class _AmbiguousIntermediateExpander {
    constructor(tree_class, node_builder) {
        this.node_builder = node_builder;
        this.tree_class = tree_class;
    }
    __call__(children) {
        let iambig_node, new_tree, processed_nodes, result;
        function _is_iambig_tree(child) {
            return "data" in child && child.data === "_iambig";
        }
        /**
    
                Recursively flatten the derivations of the parent of an '_iambig'
                node. Returns a list of '_inter' nodes guaranteed not
                to contain any nested '_iambig' nodes, or None if children does
                not contain an '_iambig' node.
    
      */
        function _collapse_iambig(children) {
            let collapsed, iambig_node, new_tree, result;
            // Due to the structure of the SPPF,
            // an '_iambig' node can only appear as the first child
            if (children && _is_iambig_tree(children[0])) {
                iambig_node = children[0];
                result = [];
                for (const grandchild of iambig_node.children) {
                    collapsed = _collapse_iambig(grandchild.children);
                    if (collapsed) {
                        for (const child of collapsed) {
                            child.children += children.slice(1);
                        }
                        result.push(...collapsed);
                    }
                    else {
                        new_tree = this.tree_class("_inter", grandchild.children + children.slice(1));
                        result.push(new_tree);
                    }
                }
                return result;
            }
        }
        let collapsed = _collapse_iambig(children);
        if (collapsed) {
            processed_nodes = collapsed.map((c) => this.node_builder(c.children));
            return this.tree_class("_ambig", processed_nodes);
        }
        return this.node_builder(children);
    }
}
const AmbiguousIntermediateExpander = callable_class(_AmbiguousIntermediateExpander);
function inplace_transformer(func) {
    function f(children) {
        // function name in a Transformer is a rule name.
        let tree = new Tree(func.name, children);
        return func(tree);
    }
    f = wraps(func)(f);
    return f;
}
function apply_visit_wrapper(func, name, wrapper) {
    if (wrapper === _vargs_meta || wrapper === _vargs_meta_inline) {
        throw new NotImplementedError("Meta args not supported for internal transformer");
    }
    function f(children) {
        return wrapper(func, name, children, null);
    }
    f = wraps(func)(f);
    return f;
}
class ParseTreeBuilder {
    constructor(rules, tree_class, propagate_positions = false, ambiguous = false, maybe_placeholders = false) {
        this.tree_class = tree_class;
        this.propagate_positions = propagate_positions;
        this.ambiguous = ambiguous;
        this.maybe_placeholders = maybe_placeholders;
        this.rule_builders = [...this._init_builders(rules)];
    }
    *_init_builders(rules) {
        let expand_single_child, keep_all_tokens, options, wrapper_chain;
        let propagate_positions = make_propagate_positions(this.propagate_positions);
        for (const rule of rules) {
            options = rule.options;
            keep_all_tokens = options.keep_all_tokens;
            expand_single_child = options.expand1;
            wrapper_chain = [
                ...filter(null, [
                    expand_single_child && !rule.alias && ExpandSingleChild,
                    maybe_create_child_filter(rule.expansion, keep_all_tokens, this.ambiguous, this.maybe_placeholders ? options.empty_indices : []),
                    propagate_positions,
                    this.ambiguous &&
                        maybe_create_ambiguous_expander(this.tree_class, rule.expansion, keep_all_tokens),
                    this.ambiguous &&
                        partial(AmbiguousIntermediateExpander, this.tree_class),
                ]),
            ];
            yield [rule, wrapper_chain];
        }
    }
    create_callback(transformer = null) {
        let f, user_callback_name, wrapper;
        let callbacks = new Map();
        for (const [rule, wrapper_chain] of this.rule_builders) {
            user_callback_name =
                rule.alias || rule.options.template_source || rule.origin.name;
            if (transformer && transformer[user_callback_name]) {
                f = transformer && transformer[user_callback_name];
                wrapper = (f && f["visit_wrapper"]) || null;
                if (wrapper !== null) {
                    f = apply_visit_wrapper(f, user_callback_name, wrapper);
                }
                else if (transformer instanceof Transformer_InPlace) {
                    f = inplace_transformer(f);
                }
            }
            else {
                f = partial(this.tree_class, user_callback_name);
            }
            for (const w of wrapper_chain) {
                f = w(f);
            }
            if (callbacks.has(rule)) {
                throw new GrammarError(format("Rule '%s' already exists", rule));
            }
            callbacks.set(rule, f);
        }
        return callbacks;
    }
}
//
// Lalr Parser
//
class LALR_Parser extends Serialize {
    constructor({ parser_conf, debug = false } = {}) {
        super();
        let analysis = new LALR_Analyzer({
            unknown_param_0: parser_conf,
            debug: debug,
        });
        analysis.compute_lalr();
        let callbacks = parser_conf.callbacks;
        this._parse_table = analysis.parse_table;
        this.parser_conf = parser_conf;
        this.parser = new _Parser(analysis.parse_table, callbacks, debug);
    }
    static deserialize(data, memo, callbacks, debug = false) {
        const cls = this;
        let inst = new_object(cls);
        inst._parse_table = IntParseTable.deserialize(data, memo);
        inst.parser = new _Parser(inst._parse_table, callbacks, debug);
        return inst;
    }
    serialize(memo) {
        return this._parse_table.serialize(memo);
    }
    parse_interactive(lexer, start) {
        return this.parser.parse({
            lexer: lexer,
            start: start,
            start_interactive: true,
        });
    }
    parse({ lexer, start, on_error = null } = {}) {
        let e, p, s;
        try {
            return this.parser.parse({ lexer: lexer, start: start });
        }
        catch (e) {
            if (e instanceof UnexpectedInput) {
                if (on_error === null) {
                    throw e;
                }
                while (true) {
                    if (e instanceof UnexpectedCharacters) {
                        s = e.interactive_parser.lexer_state.state;
                        p = s.line_ctr.char_pos;
                    }
                    if (!on_error(e)) {
                        throw e;
                    }
                    if (e instanceof UnexpectedCharacters) {
                        // If user didn't change the character position, then we should
                        if (p === s.line_ctr.char_pos) {
                            s.line_ctr.feed(s.text.slice(p, p + 1));
                        }
                    }
                    try {
                        return e.interactive_parser.resume_parse();
                    }
                    catch (e2) {
                        if (e2 instanceof UnexpectedToken) {
                            if (e instanceof UnexpectedToken &&
                                e.token.type === e2.token.type &&
                                e2.token.type === "$END" &&
                                e.interactive_parser === e2.interactive_parser) {
                                // Prevent infinite loop
                                throw e2;
                            }
                            e = e2;
                        }
                        else if (e2 instanceof UnexpectedCharacters) {
                            e = e2;
                        }
                        else {
                            throw e2;
                        }
                    }
                }
            }
            else {
                throw e;
            }
        }
    }
}
class ParseConf {
    constructor(parse_table, callbacks, start) {
        this.parse_table = parse_table;
        this.start_state = this.parse_table.start_states[start];
        this.end_state = this.parse_table.end_states[start];
        this.states = this.parse_table.states;
        this.callbacks = callbacks;
        this.start = start;
    }
}
class ParserState {
    constructor(parse_conf, lexer, state_stack = null, value_stack = null) {
        this.parse_conf = parse_conf;
        this.lexer = lexer;
        this.state_stack = state_stack || [this.parse_conf.start_state];
        this.value_stack = value_stack || [];
    }
    get position() {
        return last_item(this.state_stack);
    }
    // Necessary for match_examples() to work
    eq(other) {
        if (!(other instanceof ParserState)) {
            return NotImplemented;
        }
        return (this.state_stack.length === other.state_stack.length &&
            this.position === other.position);
    }
    copy() {
        return copy(this);
    }
    feed_token(token, is_end = false) {
        let _action, action, arg, expected, new_state, rule, s, size, state, value;
        let state_stack = this.state_stack;
        let value_stack = this.value_stack;
        let states = this.parse_conf.states;
        let end_state = this.parse_conf.end_state;
        let callbacks = this.parse_conf.callbacks;
        while (true) {
            state = last_item(state_stack);
            if (token.type in states[state]) {
                [action, arg] = states[state][token.type];
            }
            else {
                expected = new Set(dict_keys(states[state])
                    .filter((s) => isupper(s))
                    .map((s) => s));
                throw new UnexpectedToken({
                    token: token,
                    expected: expected,
                    state: this,
                    interactive_parser: null,
                });
            }
            if (action === Shift) {
                // shift once and return
                state_stack.push(arg);
                value_stack.push(!(token.type in callbacks) ? token : callbacks[token.type](token));
                return;
            }
            else {
                // reduce+shift as many times as necessary
                rule = arg;
                size = rule.expansion.length;
                if (size) {
                    s = value_stack.slice(-size);
                    state_stack.splice(-size);
                    value_stack.splice(-size);
                }
                else {
                    s = [];
                }
                value = callbacks.get(rule)(s);
                [_action, new_state] = states[last_item(state_stack)][rule.origin.name];
                state_stack.push(new_state);
                value_stack.push(value);
                if (is_end && last_item(state_stack) === end_state) {
                    return last_item(value_stack);
                }
            }
        }
    }
}
class _Parser {
    constructor(parse_table, callbacks, debug = false) {
        this.parse_table = parse_table;
        this.callbacks = callbacks;
        this.debug = debug;
    }
    parse({ lexer, start, value_stack = null, state_stack = null, start_interactive = false, } = {}) {
        let parse_conf = new ParseConf(this.parse_table, this.callbacks, start);
        let parser_state = new ParserState(parse_conf, lexer, state_stack, value_stack);
        if (start_interactive) {
            return new InteractiveParser(this, parser_state, parser_state.lexer);
        }
        return this.parse_from_state(parser_state);
    }
    parse_from_state(state) {
        let end_token, token;
        // Main LALR-parser loop
        try {
            token = null;
            for (const token of state.lexer.lex(state)) {
                state.feed_token(token);
            }
            end_token = token
                ? Token.new_borrow_pos("$END", "", token)
                : new Token("$END", "", 0, 1, 1);
            return state.feed_token(end_token, true);
        }
        catch (e) {
            if (e instanceof UnexpectedInput) {
                try {
                    e.interactive_parser = new InteractiveParser(this, state, state.lexer);
                }
                catch (e) {
                    if (e instanceof ReferenceError) {
                        // pass
                    }
                    else {
                        throw e;
                    }
                }
                throw e;
            }
            else if (e instanceof Error) {
                if (this.debug) {
                    console.log("");
                    console.log("STATE STACK DUMP");
                    console.log("----------------");
                    for (const [i, s] of enumerate(state.state_stack)) {
                        console.log(format("%d)", i), s);
                    }
                    console.log("");
                }
                throw e;
            }
            else {
                throw e;
            }
        }
    }
}
//
// Lalr Interactive Parser
//
// This module provides a LALR interactive parser, which is used for debugging and error handling
/**
  InteractiveParser gives you advanced control over parsing and error handling when parsing with LALR.

    For a simpler interface, see the ``on_error`` argument to ``Lark.parse()``.

*/
class InteractiveParser {
    constructor(parser, parser_state, lexer_state) {
        this.parser = parser;
        this.parser_state = parser_state;
        this.lexer_state = lexer_state;
    }
    /**
      Feed the parser with a token, and advance it to the next state, as if it received it from the lexer.
  
          Note that ``token`` has to be an instance of ``Token``.
  
    */
    feed_token(token) {
        return this.parser_state.feed_token(token, token.type === "$END");
    }
    /**
      Try to feed the rest of the lexer state into the interactive parser.
  
          Note that this modifies the instance in place and does not feed an '$END' Token
    */
    exhaust_lexer() {
        for (const token of this.lexer_state.lex(this.parser_state)) {
            this.parser_state.feed_token(token);
        }
    }
    /**
      Feed a '$END' Token. Borrows from 'last_token' if given.
    */
    feed_eof(last_token = null) {
        let eof = last_token !== null
            ? Token.new_borrow_pos("$END", "", last_token)
            : new Token("$END", "", 0, 1, 1);
        return this.feed_token(eof);
    }
    copy() {
        return copy(this);
    }
    eq(other) {
        if (!(other instanceof InteractiveParser)) {
            return false;
        }
        return (this.parser_state === other.parser_state &&
            this.lexer_state === other.lexer_state);
    }
    /**
      Convert to an ``ImmutableInteractiveParser``.
    */
    as_immutable() {
        let p = copy(this);
        return new ImmutableInteractiveParser(p.parser, p.parser_state, p.lexer_state);
    }
    /**
      Print the output of ``choices()`` in a way that's easier to read.
    */
    pretty() {
        let out = ["Parser choices:"];
        for (const [k, v] of dict_items(this.choices())) {
            out.push(format("\t- %s -> %s", k, v));
        }
        out.push(format("stack size: %s", this.parser_state.state_stack.length));
        return out.join("\n");
    }
    /**
      Returns a dictionary of token types, matched to their action in the parser.
  
          Only returns token types that are accepted by the current state.
  
          Updated by ``feed_token()``.
  
    */
    choices() {
        return this.parser_state.parse_conf.parse_table.states[this.parser_state.position];
    }
    /**
      Returns the set of possible tokens that will advance the parser into a new valid state.
    */
    accepts() {
        let new_cursor;
        let accepts = new Set();
        for (const t of this.choices()) {
            if (isupper(t)) {
                // is terminal?
                new_cursor = copy(this);
                exc = null;
                try {
                    new_cursor.feed_token(new Token(t, ""));
                }
                catch (e) {
                    exc = e;
                    if (e instanceof UnexpectedToken) {
                        // pass
                    }
                    else {
                        throw e;
                    }
                }
                if (!exc) {
                    accepts.add(t);
                }
            }
        }
        return accepts;
    }
    /**
      Resume automated parsing from the current state.
    */
    resume_parse() {
        return this.parser.parse_from_state(this.parser_state);
    }
}
/**
  Same as ``InteractiveParser``, but operations create a new instance instead
    of changing it in-place.

*/
class ImmutableInteractiveParser extends InteractiveParser {
    static get result() {
        return null;
    }
    get result() {
        return this.constructor.result;
    }
    feed_token(token) {
        let c = copy(this);
        c.result = InteractiveParser.feed_token(c, token);
        return c;
    }
    /**
      Try to feed the rest of the lexer state into the parser.
  
          Note that this returns a new ImmutableInteractiveParser and does not feed an '$END' Token
    */
    exhaust_lexer() {
        let cursor = this.as_mutable();
        cursor.exhaust_lexer();
        return cursor.as_immutable();
    }
    /**
      Convert to an ``InteractiveParser``.
    */
    as_mutable() {
        let p = copy(this);
        return new InteractiveParser(p.parser, p.parser_state, p.lexer_state);
    }
}
//
// Lalr Analysis
//
class Action {
    constructor(name) {
        this.name = name;
    }
    repr() {
        return this.toString();
    }
}
var Shift = new Action("Shift");
var Reduce = new Action("Reduce");
class ParseTable {
    constructor(states, start_states, end_states) {
        this.states = states;
        this.start_states = start_states;
        this.end_states = end_states;
    }
    serialize(memo) {
        let tokens = new Enumerator();
        let rules = new Enumerator();
        let states = Object.fromEntries(dict_items(this.states).map(([state, actions]) => [
            state,
            Object.fromEntries(dict_items(actions).map(([token, [action, arg]]) => [
                dict_get(tokens, token),
                action === Reduce ? [1, arg.serialize(memo)] : [0, arg],
            ])),
        ]));
        return {
            tokens: tokens.reversed(),
            states: states,
            start_states: this.start_states,
            end_states: this.end_states,
        };
    }
    static deserialize(data, memo) {
        const cls = this;
        let tokens = data["tokens"];
        let states = Object.fromEntries(dict_items(data["states"]).map(([state, actions]) => [
            state,
            Object.fromEntries(dict_items(actions).map(([token, [action, arg]]) => [
                tokens[token],
                action === 1 ? [Reduce, Rule.deserialize(arg, memo)] : [Shift, arg],
            ])),
        ]));
        return new cls(states, data["start_states"], data["end_states"]);
    }
}
class IntParseTable extends ParseTable {
    static from_ParseTable(parse_table) {
        const cls = this;
        let la;
        let enum_ = [...parse_table.states];
        let state_to_idx = Object.fromEntries(enumerate(enum_).map(([i, s]) => [s, i]));
        let int_states = {};
        for (const [s, la] of dict_items(parse_table.states)) {
            la = Object.fromEntries(dict_items(la).map(([k, v]) => [
                k,
                v[0] === Shift ? [v[0], state_to_idx[v[1]]] : v,
            ]));
            int_states[state_to_idx[s]] = la;
        }
        let start_states = Object.fromEntries(dict_items(parse_table.start_states).map(([start, s]) => [
            start,
            state_to_idx[s],
        ]));
        let end_states = Object.fromEntries(dict_items(parse_table.end_states).map(([start, s]) => [
            start,
            state_to_idx[s],
        ]));
        return new cls(int_states, start_states, end_states);
    }
}
//
// Parser Frontends
//
function _wrap_lexer(lexer_class) {
    let future_interface = (lexer_class && lexer_class["__future_interface__"]) || false;
    if (future_interface) {
        return lexer_class;
    }
    else {
        class CustomLexerWrapper extends Lexer {
            constructor(lexer_conf) {
                super();
                this.lexer = lexer_class(lexer_conf);
            }
            lex(lexer_state, parser_state) {
                return this.lexer.lex(lexer_state.text);
            }
        }
        return CustomLexerWrapper;
    }
}
class MakeParsingFrontend {
    constructor(parser_type, lexer_type) {
        this.parser_type = parser_type;
        this.lexer_type = lexer_type;
    }
    deserialize(data, memo, lexer_conf, callbacks, options) {
        let parser_conf = ParserConf.deserialize(data["parser_conf"], memo);
        let parser = LALR_Parser.deserialize(data["parser"], memo, callbacks, options.debug);
        parser_conf.callbacks = callbacks;
        return new ParsingFrontend({
            lexer_conf: lexer_conf,
            parser_conf: parser_conf,
            options: options,
            parser: parser,
        });
    }
}
// ... Continued later in the module
class ParsingFrontend extends Serialize {
    static get __serialize_fields__() {
        return ["lexer_conf", "parser_conf", "parser", "options"];
    }
    constructor({ lexer_conf, parser_conf, options, parser = null } = {}) {
        super();
        let create_lexer, create_parser;
        this.parser_conf = parser_conf;
        this.lexer_conf = lexer_conf;
        this.options = options;
        // Set-up parser
        if (parser) {
            // From cache
            this.parser = parser;
        }
        else {
            create_parser = {
                lalr: create_lalr_parser,
                earley: create_earley_parser,
                cyk: CYK_FrontEnd,
            }[parser_conf.parser_type];
            this.parser = create_parser(lexer_conf, parser_conf, options);
        }
        // Set-up lexer
        let lexer_type = lexer_conf.lexer_type;
        this.skip_lexer = false;
        if (["dynamic", "dynamic_complete"].includes(lexer_type)) {
            this.skip_lexer = true;
            return;
        }
        if ({
            standard: create_traditional_lexer,
            contextual: create_contextual_lexer,
        } &&
            lexer_type in
                {
                    standard: create_traditional_lexer,
                    contextual: create_contextual_lexer,
                }) {
            create_lexer = {
                standard: create_traditional_lexer,
                contextual: create_contextual_lexer,
            }[lexer_type];
            this.lexer = create_lexer(lexer_conf, this.parser, lexer_conf.postlex);
        }
        else {
            this.lexer = _wrap_lexer(lexer_type)(lexer_conf);
        }
        if (lexer_conf.postlex) {
            this.lexer = new PostLexConnector(this.lexer, lexer_conf.postlex);
        }
    }
    _verify_start(start = null) {
        let start_decls;
        if (start === null) {
            start_decls = this.parser_conf.start;
            if (start_decls.length > 1) {
                throw new ConfigurationError("Lark initialized with more than 1 possible start rule. Must specify which start rule to parse", start_decls);
            }
            [start] = start_decls;
        }
        else if (!(this.parser_conf.start.includes(start))) {
            throw new ConfigurationError(format("Unknown start rule %s. Must be one of %r", start, this.parser_conf.start));
        }
        return start;
    }
    parse(text, start = null, on_error = null) {
        let chosen_start = this._verify_start(start);
        let stream = this.skip_lexer ? text : new LexerThread(this.lexer, text);
        let kw = on_error === null ? {} : { on_error: on_error };
        return this.parser.parse({
            lexer: stream,
            start: chosen_start,
            ...kw,
        });
    }
    parse_interactive(text = null, start = null) {
        let chosen_start = this._verify_start(start);
        if (this.parser_conf.parser_type !== "lalr") {
            throw new ConfigurationError("parse_interactive() currently only works with parser='lalr' ");
        }
        let stream = this.skip_lexer ? text : new LexerThread(this.lexer, text);
        return this.parser.parse_interactive(stream, chosen_start);
    }
}
function get_frontend(parser, lexer) {
    let expected;
    assert_config(parser, ["lalr", "earley", "cyk"]);
    if (!(typeof lexer === "object")) {
        // not custom lexer?
        expected = {
            lalr: ["standard", "contextual"],
            earley: ["standard", "dynamic", "dynamic_complete"],
            cyk: ["standard"],
        }[parser];
        assert_config(lexer, expected, format("Parser %r does not support lexer %%r, expected one of %%s", parser));
    }
    return new MakeParsingFrontend(parser, lexer);
}
function _get_lexer_callbacks(transformer, terminals) {
    let callback;
    let result = {};
    for (const terminal of terminals) {
        callback = (transformer && transformer[terminal.name]) || null;
        if (callback !== null) {
            result[terminal.name] = callback;
        }
    }
    return result;
}
class PostLexConnector {
    constructor(lexer, postlexer) {
        this.lexer = lexer;
        this.postlexer = postlexer;
    }
    make_lexer_state(text) {
        return this.lexer.make_lexer_state(text);
    }
    lex(lexer_state, parser_state) {
        let i = this.lexer.lex(lexer_state, parser_state);
        return this.postlexer.process(i);
    }
}
function create_traditional_lexer(lexer_conf, parser, postlex) {
    return new TraditionalLexer(lexer_conf);
}
function create_contextual_lexer(lexer_conf, parser, postlex) {
    let states = Object.fromEntries(dict_items(parser._parse_table.states).map(([idx, t]) => [
        idx,
        [...dict_keys(t)],
    ]));
    let always_accept = postlex ? postlex.always_accept : [];
    return new ContextualLexer({
        conf: lexer_conf,
        states: states,
        always_accept: always_accept,
    });
}
function create_lalr_parser(lexer_conf, parser_conf, options = null) {
    let debug = options ? options.debug : false;
    return new LALR_Parser({ parser_conf: parser_conf, debug: debug });
}
var create_earley_parser = NotImplemented;
var CYK_FrontEnd = NotImplemented;
//
// Lark
//
/**
  Specifies the options for Lark


*/
class LarkOptions extends Serialize {
    static get OPTIONS_DOC() {
        return `
    **===  General Options  ===**

    start
            The start symbol. Either a string, or a list of strings for multiple possible starts (Default: "start")
    debug
            Display debug information and extra warnings. Use only when debugging (default: False)
            When used with Earley, it generates a forest graph as "sppf.png", if 'dot' is installed.
    transformer
            Applies the transformer to every parse tree (equivalent to applying it after the parse, but faster)
    propagate_positions
            Propagates (line, column, end_line, end_column) attributes into all tree branches.
            Accepts ` `` `False` `` `, ` `` `True` `` `, or a callable, which will filter which nodes to ignore when propagating.
    maybe_placeholders
            When ` `` `True` `` `, the ` `` `[]` `` ` operator returns ` `` `None` `` ` when not matched.

            When ` `` `False` `` `,  ` `` `[]` `` ` behaves like the ` `` `?` `` ` operator, and returns no value at all.
            (default= ` `` `False` `` `. Recommended to set to ` `` `True` `` `)
    cache
            Cache the results of the Lark grammar analysis, for x2 to x3 faster loading. LALR only for now.

            - When ` `` `False` `` `, does nothing (default)
            - When ` `` `True` `` `, caches to a temporary file in the local directory
            - When given a string, caches to the path pointed by the string
    regex
            When True, uses the ` `` `regex` `` ` module instead of the stdlib ` `` `re` `` `.
    g_regex_flags
            Flags that are applied to all terminals (both regex and strings)
    keep_all_tokens
            Prevent the tree builder from automagically removing "punctuation" tokens (default: False)
    tree_class
            Lark will produce trees comprised of instances of this class instead of the default ` `` `lark.Tree` `` `.

    **=== Algorithm Options ===**

    parser
            Decides which parser engine to use. Accepts "earley" or "lalr". (Default: "earley").
            (there is also a "cyk" option for legacy)
    lexer
            Decides whether or not to use a lexer stage

            - "auto" (default): Choose for me based on the parser
            - "standard": Use a standard lexer
            - "contextual": Stronger lexer (only works with parser="lalr")
            - "dynamic": Flexible and powerful (only with parser="earley")
            - "dynamic_complete": Same as dynamic, but tries *every* variation of tokenizing possible.
    ambiguity
            Decides how to handle ambiguity in the parse. Only relevant if parser="earley"

            - "resolve": The parser will automatically choose the simplest derivation
              (it chooses consistently: greedy for tokens, non-greedy for rules)
            - "explicit": The parser will return all derivations wrapped in "_ambig" tree nodes (i.e. a forest).
            - "forest": The parser will return the root of the shared packed parse forest.

    **=== Misc. / Domain Specific Options ===**

    postlex
            Lexer post-processing (Default: None) Only works with the standard and contextual lexers.
    priority
            How priorities should be evaluated - auto, none, normal, invert (Default: auto)
    lexer_callbacks
            Dictionary of callbacks for the lexer. May alter tokens during lexing. Use with caution.
    use_bytes
            Accept an input of type ` `` `bytes` `` ` instead of ` `` `str` `` ` (Python 3 only).
    edit_terminals
            A callback for editing the terminals before parse.
    import_paths
            A List of either paths or loader functions to specify from where grammars are imported
    source_path
            Override the source of from where the grammar was loaded. Useful for relative imports and unconventional grammar loading
    **=== End Options ===**
    `;
    }
    get OPTIONS_DOC() {
        return this.constructor.OPTIONS_DOC;
    }
    // Adding a new option needs to be done in multiple places:
    // - In the dictionary below. This is the primary truth of which options `Lark.__init__` accepts
    // - In the docstring above. It is used both for the docstring of `LarkOptions` and `Lark`, and in readthedocs
    // - In `lark-stubs/lark.pyi`:
    //   - As attribute to `LarkOptions`
    //   - As parameter to `Lark.__init__`
    // - Potentially in `_LOAD_ALLOWED_OPTIONS` below this class, when the option doesn't change how the grammar is loaded
    // - Potentially in `lark.tools.__init__`, if it makes sense, and it can easily be passed as a cmd argument
    static get _defaults() {
        return {
            debug: false,
            keep_all_tokens: false,
            tree_class: null,
            cache: false,
            postlex: null,
            parser: "earley",
            lexer: "auto",
            transformer: null,
            start: "start",
            priority: "auto",
            ambiguity: "auto",
            regex: false,
            propagate_positions: false,
            lexer_callbacks: {},
            maybe_placeholders: false,
            edit_terminals: null,
            g_regex_flags: '',
            use_bytes: false,
            import_paths: [],
            source_path: null,
        };
    }
    get _defaults() {
        return this.constructor._defaults;
    }
    constructor(options_dict) {
        super();
        let value;
        let o = dict(options_dict);
        let options = this;
        for (const [name, default_] of dict_items(this.constructor._defaults)) {
            if (name in o) {
                value = dict_pop(o, name);
                if (typeof default_ === "boolean" &&
                    !["cache", "use_bytes", "propagate_positions"].includes(name)) {
                    value = bool(value);
                }
            }
            else {
                value = default_;
            }
            options[name] = value;
        }
        if (typeof options["start"] === "string") {
            options["start"] = [options["start"]];
        }
        this["options"] = options;
        assert_config(this.parser, ["earley", "lalr", "cyk", null]);
        if (this.parser === "earley" && this.transformer) {
            throw new ConfigurationError("Cannot specify an embedded transformer when using the Earley algorithm. " +
                "Please use your transformer on the resulting parse tree, or use a different algorithm (i.e. LALR)");
        }
        if (Object.keys(o).length) {
            throw new ConfigurationError(format("Unknown options: %s", dict_keys(o)));
        }
    }
    serialize(memo) {
        return this.options;
    }
    static deserialize(data, memo) {
        const cls = this;
        return new cls(data);
    }
}
// Options that can be passed to the Lark parser, even when it was loaded from cache/standalone.
// These option are only used outside of `load_grammar`.
var _LOAD_ALLOWED_OPTIONS = new Set([
    "postlex",
    "transformer",
    "lexer_callbacks",
    "use_bytes",
    "debug",
    "g_regex_flags",
    "regex",
    "propagate_positions",
    "tree_class",
]);
var _VALID_PRIORITY_OPTIONS = ["auto", "normal", "invert", null];
var _VALID_AMBIGUITY_OPTIONS = ["auto", "resolve", "explicit", "forest"];
class PostLex extends ABC {
    process(stream) {
        return stream;
    }
    static get always_accept() {
        return [];
    }
    get always_accept() {
        return this.constructor.always_accept;
    }
}
/**
  Main interface for the library.

    It's mostly a thin wrapper for the many different parsers, and for the tree constructor.

    Parameters:
        grammar: a string or file-object containing the grammar spec (using Lark's ebnf syntax)
        options: a dictionary controlling various aspects of Lark.

    Example:
        >>> Lark(r'''start: "foo" ''')
        Lark(...)

*/
class Lark extends Serialize {
    static get __serialize_fields__() {
        return ["parser", "rules", "options"];
    }
    _build_lexer(dont_ignore = false) {
        let lexer_conf = this.lexer_conf;
        if (dont_ignore) {
            lexer_conf = copy(lexer_conf);
            lexer_conf.ignore = [];
        }
        return new TraditionalLexer(lexer_conf);
    }
    _prepare_callbacks() {
        this._callbacks = new Map();
        // we don't need these callbacks if we aren't building a tree
        if (this.options.ambiguity !== "forest") {
            this._parse_tree_builder = new ParseTreeBuilder(this.rules, this.options.tree_class || make_constructor(Tree), this.options.propagate_positions, this.options.parser !== "lalr" && this.options.ambiguity === "explicit", this.options.maybe_placeholders);
            this._callbacks = this._parse_tree_builder.create_callback(this.options.transformer);
        }
        dict_update(this._callbacks, _get_lexer_callbacks(this.options.transformer, this.terminals));
    }
    /**
      Saves the instance into the given file object
  
          Useful for caching and multiprocessing.
  
    */
    /**
      Loads an instance from the given file object
  
          Useful for caching and multiprocessing.
  
    */
    _deserialize_lexer_conf(data, memo, options) {
        let lexer_conf = LexerConf.deserialize(data["lexer_conf"], memo);
        lexer_conf.callbacks = options.lexer_callbacks || {};
        lexer_conf.re_module = options.regex ? regex : re;
        lexer_conf.use_bytes = options.use_bytes;
        lexer_conf.g_regex_flags = options.g_regex_flags || '';
        lexer_conf.skip_validation = true;
        lexer_conf.postlex = options.postlex;
        return lexer_conf;
    }
    _load({ f, ...kwargs } = {}) {
        let d;
        if (is_dict(f)) {
            d = f;
        }
        else {
            d = pickle.load(f);
        }
        let memo_json = d["memo"];
        let data = d["data"];
        let memo = SerializeMemoizer.deserialize(memo_json, { Rule: Rule, TerminalDef: TerminalDef }, {});
        let options = dict(data["options"]);
        // if (
        //   (new Set(kwargs) - _LOAD_ALLOWED_OPTIONS) &
        //   new Set(LarkOptions._defaults)
        // ) {
        //   throw new ConfigurationError(
        //     "Some options are not allowed when loading a Parser: {}".format(
        //       new Set(kwargs) - _LOAD_ALLOWED_OPTIONS
        //     )
        //   );
        // }
        dict_update(options, kwargs);
        this.options = LarkOptions.deserialize(options, memo);
        this.rules = data["rules"].map((r) => Rule.deserialize(r, memo));
        this.source_path = "<deserialized>";
        let parser_class = get_frontend(this.options.parser, this.options.lexer);
        this.lexer_conf = this._deserialize_lexer_conf(data["parser"], memo, this.options);
        this.terminals = this.lexer_conf.terminals;
        this._prepare_callbacks();
        this._terminals_dict = Object.fromEntries(this.terminals.map((t) => [t.name, t]));
        this.parser = parser_class.deserialize(data["parser"], memo, this.lexer_conf, this._callbacks, this.options);
        return this;
    }
    static _load_from_dict({ data, memo, ...kwargs } = {}) {
        const cls = this;
        let inst = new_object(cls);
        return inst._load({
            f: { data: data, memo: memo },
            ...kwargs,
        });
    }
    /**
      Create an instance of Lark with the grammar given by its filename
  
          If ``rel_to`` is provided, the function will find the grammar filename in relation to it.
  
          Example:
  
              >>> Lark.open("grammar_file.lark", rel_to=__file__, parser="lalr")
              Lark(...)
  
  
    */
    /**
      Create an instance of Lark with the grammar loaded from within the package `package`.
          This allows grammar loading from zipapps.
  
          Imports in the grammar will use the `package` and `search_paths` provided, through `FromPackageLoader`
  
          Example:
  
              Lark.open_from_package(__name__, "example.lark", ("grammars",), parser=...)
  
    */
    repr() {
        return format("Lark(open(%r), parser=%r, lexer=%r, ...)", this.source_path, this.options.parser, this.options.lexer);
    }
    /**
      Only lex (and postlex) the text, without parsing it. Only relevant when lexer='standard'
  
          When dont_ignore=True, the lexer will return all tokens, even those marked for %ignore.
  
    */
    lex(text, dont_ignore = false) {
        let lexer;
        if (!("lexer" in this) || dont_ignore) {
            lexer = this._build_lexer(dont_ignore);
        }
        else {
            lexer = this.lexer;
        }
        let lexer_thread = new LexerThread(lexer, text);
        let stream = lexer_thread.lex(null);
        if (this.options.postlex) {
            return this.options.postlex.process(stream);
        }
        return stream;
    }
    /**
      Get information about a terminal
    */
    get_terminal(name) {
        return this._terminals_dict[name];
    }
    /**
      Start an interactive parsing session.
  
          Parameters:
              text (str, optional): Text to be parsed. Required for ``resume_parse()``.
              start (str, optional): Start symbol
  
          Returns:
              A new InteractiveParser instance.
  
          See Also: ``Lark.parse()``
  
    */
    parse_interactive(text = null, start = null) {
        return this.parser.parse_interactive({
            unknown_param_0: text,
            start: start,
        });
    }
    /**
      Parse the given text, according to the options provided.
  
          Parameters:
              text (str): Text to be parsed.
              start (str, optional): Required if Lark was given multiple possible start symbols (using the start option).
              on_error (function, optional): if provided, will be called on UnexpectedToken error. Return true to resume parsing.
                  LALR only. See examples/advanced/error_handling.py for an example of how to use on_error.
  
          Returns:
              If a transformer is supplied to ``__init__``, returns whatever is the
              result of the transformation. Otherwise, returns a Tree instance.
  
  
    */
    parse(text, start = null, on_error = null) {
        return this.parser.parse(text, start, on_error);
    }
}
//
// Indenter
//
class DedentError extends LarkError {
}
class Indenter extends PostLex {
    constructor() {
        super();
        this.paren_level = null;
        this.indent_level = null;
    }
    *handle_NL(token) {
        if (this.paren_level > 0) {
            return;
        }
        yield token;
        let indent_str = rsplit(token.value, "\n", 1)[1];
        // Tabs and spaces
        let indent = str_count(indent_str, " ") + str_count(indent_str, "\t") * this.tab_len;
        if (indent > last_item(this.indent_level)) {
            this.indent_level.push(indent);
            yield Token.new_borrow_pos(this.INDENT_type, indent_str, token);
        }
        else {
            while (indent < last_item(this.indent_level)) {
                this.indent_level.pop();
                yield Token.new_borrow_pos(this.DEDENT_type, indent_str, token);
            }
            if (indent !== last_item(this.indent_level)) {
                throw new DedentError(format("Unexpected dedent to column %s. Expected dedent to %s", indent, last_item(this.indent_level)));
            }
        }
    }
    *_process(stream) {
        for (const token of stream) {
            if (token.type === this.NL_type) {
                for (const t of this.handle_NL(token)) {
                    yield t;
                }
            }
            else {
                yield token;
            }
            if (this.OPEN_PAREN_types.includes(token.type)) {
                this.paren_level += 1;
            }
            else if (this.CLOSE_PAREN_types.includes(token.type)) {
                this.paren_level -= 1;
            }
        }
        while (this.indent_level.length > 1) {
            this.indent_level.pop();
            yield new Token(this.DEDENT_type, "");
        }
    }
    process(stream) {
        this.paren_level = 0;
        this.indent_level = [0];
        return this._process(stream);
    }
    // XXX Hack for ContextualLexer. Maybe there's a more elegant solution?
    get always_accept() {
        return [this.NL_type];
    }
}
export { LarkError, ConfigurationError, GrammarError, ParseError, LexError, UnexpectedInput, UnexpectedEOF, UnexpectedCharacters, UnexpectedToken, VisitError, Meta, Tree, Discard, Transformer, Transformer_InPlace, Transformer_NonRecursive, Transformer_InPlaceRecursive, VisitorBase, Visitor, Visitor_Recursive, Interpreter, Symbol, Terminal, NonTerminal, RuleOptions, Rule, Pattern, PatternStr, PatternRE, TerminalDef, Token, Lexer, LexerConf, ParserConf, InteractiveParser, ImmutableInteractiveParser, PostLex, Lark, DedentError, Indenter, get_parser, };
var DATA = {
    "parser": {
        "lexer_conf": {
            "terminals": [
                {
                    "@": 0
                },
                {
                    "@": 1
                },
                {
                    "@": 2
                },
                {
                    "@": 3
                },
                {
                    "@": 4
                },
                {
                    "@": 5
                },
                {
                    "@": 6
                },
                {
                    "@": 7
                },
                {
                    "@": 8
                },
                {
                    "@": 9
                },
                {
                    "@": 10
                },
                {
                    "@": 11
                },
                {
                    "@": 12
                },
                {
                    "@": 13
                },
                {
                    "@": 14
                },
                {
                    "@": 15
                },
                {
                    "@": 16
                },
                {
                    "@": 17
                }
            ],
            "ignore": [
                "WS"
            ],
            "g_regex_flags": 0,
            "use_bytes": false,
            "lexer_type": "contextual",
            "__type__": "LexerConf"
        },
        "parser_conf": {
            "rules": [
                {
                    "@": 18
                },
                {
                    "@": 19
                },
                {
                    "@": 20
                },
                {
                    "@": 21
                },
                {
                    "@": 22
                },
                {
                    "@": 23
                },
                {
                    "@": 24
                },
                {
                    "@": 25
                },
                {
                    "@": 26
                },
                {
                    "@": 27
                },
                {
                    "@": 28
                },
                {
                    "@": 29
                },
                {
                    "@": 30
                },
                {
                    "@": 31
                },
                {
                    "@": 32
                },
                {
                    "@": 33
                },
                {
                    "@": 34
                },
                {
                    "@": 35
                },
                {
                    "@": 36
                },
                {
                    "@": 37
                },
                {
                    "@": 38
                },
                {
                    "@": 39
                },
                {
                    "@": 40
                },
                {
                    "@": 41
                },
                {
                    "@": 42
                },
                {
                    "@": 43
                },
                {
                    "@": 44
                },
                {
                    "@": 45
                },
                {
                    "@": 46
                },
                {
                    "@": 47
                },
                {
                    "@": 48
                },
                {
                    "@": 49
                },
                {
                    "@": 50
                },
                {
                    "@": 51
                },
                {
                    "@": 52
                },
                {
                    "@": 53
                },
                {
                    "@": 54
                },
                {
                    "@": 55
                },
                {
                    "@": 56
                },
                {
                    "@": 57
                },
                {
                    "@": 58
                },
                {
                    "@": 59
                },
                {
                    "@": 60
                },
                {
                    "@": 61
                },
                {
                    "@": 62
                },
                {
                    "@": 63
                },
                {
                    "@": 64
                },
                {
                    "@": 65
                },
                {
                    "@": 66
                },
                {
                    "@": 67
                },
                {
                    "@": 68
                },
                {
                    "@": 69
                },
                {
                    "@": 70
                },
                {
                    "@": 71
                },
                {
                    "@": 72
                },
                {
                    "@": 73
                },
                {
                    "@": 74
                },
                {
                    "@": 75
                },
                {
                    "@": 76
                },
                {
                    "@": 77
                },
                {
                    "@": 78
                },
                {
                    "@": 79
                }
            ],
            "start": [
                "start"
            ],
            "parser_type": "lalr",
            "__type__": "ParserConf"
        },
        "parser": {
            "tokens": {
                "0": "WORD",
                "1": "word",
                "2": "__temps_plus_5",
                "3": "VBAR",
                "4": "PARAM",
                "5": "$END",
                "6": "__ANON_1",
                "7": "__ANON_3",
                "8": "__ANON_2",
                "9": "LSQB",
                "10": "symbol",
                "11": "SYMBOL",
                "12": "primary",
                "13": "STRING",
                "14": "block",
                "15": "NUMBER",
                "16": "scalar",
                "17": "map",
                "18": "array",
                "19": "string",
                "20": "number",
                "21": "KEYWORD",
                "22": "RBRACE",
                "23": "OP",
                "24": "DOT",
                "25": "RSQB",
                "26": "SEMICOLON",
                "27": "__map_star_6",
                "28": "kw",
                "29": "mappair",
                "30": "op",
                "31": "keypairs",
                "32": "__keypairs_plus_1",
                "33": "keypair",
                "34": "__exprs_star_0",
                "35": "unary",
                "36": "binary",
                "37": "send",
                "38": "assign",
                "39": "expr",
                "40": "keyword",
                "41": "__array_star_3",
                "42": "exprs",
                "43": "__ANON_0",
                "44": "cascades",
                "45": "__cascades_plus_2",
                "46": "param",
                "47": "params",
                "48": "__params_plus_4",
                "49": "temps",
                "50": "start",
                "51": "repl"
            },
            "states": {
                "0": {
                    "0": [
                        0,
                        24
                    ],
                    "1": [
                        0,
                        68
                    ],
                    "2": [
                        0,
                        87
                    ]
                },
                "1": {
                    "3": [
                        1,
                        {
                            "@": 74
                        }
                    ],
                    "4": [
                        1,
                        {
                            "@": 74
                        }
                    ]
                },
                "2": {
                    "5": [
                        1,
                        {
                            "@": 18
                        }
                    ]
                },
                "3": {
                    "6": [
                        0,
                        22
                    ],
                    "7": [
                        0,
                        20
                    ],
                    "8": [
                        0,
                        11
                    ],
                    "9": [
                        0,
                        36
                    ],
                    "10": [
                        0,
                        41
                    ],
                    "11": [
                        0,
                        5
                    ],
                    "12": [
                        0,
                        47
                    ],
                    "13": [
                        0,
                        12
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "14": [
                        0,
                        61
                    ],
                    "15": [
                        0,
                        39
                    ],
                    "1": [
                        0,
                        77
                    ],
                    "16": [
                        0,
                        72
                    ],
                    "17": [
                        0,
                        28
                    ],
                    "18": [
                        0,
                        76
                    ],
                    "19": [
                        0,
                        34
                    ],
                    "20": [
                        0,
                        75
                    ]
                },
                "4": {
                    "21": [
                        1,
                        {
                            "@": 49
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 49
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 49
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 49
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 49
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 49
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 49
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 49
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 49
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 49
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 49
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 49
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 49
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 49
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 49
                        }
                    ]
                },
                "5": {
                    "21": [
                        1,
                        {
                            "@": 64
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 64
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 64
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 64
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 64
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 64
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 64
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 64
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 64
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 64
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 64
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 64
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 64
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 64
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 64
                        }
                    ]
                },
                "6": {
                    "5": [
                        1,
                        {
                            "@": 20
                        }
                    ]
                },
                "7": {
                    "21": [
                        1,
                        {
                            "@": 51
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 51
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 51
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 51
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 51
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 51
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 51
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 51
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 51
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 51
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 51
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 51
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 51
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 51
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 51
                        }
                    ]
                },
                "8": {
                    "21": [
                        1,
                        {
                            "@": 56
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 56
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 56
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 56
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 56
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 56
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 56
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 56
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 56
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 56
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 56
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 56
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 56
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 56
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 56
                        }
                    ]
                },
                "9": {
                    "5": [
                        1,
                        {
                            "@": 67
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 67
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 67
                        }
                    ]
                },
                "10": {
                    "25": [
                        1,
                        {
                            "@": 71
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 71
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 71
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 71
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 71
                        }
                    ]
                },
                "11": {
                    "27": [
                        0,
                        65
                    ],
                    "22": [
                        0,
                        8
                    ],
                    "21": [
                        0,
                        40
                    ],
                    "28": [
                        0,
                        3
                    ],
                    "29": [
                        0,
                        44
                    ]
                },
                "12": {
                    "21": [
                        1,
                        {
                            "@": 63
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 63
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 63
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 63
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 63
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 63
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 63
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 63
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 63
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 63
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 63
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 63
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 63
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 63
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 63
                        }
                    ]
                },
                "13": {
                    "30": [
                        0,
                        38
                    ],
                    "31": [
                        0,
                        35
                    ],
                    "23": [
                        0,
                        64
                    ],
                    "32": [
                        0,
                        84
                    ],
                    "33": [
                        0,
                        60
                    ],
                    "21": [
                        0,
                        40
                    ],
                    "28": [
                        0,
                        55
                    ],
                    "22": [
                        1,
                        {
                            "@": 31
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 31
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 31
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 31
                        }
                    ]
                },
                "14": {
                    "21": [
                        1,
                        {
                            "@": 38
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 38
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 38
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 38
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 38
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 38
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 38
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 38
                        }
                    ]
                },
                "15": {
                    "22": [
                        1,
                        {
                            "@": 27
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 27
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 27
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 27
                        }
                    ]
                },
                "16": {
                    "22": [
                        0,
                        54
                    ]
                },
                "17": {
                    "22": [
                        1,
                        {
                            "@": 28
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 28
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 28
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 28
                        }
                    ]
                },
                "18": {
                    "34": [
                        0,
                        78
                    ],
                    "24": [
                        0,
                        88
                    ],
                    "5": [
                        1,
                        {
                            "@": 24
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 24
                        }
                    ]
                },
                "19": {
                    "5": [
                        1,
                        {
                            "@": 66
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 66
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 66
                        }
                    ]
                },
                "20": {
                    "7": [
                        0,
                        20
                    ],
                    "8": [
                        0,
                        11
                    ],
                    "9": [
                        0,
                        36
                    ],
                    "35": [
                        0,
                        25
                    ],
                    "36": [
                        0,
                        13
                    ],
                    "6": [
                        0,
                        22
                    ],
                    "1": [
                        0,
                        30
                    ],
                    "37": [
                        0,
                        15
                    ],
                    "14": [
                        0,
                        61
                    ],
                    "16": [
                        0,
                        72
                    ],
                    "18": [
                        0,
                        76
                    ],
                    "20": [
                        0,
                        75
                    ],
                    "38": [
                        0,
                        67
                    ],
                    "10": [
                        0,
                        41
                    ],
                    "11": [
                        0,
                        5
                    ],
                    "13": [
                        0,
                        12
                    ],
                    "39": [
                        0,
                        16
                    ],
                    "12": [
                        0,
                        14
                    ],
                    "40": [
                        0,
                        17
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "15": [
                        0,
                        39
                    ],
                    "17": [
                        0,
                        28
                    ],
                    "19": [
                        0,
                        34
                    ]
                },
                "21": {
                    "33": [
                        0,
                        60
                    ],
                    "31": [
                        0,
                        10
                    ],
                    "21": [
                        0,
                        40
                    ],
                    "32": [
                        0,
                        84
                    ],
                    "28": [
                        0,
                        55
                    ]
                },
                "22": {
                    "6": [
                        0,
                        22
                    ],
                    "7": [
                        0,
                        20
                    ],
                    "8": [
                        0,
                        11
                    ],
                    "9": [
                        0,
                        36
                    ],
                    "10": [
                        0,
                        41
                    ],
                    "11": [
                        0,
                        5
                    ],
                    "12": [
                        0,
                        80
                    ],
                    "13": [
                        0,
                        12
                    ],
                    "41": [
                        0,
                        70
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "14": [
                        0,
                        61
                    ],
                    "15": [
                        0,
                        39
                    ],
                    "1": [
                        0,
                        77
                    ],
                    "16": [
                        0,
                        72
                    ],
                    "22": [
                        0,
                        82
                    ],
                    "17": [
                        0,
                        28
                    ],
                    "18": [
                        0,
                        76
                    ],
                    "19": [
                        0,
                        34
                    ],
                    "20": [
                        0,
                        75
                    ]
                },
                "23": {
                    "7": [
                        0,
                        20
                    ],
                    "8": [
                        0,
                        11
                    ],
                    "9": [
                        0,
                        36
                    ],
                    "35": [
                        0,
                        25
                    ],
                    "36": [
                        0,
                        13
                    ],
                    "6": [
                        0,
                        22
                    ],
                    "1": [
                        0,
                        30
                    ],
                    "37": [
                        0,
                        15
                    ],
                    "14": [
                        0,
                        61
                    ],
                    "16": [
                        0,
                        72
                    ],
                    "18": [
                        0,
                        76
                    ],
                    "20": [
                        0,
                        75
                    ],
                    "38": [
                        0,
                        67
                    ],
                    "10": [
                        0,
                        41
                    ],
                    "39": [
                        0,
                        18
                    ],
                    "11": [
                        0,
                        5
                    ],
                    "13": [
                        0,
                        12
                    ],
                    "12": [
                        0,
                        14
                    ],
                    "40": [
                        0,
                        17
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "15": [
                        0,
                        39
                    ],
                    "42": [
                        0,
                        86
                    ],
                    "17": [
                        0,
                        28
                    ],
                    "19": [
                        0,
                        34
                    ]
                },
                "24": {
                    "0": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "3": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "21": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "43": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 65
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 65
                        }
                    ]
                },
                "25": {
                    "0": [
                        0,
                        24
                    ],
                    "1": [
                        0,
                        26
                    ],
                    "21": [
                        1,
                        {
                            "@": 36
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 36
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 36
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 36
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 36
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 36
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 36
                        }
                    ]
                },
                "26": {
                    "21": [
                        1,
                        {
                            "@": 37
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 37
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 37
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 37
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 37
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 37
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 37
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 37
                        }
                    ]
                },
                "27": {},
                "28": {
                    "21": [
                        1,
                        {
                            "@": 41
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 41
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 41
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 41
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 41
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 41
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 41
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 41
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 41
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 41
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 41
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 41
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 41
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 41
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 41
                        }
                    ]
                },
                "29": {
                    "25": [
                        0,
                        4
                    ]
                },
                "30": {
                    "43": [
                        0,
                        37
                    ],
                    "21": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 46
                        }
                    ]
                },
                "31": {
                    "21": [
                        1,
                        {
                            "@": 50
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 50
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 50
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 50
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 50
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 50
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 50
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 50
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 50
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 50
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 50
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 50
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 50
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 50
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 50
                        }
                    ]
                },
                "32": {
                    "22": [
                        1,
                        {
                            "@": 73
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 73
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 73
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 73
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 73
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 73
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 73
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 73
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 73
                        }
                    ]
                },
                "33": {
                    "25": [
                        1,
                        {
                            "@": 70
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 70
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 70
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 70
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 70
                        }
                    ]
                },
                "34": {
                    "21": [
                        1,
                        {
                            "@": 44
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 44
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 44
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 44
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 44
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 44
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 44
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 44
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 44
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 44
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 44
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 44
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 44
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 44
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 44
                        }
                    ]
                },
                "35": {
                    "44": [
                        0,
                        51
                    ],
                    "26": [
                        0,
                        66
                    ],
                    "45": [
                        0,
                        79
                    ],
                    "22": [
                        1,
                        {
                            "@": 30
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 30
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 30
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 30
                        }
                    ]
                },
                "36": {
                    "7": [
                        0,
                        20
                    ],
                    "8": [
                        0,
                        11
                    ],
                    "9": [
                        0,
                        36
                    ],
                    "35": [
                        0,
                        25
                    ],
                    "46": [
                        0,
                        1
                    ],
                    "4": [
                        0,
                        48
                    ],
                    "36": [
                        0,
                        13
                    ],
                    "6": [
                        0,
                        22
                    ],
                    "1": [
                        0,
                        30
                    ],
                    "37": [
                        0,
                        15
                    ],
                    "3": [
                        0,
                        0
                    ],
                    "14": [
                        0,
                        61
                    ],
                    "16": [
                        0,
                        72
                    ],
                    "47": [
                        0,
                        57
                    ],
                    "48": [
                        0,
                        63
                    ],
                    "18": [
                        0,
                        76
                    ],
                    "20": [
                        0,
                        75
                    ],
                    "38": [
                        0,
                        67
                    ],
                    "10": [
                        0,
                        41
                    ],
                    "39": [
                        0,
                        18
                    ],
                    "11": [
                        0,
                        5
                    ],
                    "13": [
                        0,
                        12
                    ],
                    "49": [
                        0,
                        81
                    ],
                    "12": [
                        0,
                        14
                    ],
                    "40": [
                        0,
                        17
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "15": [
                        0,
                        39
                    ],
                    "42": [
                        0,
                        71
                    ],
                    "17": [
                        0,
                        28
                    ],
                    "19": [
                        0,
                        34
                    ]
                },
                "37": {
                    "7": [
                        0,
                        20
                    ],
                    "8": [
                        0,
                        11
                    ],
                    "9": [
                        0,
                        36
                    ],
                    "35": [
                        0,
                        25
                    ],
                    "36": [
                        0,
                        13
                    ],
                    "6": [
                        0,
                        22
                    ],
                    "1": [
                        0,
                        30
                    ],
                    "37": [
                        0,
                        15
                    ],
                    "14": [
                        0,
                        61
                    ],
                    "16": [
                        0,
                        72
                    ],
                    "18": [
                        0,
                        76
                    ],
                    "20": [
                        0,
                        75
                    ],
                    "10": [
                        0,
                        41
                    ],
                    "11": [
                        0,
                        5
                    ],
                    "38": [
                        0,
                        43
                    ],
                    "13": [
                        0,
                        12
                    ],
                    "12": [
                        0,
                        14
                    ],
                    "40": [
                        0,
                        17
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "15": [
                        0,
                        39
                    ],
                    "17": [
                        0,
                        28
                    ],
                    "19": [
                        0,
                        34
                    ]
                },
                "38": {
                    "6": [
                        0,
                        22
                    ],
                    "7": [
                        0,
                        20
                    ],
                    "8": [
                        0,
                        11
                    ],
                    "9": [
                        0,
                        36
                    ],
                    "10": [
                        0,
                        41
                    ],
                    "11": [
                        0,
                        5
                    ],
                    "35": [
                        0,
                        53
                    ],
                    "13": [
                        0,
                        12
                    ],
                    "12": [
                        0,
                        14
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "14": [
                        0,
                        61
                    ],
                    "15": [
                        0,
                        39
                    ],
                    "1": [
                        0,
                        77
                    ],
                    "16": [
                        0,
                        72
                    ],
                    "17": [
                        0,
                        28
                    ],
                    "18": [
                        0,
                        76
                    ],
                    "19": [
                        0,
                        34
                    ],
                    "20": [
                        0,
                        75
                    ]
                },
                "39": {
                    "21": [
                        1,
                        {
                            "@": 60
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 60
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 60
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 60
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 60
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 60
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 60
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 60
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 60
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 60
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 60
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 60
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 60
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 60
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 60
                        }
                    ]
                },
                "40": {
                    "15": [
                        1,
                        {
                            "@": 59
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 59
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 59
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 59
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 59
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 59
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 59
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 59
                        }
                    ]
                },
                "41": {
                    "21": [
                        1,
                        {
                            "@": 45
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 45
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 45
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 45
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 45
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 45
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 45
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 45
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 45
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 45
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 45
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 45
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 45
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 45
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 45
                        }
                    ]
                },
                "42": {
                    "30": [
                        0,
                        38
                    ],
                    "23": [
                        0,
                        64
                    ],
                    "21": [
                        1,
                        {
                            "@": 33
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 33
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 33
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 33
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 33
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 33
                        }
                    ]
                },
                "43": {
                    "22": [
                        1,
                        {
                            "@": 26
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 26
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 26
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 26
                        }
                    ]
                },
                "44": {
                    "21": [
                        1,
                        {
                            "@": 78
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 78
                        }
                    ]
                },
                "45": {
                    "7": [
                        0,
                        20
                    ],
                    "8": [
                        0,
                        11
                    ],
                    "9": [
                        0,
                        36
                    ],
                    "35": [
                        0,
                        25
                    ],
                    "36": [
                        0,
                        13
                    ],
                    "6": [
                        0,
                        22
                    ],
                    "1": [
                        0,
                        30
                    ],
                    "37": [
                        0,
                        15
                    ],
                    "14": [
                        0,
                        61
                    ],
                    "16": [
                        0,
                        72
                    ],
                    "39": [
                        0,
                        9
                    ],
                    "18": [
                        0,
                        76
                    ],
                    "20": [
                        0,
                        75
                    ],
                    "38": [
                        0,
                        67
                    ],
                    "10": [
                        0,
                        41
                    ],
                    "11": [
                        0,
                        5
                    ],
                    "13": [
                        0,
                        12
                    ],
                    "12": [
                        0,
                        14
                    ],
                    "40": [
                        0,
                        17
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "15": [
                        0,
                        39
                    ],
                    "17": [
                        0,
                        28
                    ],
                    "19": [
                        0,
                        34
                    ],
                    "5": [
                        1,
                        {
                            "@": 21
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 21
                        }
                    ]
                },
                "46": {
                    "50": [
                        0,
                        27
                    ],
                    "7": [
                        0,
                        20
                    ],
                    "8": [
                        0,
                        11
                    ],
                    "9": [
                        0,
                        36
                    ],
                    "35": [
                        0,
                        25
                    ],
                    "36": [
                        0,
                        13
                    ],
                    "6": [
                        0,
                        22
                    ],
                    "49": [
                        0,
                        23
                    ],
                    "1": [
                        0,
                        30
                    ],
                    "37": [
                        0,
                        15
                    ],
                    "3": [
                        0,
                        0
                    ],
                    "14": [
                        0,
                        61
                    ],
                    "16": [
                        0,
                        72
                    ],
                    "18": [
                        0,
                        76
                    ],
                    "20": [
                        0,
                        75
                    ],
                    "38": [
                        0,
                        67
                    ],
                    "10": [
                        0,
                        41
                    ],
                    "39": [
                        0,
                        18
                    ],
                    "11": [
                        0,
                        5
                    ],
                    "42": [
                        0,
                        6
                    ],
                    "13": [
                        0,
                        12
                    ],
                    "40": [
                        0,
                        17
                    ],
                    "12": [
                        0,
                        14
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "15": [
                        0,
                        39
                    ],
                    "17": [
                        0,
                        28
                    ],
                    "19": [
                        0,
                        34
                    ],
                    "51": [
                        0,
                        2
                    ]
                },
                "47": {
                    "21": [
                        1,
                        {
                            "@": 57
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 57
                        }
                    ]
                },
                "48": {
                    "3": [
                        1,
                        {
                            "@": 62
                        }
                    ],
                    "4": [
                        1,
                        {
                            "@": 62
                        }
                    ]
                },
                "49": {
                    "21": [
                        1,
                        {
                            "@": 69
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 69
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 69
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 69
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 69
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 69
                        }
                    ]
                },
                "50": {
                    "21": [
                        1,
                        {
                            "@": 79
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 79
                        }
                    ]
                },
                "51": {
                    "22": [
                        1,
                        {
                            "@": 29
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 29
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 29
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 29
                        }
                    ]
                },
                "52": {
                    "0": [
                        1,
                        {
                            "@": 77
                        }
                    ],
                    "3": [
                        1,
                        {
                            "@": 77
                        }
                    ]
                },
                "53": {
                    "1": [
                        0,
                        26
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "21": [
                        1,
                        {
                            "@": 35
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 35
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 35
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 35
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 35
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 35
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 35
                        }
                    ]
                },
                "54": {
                    "21": [
                        1,
                        {
                            "@": 58
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 58
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 58
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 58
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 58
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 58
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 58
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 58
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 58
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 58
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 58
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 58
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 58
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 58
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 58
                        }
                    ]
                },
                "55": {
                    "6": [
                        0,
                        22
                    ],
                    "7": [
                        0,
                        20
                    ],
                    "8": [
                        0,
                        11
                    ],
                    "9": [
                        0,
                        36
                    ],
                    "10": [
                        0,
                        41
                    ],
                    "11": [
                        0,
                        5
                    ],
                    "35": [
                        0,
                        25
                    ],
                    "13": [
                        0,
                        12
                    ],
                    "36": [
                        0,
                        42
                    ],
                    "12": [
                        0,
                        14
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "14": [
                        0,
                        61
                    ],
                    "15": [
                        0,
                        39
                    ],
                    "1": [
                        0,
                        77
                    ],
                    "16": [
                        0,
                        72
                    ],
                    "17": [
                        0,
                        28
                    ],
                    "18": [
                        0,
                        76
                    ],
                    "19": [
                        0,
                        34
                    ],
                    "20": [
                        0,
                        75
                    ]
                },
                "56": {
                    "21": [
                        1,
                        {
                            "@": 52
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 52
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 52
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 52
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 52
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 52
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 52
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 52
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 52
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 52
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 52
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 52
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 52
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 52
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 52
                        }
                    ]
                },
                "57": {
                    "7": [
                        0,
                        20
                    ],
                    "8": [
                        0,
                        11
                    ],
                    "9": [
                        0,
                        36
                    ],
                    "35": [
                        0,
                        25
                    ],
                    "42": [
                        0,
                        58
                    ],
                    "49": [
                        0,
                        85
                    ],
                    "36": [
                        0,
                        13
                    ],
                    "6": [
                        0,
                        22
                    ],
                    "1": [
                        0,
                        30
                    ],
                    "37": [
                        0,
                        15
                    ],
                    "3": [
                        0,
                        0
                    ],
                    "14": [
                        0,
                        61
                    ],
                    "16": [
                        0,
                        72
                    ],
                    "18": [
                        0,
                        76
                    ],
                    "20": [
                        0,
                        75
                    ],
                    "38": [
                        0,
                        67
                    ],
                    "10": [
                        0,
                        41
                    ],
                    "39": [
                        0,
                        18
                    ],
                    "11": [
                        0,
                        5
                    ],
                    "13": [
                        0,
                        12
                    ],
                    "12": [
                        0,
                        14
                    ],
                    "40": [
                        0,
                        17
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "15": [
                        0,
                        39
                    ],
                    "17": [
                        0,
                        28
                    ],
                    "19": [
                        0,
                        34
                    ]
                },
                "58": {
                    "25": [
                        0,
                        31
                    ]
                },
                "59": {
                    "15": [
                        1,
                        {
                            "@": 53
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 53
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 53
                        }
                    ],
                    "3": [
                        1,
                        {
                            "@": 53
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 53
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 53
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 53
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 53
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 53
                        }
                    ]
                },
                "60": {
                    "21": [
                        1,
                        {
                            "@": 68
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 68
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 68
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 68
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 68
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 68
                        }
                    ]
                },
                "61": {
                    "21": [
                        1,
                        {
                            "@": 40
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 40
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 40
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 40
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 40
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 40
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 40
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 40
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 40
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 40
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 40
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 40
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 40
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 40
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 40
                        }
                    ]
                },
                "62": {
                    "25": [
                        0,
                        7
                    ]
                },
                "63": {
                    "4": [
                        0,
                        48
                    ],
                    "46": [
                        0,
                        69
                    ],
                    "3": [
                        0,
                        59
                    ]
                },
                "64": {
                    "15": [
                        1,
                        {
                            "@": 61
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 61
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 61
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 61
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 61
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 61
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 61
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 61
                        }
                    ]
                },
                "65": {
                    "22": [
                        0,
                        83
                    ],
                    "28": [
                        0,
                        3
                    ],
                    "29": [
                        0,
                        50
                    ],
                    "21": [
                        0,
                        40
                    ]
                },
                "66": {
                    "31": [
                        0,
                        33
                    ],
                    "33": [
                        0,
                        60
                    ],
                    "21": [
                        0,
                        40
                    ],
                    "32": [
                        0,
                        84
                    ],
                    "28": [
                        0,
                        55
                    ]
                },
                "67": {
                    "22": [
                        1,
                        {
                            "@": 25
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 25
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 25
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 25
                        }
                    ]
                },
                "68": {
                    "0": [
                        1,
                        {
                            "@": 76
                        }
                    ],
                    "3": [
                        1,
                        {
                            "@": 76
                        }
                    ]
                },
                "69": {
                    "3": [
                        1,
                        {
                            "@": 75
                        }
                    ],
                    "4": [
                        1,
                        {
                            "@": 75
                        }
                    ]
                },
                "70": {
                    "6": [
                        0,
                        22
                    ],
                    "7": [
                        0,
                        20
                    ],
                    "8": [
                        0,
                        11
                    ],
                    "9": [
                        0,
                        36
                    ],
                    "10": [
                        0,
                        41
                    ],
                    "11": [
                        0,
                        5
                    ],
                    "13": [
                        0,
                        12
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "14": [
                        0,
                        61
                    ],
                    "15": [
                        0,
                        39
                    ],
                    "22": [
                        0,
                        74
                    ],
                    "1": [
                        0,
                        77
                    ],
                    "16": [
                        0,
                        72
                    ],
                    "17": [
                        0,
                        28
                    ],
                    "18": [
                        0,
                        76
                    ],
                    "19": [
                        0,
                        34
                    ],
                    "12": [
                        0,
                        32
                    ],
                    "20": [
                        0,
                        75
                    ]
                },
                "71": {
                    "25": [
                        0,
                        56
                    ]
                },
                "72": {
                    "21": [
                        1,
                        {
                            "@": 42
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 42
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 42
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 42
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 42
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 42
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 42
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 42
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 42
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 42
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 42
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 42
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 42
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 42
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 42
                        }
                    ]
                },
                "73": {
                    "15": [
                        1,
                        {
                            "@": 54
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 54
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 54
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 54
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 54
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 54
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 54
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 54
                        }
                    ]
                },
                "74": {
                    "21": [
                        1,
                        {
                            "@": 47
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 47
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 47
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 47
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 47
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 47
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 47
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 47
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 47
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 47
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 47
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 47
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 47
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 47
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 47
                        }
                    ]
                },
                "75": {
                    "21": [
                        1,
                        {
                            "@": 43
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 43
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 43
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 43
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 43
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 43
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 43
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 43
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 43
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 43
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 43
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 43
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 43
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 43
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 43
                        }
                    ]
                },
                "76": {
                    "21": [
                        1,
                        {
                            "@": 39
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 39
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 39
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 39
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 39
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 39
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 39
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 39
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 39
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 39
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 39
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 39
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 39
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 39
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 39
                        }
                    ]
                },
                "77": {
                    "21": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 46
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 46
                        }
                    ]
                },
                "78": {
                    "24": [
                        0,
                        45
                    ],
                    "5": [
                        1,
                        {
                            "@": 22
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 22
                        }
                    ]
                },
                "79": {
                    "26": [
                        0,
                        21
                    ],
                    "25": [
                        1,
                        {
                            "@": 34
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 34
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 34
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 34
                        }
                    ]
                },
                "80": {
                    "22": [
                        1,
                        {
                            "@": 72
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 72
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 72
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 72
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 72
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 72
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 72
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 72
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 72
                        }
                    ]
                },
                "81": {
                    "7": [
                        0,
                        20
                    ],
                    "8": [
                        0,
                        11
                    ],
                    "9": [
                        0,
                        36
                    ],
                    "35": [
                        0,
                        25
                    ],
                    "36": [
                        0,
                        13
                    ],
                    "6": [
                        0,
                        22
                    ],
                    "1": [
                        0,
                        30
                    ],
                    "37": [
                        0,
                        15
                    ],
                    "14": [
                        0,
                        61
                    ],
                    "16": [
                        0,
                        72
                    ],
                    "42": [
                        0,
                        62
                    ],
                    "18": [
                        0,
                        76
                    ],
                    "20": [
                        0,
                        75
                    ],
                    "38": [
                        0,
                        67
                    ],
                    "10": [
                        0,
                        41
                    ],
                    "39": [
                        0,
                        18
                    ],
                    "11": [
                        0,
                        5
                    ],
                    "13": [
                        0,
                        12
                    ],
                    "12": [
                        0,
                        14
                    ],
                    "40": [
                        0,
                        17
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "15": [
                        0,
                        39
                    ],
                    "17": [
                        0,
                        28
                    ],
                    "19": [
                        0,
                        34
                    ]
                },
                "82": {
                    "21": [
                        1,
                        {
                            "@": 48
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 48
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 48
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 48
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 48
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 48
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 48
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 48
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 48
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 48
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 48
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 48
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 48
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 48
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 48
                        }
                    ]
                },
                "83": {
                    "21": [
                        1,
                        {
                            "@": 55
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 55
                        }
                    ],
                    "0": [
                        1,
                        {
                            "@": 55
                        }
                    ],
                    "23": [
                        1,
                        {
                            "@": 55
                        }
                    ],
                    "15": [
                        1,
                        {
                            "@": 55
                        }
                    ],
                    "11": [
                        1,
                        {
                            "@": 55
                        }
                    ],
                    "7": [
                        1,
                        {
                            "@": 55
                        }
                    ],
                    "6": [
                        1,
                        {
                            "@": 55
                        }
                    ],
                    "8": [
                        1,
                        {
                            "@": 55
                        }
                    ],
                    "9": [
                        1,
                        {
                            "@": 55
                        }
                    ],
                    "13": [
                        1,
                        {
                            "@": 55
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 55
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 55
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 55
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 55
                        }
                    ]
                },
                "84": {
                    "28": [
                        0,
                        55
                    ],
                    "21": [
                        0,
                        40
                    ],
                    "33": [
                        0,
                        49
                    ],
                    "25": [
                        1,
                        {
                            "@": 32
                        }
                    ],
                    "22": [
                        1,
                        {
                            "@": 32
                        }
                    ],
                    "5": [
                        1,
                        {
                            "@": 32
                        }
                    ],
                    "26": [
                        1,
                        {
                            "@": 32
                        }
                    ],
                    "24": [
                        1,
                        {
                            "@": 32
                        }
                    ]
                },
                "85": {
                    "7": [
                        0,
                        20
                    ],
                    "8": [
                        0,
                        11
                    ],
                    "9": [
                        0,
                        36
                    ],
                    "35": [
                        0,
                        25
                    ],
                    "36": [
                        0,
                        13
                    ],
                    "6": [
                        0,
                        22
                    ],
                    "1": [
                        0,
                        30
                    ],
                    "37": [
                        0,
                        15
                    ],
                    "14": [
                        0,
                        61
                    ],
                    "16": [
                        0,
                        72
                    ],
                    "18": [
                        0,
                        76
                    ],
                    "20": [
                        0,
                        75
                    ],
                    "38": [
                        0,
                        67
                    ],
                    "10": [
                        0,
                        41
                    ],
                    "39": [
                        0,
                        18
                    ],
                    "11": [
                        0,
                        5
                    ],
                    "13": [
                        0,
                        12
                    ],
                    "12": [
                        0,
                        14
                    ],
                    "40": [
                        0,
                        17
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "42": [
                        0,
                        29
                    ],
                    "15": [
                        0,
                        39
                    ],
                    "17": [
                        0,
                        28
                    ],
                    "19": [
                        0,
                        34
                    ]
                },
                "86": {
                    "5": [
                        1,
                        {
                            "@": 19
                        }
                    ]
                },
                "87": {
                    "1": [
                        0,
                        52
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "3": [
                        0,
                        73
                    ]
                },
                "88": {
                    "7": [
                        0,
                        20
                    ],
                    "8": [
                        0,
                        11
                    ],
                    "9": [
                        0,
                        36
                    ],
                    "35": [
                        0,
                        25
                    ],
                    "36": [
                        0,
                        13
                    ],
                    "6": [
                        0,
                        22
                    ],
                    "1": [
                        0,
                        30
                    ],
                    "37": [
                        0,
                        15
                    ],
                    "14": [
                        0,
                        61
                    ],
                    "16": [
                        0,
                        72
                    ],
                    "18": [
                        0,
                        76
                    ],
                    "20": [
                        0,
                        75
                    ],
                    "38": [
                        0,
                        67
                    ],
                    "10": [
                        0,
                        41
                    ],
                    "11": [
                        0,
                        5
                    ],
                    "39": [
                        0,
                        19
                    ],
                    "13": [
                        0,
                        12
                    ],
                    "12": [
                        0,
                        14
                    ],
                    "40": [
                        0,
                        17
                    ],
                    "0": [
                        0,
                        24
                    ],
                    "15": [
                        0,
                        39
                    ],
                    "17": [
                        0,
                        28
                    ],
                    "19": [
                        0,
                        34
                    ],
                    "5": [
                        1,
                        {
                            "@": 23
                        }
                    ],
                    "25": [
                        1,
                        {
                            "@": 23
                        }
                    ]
                }
            },
            "start_states": {
                "start": 46
            },
            "end_states": {
                "start": 27
            }
        },
        "options": {
            "debug": false,
            "keep_all_tokens": false,
            "tree_class": null,
            "cache": false,
            "postlex": null,
            "parser": "lalr",
            "lexer": "contextual",
            "transformer": null,
            "start": [
                "start"
            ],
            "priority": "normal",
            "ambiguity": "auto",
            "regex": false,
            "propagate_positions": false,
            "lexer_callbacks": {},
            "maybe_placeholders": false,
            "edit_terminals": null,
            "g_regex_flags": 0,
            "use_bytes": false,
            "import_paths": [],
            "source_path": null
        },
        "__type__": "ParsingFrontend"
    },
    "rules": [
        {
            "@": 18
        },
        {
            "@": 19
        },
        {
            "@": 20
        },
        {
            "@": 21
        },
        {
            "@": 22
        },
        {
            "@": 23
        },
        {
            "@": 24
        },
        {
            "@": 25
        },
        {
            "@": 26
        },
        {
            "@": 27
        },
        {
            "@": 28
        },
        {
            "@": 29
        },
        {
            "@": 30
        },
        {
            "@": 31
        },
        {
            "@": 32
        },
        {
            "@": 33
        },
        {
            "@": 34
        },
        {
            "@": 35
        },
        {
            "@": 36
        },
        {
            "@": 37
        },
        {
            "@": 38
        },
        {
            "@": 39
        },
        {
            "@": 40
        },
        {
            "@": 41
        },
        {
            "@": 42
        },
        {
            "@": 43
        },
        {
            "@": 44
        },
        {
            "@": 45
        },
        {
            "@": 46
        },
        {
            "@": 47
        },
        {
            "@": 48
        },
        {
            "@": 49
        },
        {
            "@": 50
        },
        {
            "@": 51
        },
        {
            "@": 52
        },
        {
            "@": 53
        },
        {
            "@": 54
        },
        {
            "@": 55
        },
        {
            "@": 56
        },
        {
            "@": 57
        },
        {
            "@": 58
        },
        {
            "@": 59
        },
        {
            "@": 60
        },
        {
            "@": 61
        },
        {
            "@": 62
        },
        {
            "@": 63
        },
        {
            "@": 64
        },
        {
            "@": 65
        },
        {
            "@": 66
        },
        {
            "@": 67
        },
        {
            "@": 68
        },
        {
            "@": 69
        },
        {
            "@": 70
        },
        {
            "@": 71
        },
        {
            "@": 72
        },
        {
            "@": 73
        },
        {
            "@": 74
        },
        {
            "@": 75
        },
        {
            "@": 76
        },
        {
            "@": 77
        },
        {
            "@": 78
        },
        {
            "@": 79
        }
    ],
    "options": {
        "debug": false,
        "keep_all_tokens": false,
        "tree_class": null,
        "cache": false,
        "postlex": null,
        "parser": "lalr",
        "lexer": "contextual",
        "transformer": null,
        "start": [
            "start"
        ],
        "priority": "normal",
        "ambiguity": "auto",
        "regex": false,
        "propagate_positions": false,
        "lexer_callbacks": {},
        "maybe_placeholders": false,
        "edit_terminals": null,
        "g_regex_flags": 0,
        "use_bytes": false,
        "import_paths": [],
        "source_path": null
    },
    "__type__": "Lark"
};
var MEMO = {
    "0": {
        "name": "WORD",
        "pattern": {
            "value": "(?:(?:[A-Z]|[a-z]))+",
            "flags": [],
            "_width": [
                1,
                4294967295
            ],
            "__type__": "PatternRE"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "1": {
        "name": "WS",
        "pattern": {
            "value": "(?:[ \t\f\r\n])+",
            "flags": [],
            "_width": [
                1,
                4294967295
            ],
            "__type__": "PatternRE"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "2": {
        "name": "KEYWORD",
        "pattern": {
            "value": "(?:(?:[A-Z]|[a-z]))+:",
            "flags": [],
            "_width": [
                2,
                4294967295
            ],
            "__type__": "PatternRE"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "3": {
        "name": "NUMBER",
        "pattern": {
            "value": "(?:[0-9])+(?:\\.(?:[0-9])+)?",
            "flags": [],
            "_width": [
                1,
                4294967295
            ],
            "__type__": "PatternRE"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "4": {
        "name": "OP",
        "pattern": {
            "value": "(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:\\~|@)|%)|\\&)|\\*)|\\-)|=)|\\+)|\\\\)|\\|)|,)|<)|>)|/)|\\?))+",
            "flags": [],
            "_width": [
                1,
                4294967295
            ],
            "__type__": "PatternRE"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "5": {
        "name": "PARAM",
        "pattern": {
            "value": ":(?:(?:[A-Z]|[a-z]))+",
            "flags": [],
            "_width": [
                2,
                4294967295
            ],
            "__type__": "PatternRE"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "6": {
        "name": "STRING",
        "pattern": {
            "value": "'.*?(?<!\\\\)(\\\\\\\\)*?'",
            "flags": [],
            "_width": [
                2,
                4294967295
            ],
            "__type__": "PatternRE"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "7": {
        "name": "SYMBOL",
        "pattern": {
            "value": "\\#(?:(?:[A-Z]|[a-z]))+",
            "flags": [],
            "_width": [
                2,
                4294967295
            ],
            "__type__": "PatternRE"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "8": {
        "name": "DOT",
        "pattern": {
            "value": ".",
            "flags": [],
            "__type__": "PatternStr"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "9": {
        "name": "__ANON_0",
        "pattern": {
            "value": ":=",
            "flags": [],
            "__type__": "PatternStr"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "10": {
        "name": "SEMICOLON",
        "pattern": {
            "value": ";",
            "flags": [],
            "__type__": "PatternStr"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "11": {
        "name": "__ANON_1",
        "pattern": {
            "value": "@{",
            "flags": [],
            "__type__": "PatternStr"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "12": {
        "name": "RBRACE",
        "pattern": {
            "value": "}",
            "flags": [],
            "__type__": "PatternStr"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "13": {
        "name": "LSQB",
        "pattern": {
            "value": "[",
            "flags": [],
            "__type__": "PatternStr"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "14": {
        "name": "RSQB",
        "pattern": {
            "value": "]",
            "flags": [],
            "__type__": "PatternStr"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "15": {
        "name": "VBAR",
        "pattern": {
            "value": "|",
            "flags": [],
            "__type__": "PatternStr"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "16": {
        "name": "__ANON_2",
        "pattern": {
            "value": "%{",
            "flags": [],
            "__type__": "PatternStr"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "17": {
        "name": "__ANON_3",
        "pattern": {
            "value": "${",
            "flags": [],
            "__type__": "PatternStr"
        },
        "priority": 1,
        "__type__": "TerminalDef"
    },
    "18": {
        "origin": {
            "name": "start",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "repl",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "19": {
        "origin": {
            "name": "repl",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "temps",
                "__type__": "NonTerminal"
            },
            {
                "name": "exprs",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "20": {
        "origin": {
            "name": "repl",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "exprs",
                "__type__": "NonTerminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "21": {
        "origin": {
            "name": "exprs",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "expr",
                "__type__": "NonTerminal"
            },
            {
                "name": "__exprs_star_0",
                "__type__": "NonTerminal"
            },
            {
                "name": "DOT",
                "filter_out": true,
                "__type__": "Terminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "22": {
        "origin": {
            "name": "exprs",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "expr",
                "__type__": "NonTerminal"
            },
            {
                "name": "__exprs_star_0",
                "__type__": "NonTerminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "23": {
        "origin": {
            "name": "exprs",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "expr",
                "__type__": "NonTerminal"
            },
            {
                "name": "DOT",
                "filter_out": true,
                "__type__": "Terminal"
            }
        ],
        "order": 2,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "24": {
        "origin": {
            "name": "exprs",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "expr",
                "__type__": "NonTerminal"
            }
        ],
        "order": 3,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "25": {
        "origin": {
            "name": "expr",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "assign",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "26": {
        "origin": {
            "name": "assign",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "word",
                "__type__": "NonTerminal"
            },
            {
                "name": "__ANON_0",
                "filter_out": true,
                "__type__": "Terminal"
            },
            {
                "name": "assign",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "27": {
        "origin": {
            "name": "assign",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "send",
                "__type__": "NonTerminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "28": {
        "origin": {
            "name": "send",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "keyword",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "29": {
        "origin": {
            "name": "keyword",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "binary",
                "__type__": "NonTerminal"
            },
            {
                "name": "keypairs",
                "__type__": "NonTerminal"
            },
            {
                "name": "cascades",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "30": {
        "origin": {
            "name": "keyword",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "binary",
                "__type__": "NonTerminal"
            },
            {
                "name": "keypairs",
                "__type__": "NonTerminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "31": {
        "origin": {
            "name": "keyword",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "binary",
                "__type__": "NonTerminal"
            }
        ],
        "order": 2,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [
                false,
                true
            ],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "32": {
        "origin": {
            "name": "keypairs",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "__keypairs_plus_1",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "33": {
        "origin": {
            "name": "keypair",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "kw",
                "__type__": "NonTerminal"
            },
            {
                "name": "binary",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "34": {
        "origin": {
            "name": "cascades",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "__cascades_plus_2",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "35": {
        "origin": {
            "name": "binary",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "binary",
                "__type__": "NonTerminal"
            },
            {
                "name": "op",
                "__type__": "NonTerminal"
            },
            {
                "name": "unary",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "36": {
        "origin": {
            "name": "binary",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "unary",
                "__type__": "NonTerminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [
                true,
                false
            ],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "37": {
        "origin": {
            "name": "unary",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "unary",
                "__type__": "NonTerminal"
            },
            {
                "name": "word",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "38": {
        "origin": {
            "name": "unary",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "primary",
                "__type__": "NonTerminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "39": {
        "origin": {
            "name": "primary",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "array",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "40": {
        "origin": {
            "name": "primary",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "block",
                "__type__": "NonTerminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "41": {
        "origin": {
            "name": "primary",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "map",
                "__type__": "NonTerminal"
            }
        ],
        "order": 2,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "42": {
        "origin": {
            "name": "primary",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "scalar",
                "__type__": "NonTerminal"
            }
        ],
        "order": 3,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "43": {
        "origin": {
            "name": "primary",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "number",
                "__type__": "NonTerminal"
            }
        ],
        "order": 4,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "44": {
        "origin": {
            "name": "primary",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "string",
                "__type__": "NonTerminal"
            }
        ],
        "order": 5,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "45": {
        "origin": {
            "name": "primary",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "symbol",
                "__type__": "NonTerminal"
            }
        ],
        "order": 6,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "46": {
        "origin": {
            "name": "primary",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "word",
                "__type__": "NonTerminal"
            }
        ],
        "order": 7,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": true,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "47": {
        "origin": {
            "name": "array",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "__ANON_1",
                "filter_out": true,
                "__type__": "Terminal"
            },
            {
                "name": "__array_star_3",
                "__type__": "NonTerminal"
            },
            {
                "name": "RBRACE",
                "filter_out": true,
                "__type__": "Terminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "48": {
        "origin": {
            "name": "array",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "__ANON_1",
                "filter_out": true,
                "__type__": "Terminal"
            },
            {
                "name": "RBRACE",
                "filter_out": true,
                "__type__": "Terminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "49": {
        "origin": {
            "name": "block",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "LSQB",
                "filter_out": true,
                "__type__": "Terminal"
            },
            {
                "name": "params",
                "__type__": "NonTerminal"
            },
            {
                "name": "temps",
                "__type__": "NonTerminal"
            },
            {
                "name": "exprs",
                "__type__": "NonTerminal"
            },
            {
                "name": "RSQB",
                "filter_out": true,
                "__type__": "Terminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "50": {
        "origin": {
            "name": "block",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "LSQB",
                "filter_out": true,
                "__type__": "Terminal"
            },
            {
                "name": "params",
                "__type__": "NonTerminal"
            },
            {
                "name": "exprs",
                "__type__": "NonTerminal"
            },
            {
                "name": "RSQB",
                "filter_out": true,
                "__type__": "Terminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "51": {
        "origin": {
            "name": "block",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "LSQB",
                "filter_out": true,
                "__type__": "Terminal"
            },
            {
                "name": "temps",
                "__type__": "NonTerminal"
            },
            {
                "name": "exprs",
                "__type__": "NonTerminal"
            },
            {
                "name": "RSQB",
                "filter_out": true,
                "__type__": "Terminal"
            }
        ],
        "order": 2,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "52": {
        "origin": {
            "name": "block",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "LSQB",
                "filter_out": true,
                "__type__": "Terminal"
            },
            {
                "name": "exprs",
                "__type__": "NonTerminal"
            },
            {
                "name": "RSQB",
                "filter_out": true,
                "__type__": "Terminal"
            }
        ],
        "order": 3,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "53": {
        "origin": {
            "name": "params",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "__params_plus_4",
                "__type__": "NonTerminal"
            },
            {
                "name": "VBAR",
                "filter_out": true,
                "__type__": "Terminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "54": {
        "origin": {
            "name": "temps",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "VBAR",
                "filter_out": true,
                "__type__": "Terminal"
            },
            {
                "name": "__temps_plus_5",
                "__type__": "NonTerminal"
            },
            {
                "name": "VBAR",
                "filter_out": true,
                "__type__": "Terminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "55": {
        "origin": {
            "name": "map",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "__ANON_2",
                "filter_out": true,
                "__type__": "Terminal"
            },
            {
                "name": "__map_star_6",
                "__type__": "NonTerminal"
            },
            {
                "name": "RBRACE",
                "filter_out": true,
                "__type__": "Terminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "56": {
        "origin": {
            "name": "map",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "__ANON_2",
                "filter_out": true,
                "__type__": "Terminal"
            },
            {
                "name": "RBRACE",
                "filter_out": true,
                "__type__": "Terminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "57": {
        "origin": {
            "name": "mappair",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "kw",
                "__type__": "NonTerminal"
            },
            {
                "name": "primary",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "58": {
        "origin": {
            "name": "scalar",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "__ANON_3",
                "filter_out": true,
                "__type__": "Terminal"
            },
            {
                "name": "expr",
                "__type__": "NonTerminal"
            },
            {
                "name": "RBRACE",
                "filter_out": true,
                "__type__": "Terminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "59": {
        "origin": {
            "name": "kw",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "KEYWORD",
                "filter_out": false,
                "__type__": "Terminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "60": {
        "origin": {
            "name": "number",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "NUMBER",
                "filter_out": false,
                "__type__": "Terminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "61": {
        "origin": {
            "name": "op",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "OP",
                "filter_out": false,
                "__type__": "Terminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "62": {
        "origin": {
            "name": "param",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "PARAM",
                "filter_out": false,
                "__type__": "Terminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "63": {
        "origin": {
            "name": "string",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "STRING",
                "filter_out": false,
                "__type__": "Terminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "64": {
        "origin": {
            "name": "symbol",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "SYMBOL",
                "filter_out": false,
                "__type__": "Terminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "65": {
        "origin": {
            "name": "word",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "WORD",
                "filter_out": false,
                "__type__": "Terminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "66": {
        "origin": {
            "name": "__exprs_star_0",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "DOT",
                "filter_out": true,
                "__type__": "Terminal"
            },
            {
                "name": "expr",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "67": {
        "origin": {
            "name": "__exprs_star_0",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "__exprs_star_0",
                "__type__": "NonTerminal"
            },
            {
                "name": "DOT",
                "filter_out": true,
                "__type__": "Terminal"
            },
            {
                "name": "expr",
                "__type__": "NonTerminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "68": {
        "origin": {
            "name": "__keypairs_plus_1",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "keypair",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "69": {
        "origin": {
            "name": "__keypairs_plus_1",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "__keypairs_plus_1",
                "__type__": "NonTerminal"
            },
            {
                "name": "keypair",
                "__type__": "NonTerminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "70": {
        "origin": {
            "name": "__cascades_plus_2",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "SEMICOLON",
                "filter_out": true,
                "__type__": "Terminal"
            },
            {
                "name": "keypairs",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "71": {
        "origin": {
            "name": "__cascades_plus_2",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "__cascades_plus_2",
                "__type__": "NonTerminal"
            },
            {
                "name": "SEMICOLON",
                "filter_out": true,
                "__type__": "Terminal"
            },
            {
                "name": "keypairs",
                "__type__": "NonTerminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "72": {
        "origin": {
            "name": "__array_star_3",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "primary",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "73": {
        "origin": {
            "name": "__array_star_3",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "__array_star_3",
                "__type__": "NonTerminal"
            },
            {
                "name": "primary",
                "__type__": "NonTerminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "74": {
        "origin": {
            "name": "__params_plus_4",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "param",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "75": {
        "origin": {
            "name": "__params_plus_4",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "__params_plus_4",
                "__type__": "NonTerminal"
            },
            {
                "name": "param",
                "__type__": "NonTerminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "76": {
        "origin": {
            "name": "__temps_plus_5",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "word",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "77": {
        "origin": {
            "name": "__temps_plus_5",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "__temps_plus_5",
                "__type__": "NonTerminal"
            },
            {
                "name": "word",
                "__type__": "NonTerminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "78": {
        "origin": {
            "name": "__map_star_6",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "mappair",
                "__type__": "NonTerminal"
            }
        ],
        "order": 0,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    },
    "79": {
        "origin": {
            "name": "__map_star_6",
            "__type__": "NonTerminal"
        },
        "expansion": [
            {
                "name": "__map_star_6",
                "__type__": "NonTerminal"
            },
            {
                "name": "mappair",
                "__type__": "NonTerminal"
            }
        ],
        "order": 1,
        "alias": null,
        "options": {
            "keep_all_tokens": false,
            "expand1": false,
            "priority": null,
            "template_source": null,
            "empty_indices": [],
            "__type__": "RuleOptions"
        },
        "__type__": "Rule"
    }
};
