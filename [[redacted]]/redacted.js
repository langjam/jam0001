function input_stream_for(string, filename) {
	let index = 0
	let line = 1
	let column = 1

	/* TODO: Don't have dumb code here */
	function line_for(index) {
		return string.slice(0, index + 1).split('\n').length
	}
	function column_for(index) {
		return index - string.lastIndexOf('\n', index)
	}

	return {
		get line() { return line },
		get column() { return column },
		get index() { return index },
		get filename() { return filename },

		try_consume(regex_str, name, cb=()=>null, match_predicate=()=>true) {
			const regex = new RegExp(regex_str, 'gm')
			regex.lastIndex = index
			const match = regex.exec(string)
			if (match !== null && match.index === index && match_predicate(match[0])) {
				const [matched, ...groups] = match
				const match_start_index = match.index
				const match_end_index = match_start_index + matched.length
				const match_start_line = line_for(match_start_index)
				const match_end_line = line_for(match_end_index)
				const match_start_column = column_for(match_start_index)
				const match_end_column = column_for(match_end_index)
				index = match_end_index
				line = match_end_line
				column = match_end_column
				const token = {
					kind: name,
					matched,
					data: cb(matched, groups),
					index_start: match_start_index,
					index_end: match_end_index,
					line_start: match_start_line,
					line_end: match_end_line,
					column_start: match_start_column,
					column_end: match_end_column,
					groups 
				}
				return token;
			}
			return undefined
		},
		at_end() { return string.length < index + 1 }
	}
}

function process_comment(matched_text) {
	const text = matched_text.slice(2, -2).trim()
	const comment_regex = /^([^ (]+) *(\([^)]+\))?:/
	const match = comment_regex.exec(text)
	if (match === null) {
		return { kind: 'naked', text }
	} else if (match[2] === undefined) {
		return { kind: 'tagged', tag: match[1], text: text.slice(match[0].length) }
	}
	const parameters = match[2].slice(1, -1).split(',').map(param => param.trim())
	return { kind: 'parametric', tag: match[1], text: text.slice(match[0].length), parameters }
}

function* lex(input) {
	const keywords = new Set(['with', 'do', 'done', 'if', 'then', 'else', 'while', 'holds'])
	while (!input.at_end()) {
		let token = input.try_consume('>>', '>>')
		         || input.try_consume(':=', ':=')
		         || input.try_consume('\\(', '(')
		         || input.try_consume('\\)', ')')
		         || input.try_consume('\\.', '.')
		         || input.try_consume('[0-9]+', 'int', parseInt)
		         || input.try_consume('([^ \t\r\n.>>:,()\\[\\]\']|>[^>]|\\[[^\[]|][^\\]]|:[^=])+', 'ident', s => s, s => !keywords.has(s))
		         || input.try_consume(String.raw`\[\[([^\]]*|\][^\]])*]]`, 'comment', process_comment)
		         || input.try_consume('\'(:?[^\']*|\'\')*\'', 'string', s => s.slice(1, -1).replace(/''/g, '\''))
		         || input.try_consume('with', 'with')
		         || input.try_consume('done', 'done')
		         || input.try_consume('do', 'do')
		         || input.try_consume('if', 'if')
		         || input.try_consume('then', 'then')
		         || input.try_consume('else', 'else')
		         || input.try_consume('while', 'while')
		         || input.try_consume('holds', 'holds')
		         || input.try_consume('[ \t\r\n]+', 'whitespace')
		if (token === undefined)
			throw console.log(input), new Error(`Failed lexing at ${input.filename}:${input.line}:${input.column}`)
		if (token.kind === 'whitespace')
			continue
		yield token
	}
}

function build_parser() {
	const parser = {
		input_filename: null,
		input_stream: null,
		token_stream: null,
		token_stream_index: 0,
		for_input,
		reset,
		parse(rule_name) {
			result = {
				comments: [],
				parsetree: null
			}

			if (rule_name in rules)
				result.parsetree = rules[rule_name]()
			else 
				result.parsetree = try_consume(rule_name)
			return result
		}
	}
	let result = {
		comments: [],
		parsetree: null,
	}
	function for_input(contents, filename) {
		parser.input_filename = filename
		parser.input_stream = input_stream_for(contents, filename)
		parser.token_stream = [...lex(parser.input_stream)]
		reset()
		return parser
	}
	function reset() {
		parser.token_stream_index = 0
		return parser
	}
	function skip_comments() {
		while (parser.token_stream_index < parser.token_stream.length && parser.token_stream[parser.token_stream_index].kind === 'comment')
			result.comments.push(parser.token_stream[parser.token_stream_index++])
	}
	function peek() {
		skip_comments()
		if (parser.token_stream_index < parser.token_stream.length) {
			return parser.token_stream[parser.token_stream_index]
		}
		return { kind: 'eof' }
	}
	function try_consume(kind) {
		const token = peek()
		if (token.kind === kind) {
			parser.token_stream_index++
			return token
		}
	}
	function consume(kind) {
		const token = peek()
		if (token.kind !== kind)
			throw new Error(`Failed parsing: Failed to consume a '${kind}' token at ${parser.input_filename}:${token.line_start}:${token.column_start}`)
		parser.token_stream_index++
		return token
	}
	function fail() {
		const token = peek()
		throw new Error(`Failed parsing at ${parser.input_filename}:${token.line_start}:${token.column_start}`)
	}

	const rules = {
		sequence,
		call,
		assignment,
		field_access,
		literal,
		lambda,
		cond,
		loop,
		parenthesized_expr,
		block
	}

	const obvious = () =>
		literal() || lambda() || cond() || loop() || parenthesized_expr() || block() || try_consume('ident')
	function sequence() {
		const statements = [assignment() || fail()]
		const children = [...statements]
		let current_dot
		while ((current_dot = try_consume('.')) !== undefined) {
			children.push(current_dot)
			const statement = assignment() || obvious()
			if (statement === undefined)
				break
			statements.push(statement)
			children.push(statement)
		}
		return { kind: 'sequence', statements, children }
	}
	function call() {
		const func = field_access() || obvious()
		if (func === undefined) return undefined

		let current_arg = field_access() || obvious()
		if (current_arg === undefined) return func
		const args = [current_arg]	
		while ((current_arg = field_access() || obvious()) !== undefined)
			args.push(current_arg)
		return { kind: 'call', func, args, children: [func, ...args] }
	}
	function assignment() {
		const maybe_call = call()
		if (maybe_call === undefined) return undefined
		if (maybe_call.kind === 'field-access' || maybe_call.kind === 'ident') {
			const last_destination = maybe_call
			// a := b := c>>d := (if cond then thing else another_thing)>>field := value
			let destinations = [last_destination]
			let children = [last_destination]
			let assignment_token
			while ((assignment_token = try_consume(':=')) !== undefined) {
				children.push(assignment_token)
				let destination = call()
				if (destination === undefined) break
				children.push(destination)
				destinations.unshift(destination)
				if (destination.kind !== 'field-access' && destination.kind !== 'ident') break
			}

			let source
			let last_assignment = try_consume(':=')
			if (last_assignment === undefined)
				[source, ...destinations] = destinations
			else {
				source = obvious() || fail()
				children.push(last_assignment, source)
			}
			if (destinations.length === 0) return source
			destinations.reverse()
			return { kind: 'assignment', destinations, source, children }
		}
		return maybe_call
	}
	function field_access() {
		let result = try_consume('ident') || parenthesized_expr()
		if (result === undefined) return undefined
		let access_token
		while ((access_token = try_consume('>>')) !== undefined) {
			const field = try_consume('ident') || consume('int')
			result = { kind: 'field-access', source: result, field, children: [result, access_token, field] }
		}
		return result
		
	}
	function literal() {
		const literal = try_consume('int') || try_consume('string')
		if (literal === undefined) return undefined
		return { kind: 'literal', literal, children: [literal]}
	}
	function lambda() {
		const with_token = try_consume('with')
		if (with_token === undefined) return undefined
		const params = []
		let current_param;
		while ((current_param = try_consume('ident')) !== undefined)
			params.push(current_param)
		const body = block() || fail()
		return { kind: 'lambda', params, body, children: [with_token, ...params, body] }
	}
	function cond() {
		const if_token = try_consume('if')
		if (if_token === undefined) return undefined
		const cond = assignment() || parenthesized_expr() || fail()
		const then_token = consume('then')
		const then_body = assignment() || obvious() || fail()
		const else_token = try_consume('else')
		if (else_token !== undefined) {
			const else_body = assignment() || obvious() || fail()
			return { kind: 'cond', cond, then_body, else_body, children: [if_token, cond, then_token, then_body, else_token, else_body] }
		}
		return { kind: 'cond', cond, then_body, else_body: null, children: [if_token, cond, then_token, then_body] }
	}
	function loop() {
		const while_token = try_consume('while')
		if (while_token === undefined) return undefined
		const cond = assignment() || parenthesized_expr() || fail()
		const holds_token = consume('holds')
		const body = assignment() || obvious() || fail()
		return { kind: 'loop', cond, body, children: [while_token, cond, holds_token, body] }
	}
	function parenthesized_expr() {
		const left_paren = try_consume('(')
		if (left_paren === undefined) return undefined
		const statements = sequence() || fail()
		const right_paren = consume(')')
		return { ...statements, children: [left_paren, ...statements.children, right_paren] }
	}
	function block() {
		const do_token = try_consume('do')
		if (do_token === undefined) return undefined
		const statements = sequence() || fail()
		const done_token = consume('done')
		return { ...statements, children: [do_token, ...statements.children, done_token] }
	}

	return parser
}

function pprint_ast(ast, ident_size=2, ident_char=' ') {
	let result = ''
	const pad_for = level => ''.padStart(level * ident_size, ident_char)
	function pprint_internal(node, level=0, should_pad=false) {
		if (node.kind === 'sequence' && node.statements.length === 1)
			return pprint_internal(node.statements[0], level, should_pad)
		if (node.kind === 'block')
			return pprint_internal(node.sequence, level, should_pad)
		if (should_pad) result += pad_for(level)
		result += node.kind
		if ('data' in node && node.data !== null)
			result += ' ' + node.data + '\n'
		else if ('matched' in node)
			result += ' ' + matched + '\n'
		else {
			result += '\n'
			for (const key in node) {
				if (key === 'kind' || key === 'children') continue
				if (node[key] === undefined || node[key] === null) continue
				result += pad_for(level + 1)
				       + key
				       + ' = '
				if (node[key] instanceof Array) {
					result += '\n'
					for (const child of node[key])
						pprint_internal(child, level + 2, true)
				} else
					pprint_internal(node[key], level + 1, false)
			}
		}
	}
	pprint_internal(ast)
	return result
}

class RedactedObject {
	constructor() {
		this.fields = new Map()
	}
	load_prop(name) {
		const value = this.fields.get(name)
		return value === undefined ? RedactedNone : value
	}
	store_prop(name, value) {
		this.fields.set(name, value)
	}
	call() { throw new Error('Redacted Objects are not callable') }
	to_str() { return `[[REDACTED (${this.fields.size} fields)]]`}
	is_false() { return false }
}

class RedactedFunction extends RedactedObject {
	constructor(debug_info, program, parent_scope) {
		super()
		this.debug_info = debug_info
		this.program = program
		this.parent_scope = parent_scope
	}
	call(...args) {
		if (this.debug_info.arity !== args.length)
			throw new Error(`Arity issue: expected ${this.debug_info.arity} but got ${args.length} parameters`)
		return run(this.program, this.debug_info.start, this, args)
	}
	load_prop(key) {
		if (key === 'call')
			return this.call()
		if (key === 'with_args')
			return redactify(args => {
				if (!(args instanceof RedactedArray))
					throw new Error('Expected an array as a parameter')
				return this.call(...args.elements.slice().reverse())
			})
		return super.load_prop(key)
	}
	store_prop(key, value) { if (key !== 'call' && key !== 'with_args') super.store_prop(key, value) }
	to_str() { return `[[REDACTED (${this.debug_info.end - this.debug_info.start} instructions)]]`}
}

class RedactedArray extends RedactedObject {
	constructor(elements) {
		super()
		this.elements = elements
	}
	load_prop(key) {
		if ((typeof key === 'number' && key === Math.floor(key)) || /^[1-9][0-9]+$/.test(key))
			return this.elements[key]
		else if (key === 'length')
			return redactify(this.elements.length)
		else if (key === 'at')
			return redactify(idx => {
				if (idx.kind === 'jsvalue' && typeof idx.v === 'number')
					return this.elements[idx.v]
				return RedactedNone
			})
		else if (key === 'set')
			return redactify((idx, value) => {
				if (idx.kind === 'jsvalue' && typeof idx.v === 'number')
					return this.elements[idx.v] = value
				return RedactedNone
			})
		else if (key === 'push')
			return redactify(value => redactify(this.elements.push(value)))
		else if (key === 'pop')
			return redactify(() => redactify(this.elements.pop()))
	}
	store_prop(key, value) {
		if ((typeof key === 'number' && key === Math.floor(key)) || /^[1-9][0-9]+$/.test(key))
			return this.elements[key]
		if (key === 'length' || key === 'at' || key === 'set' || key === 'push' || key === 'pop') return
		super.store(key, value)
	}
	to_str() { return `[[REDACTED (${this.elements.length} elements)]]` }
}

const RedactedNone = new (class RedactedNone extends RedactedObject {
	load_prop() { throw new Error('Cannot load property of none') }
	store_prop() { throw new Error('Cannot store property to none') }
	call() { throw new Error('None is not callable') }
	to_str() { return 'none' }
	is_false() { return true }
})


function redactify(v) {
	if (v instanceof RedactedObject) return v
	if (v === undefined || v === null) return RedactedNone
	if (v.kind === 'jsvalue') return v
	const redacted_value = {
		kind: 'jsvalue',
		v,
		load_prop(k) {
			if (v instanceof Function) {
				if (k === 'call')
					return this.call()
				else if (k === 'with_args')
					return redactify(args => {
						if (!(args instanceof RedactedArray))
							throw new Error('Expected an array as a parameter')
						return this.call(...args.elements.slice().reverse())
					})
			}
			return redactify(v[k])
		},
		store_prop() { throw new Error('Primitive values are immutable for now') },
		call(...args) { return redactify(v(...args.reverse())) },
		to_str() { return v.toString() },
		is_false() { return !v }
	}
	return redacted_value
}

const opcodes = []
const op = {}
const operation = {}
function add_op(opcode, ...args) {
	const id = opcodes.length
	const descriptor = [opcode, id, args]
	opcodes.push(descriptor)
	op[opcode] = descriptor
	operation[opcode] = id
}
/*      NAME,          arg0,       arg1 */
add_op('PUSH',        'data_ref')
add_op('DROP')
add_op('LOAD',        'int',      'int')
add_op('STORE',       'int',      'int')
add_op('LOAD_PROP',   'data_ref')
add_op('STORE_PROP',  'data_ref')
add_op('CALL',        'int')
add_op('CREATE_FUNC', 'int')
add_op('JNO',         'label')
add_op('JMP',         'label')
add_op('RET')
add_op('COMMENT',     'comment_ref')

const primitive_ids = new Map()
const primitive_list = []
function add_primitive(name, value) {
	primitive_ids.set(name, primitive_list.length)
	primitive_list.push(redactify(value))
}
add_primitive('none', RedactedNone)
add_primitive('array', (...args) => new RedactedArray(args))
add_primitive('obj', () => new RedactedObject())
add_primitive('print', (...args) => { console.log(...args.map(v => v.to_str())) })
add_primitive('str_concat', (...args) => {
	let result = ''
	for (const arg of args) {
		result += arg.to_str()
	}
	return result
})
add_primitive('comments', new (class RedactedCommentsController extends RedactedObject {
	load_prop(key) {
		if (key === 'category')
			return redactify((category, maybe_handler) => {
				if (maybe_handler === undefined)
					return comment_handlers.get(category.to_str()) || RedactedNone
				else if (maybe_handler === RedactedNone)
					comment_handlers.delete(category.to_str())
				else
					comment_handlers.set(category.to_str(), maybe_handler)
				return RedactedNone
			})
		return RedactedNone
	}
	set_property() { throw new Error('The comments controller is immutable') }
}))
add_primitive('<', (lhs, rhs) => {
	if (lhs.kind === 'jsvalue' && rhs.kind === 'jsvalue')
		return lhs.v < rhs.v
	return RedactedNone
})
add_primitive('=', (lhs, rhs) => {
	if (lhs.kind === 'jsvalue' && rhs.kind === 'jsvalue')
		return lhs.v === rhs.v
	return lhs === rhs
})
add_primitive('+', (lhs, rhs) => {
	if (lhs.kind === 'jsvalue' && rhs.kind === 'jsvalue')
		return lhs.v + rhs.v
	return RedactedNone
})

const comment_handlers = new Map()

function compile(parsetree, comments, filename='') {
	let function_id = 0
	const remaining_functions = [{
		name: '[[main]]',
		func: {
			params: [],
			body: parsetree,
			children: [
				{ kind: 'start', index_start: 0, index_end: 0 },
				parsetree,
				{ kind: 'eof', index_start: Infinity, index_end: Infinity }
			]
		},
		parent: null
	}]
	const bytecode = []
	const rodata = []
	const rodata_offsets = new Map()
	const function_list = []

	function label() {
		let v = null
		let should_call = []
		return {
			referenced(setter) {
				if (v !== null) setter(v)
				else should_call.push(setter)
			},
			resolved(value) {
				v = value
				for (const setter of should_call) setter(v)
				should_call = null
			}
		}
	}

	function resolve(label) {
		label.resolved(bytecode.length - 1)
	}

	function gather_assignments(params, node) {
		const variable_to_offset = new Map()
		const offset_to_variable = []
		function seen(name) {
			if (!variable_to_offset.has(name)) {
				variable_to_offset.set(name, offset_to_variable.length)
				offset_to_variable.push(name)
			}
		}

		function visit(node) {
			if (node.kind === 'lambda') return
			else if (!('children' in node)) return

			if (node.kind === 'assignment')
				for (const destination of node.destinations)
					if (destination.kind === 'ident')
						seen(destination.data)
			for (const child of node.children)
				visit(child)
		}

		for (const param of params.slice().reverse()) seen(param.data)
		visit(node)

		return {
			variable_to_offset,
			offset_to_variable,
			offset_for(varname) {
				return variable_to_offset.get(varname)
			},
			varname_for(offset) {
				return offset_to_variable[offset]
			}
		}
	}

	function compile_function(name, func, parent) {
		const debug_info = {
			name,
			start: bytecode.length,
			end: bytecode.length,
			arity: func.params.length,
			scope: gather_assignments(func.params, func.body),
			parent
		}

		function variable_lookup(varname) {
			let depth = 0
			let fn = debug_info
			while (fn !== null) {
				const varname_offset = fn.scope.offset_for(varname)
				if (varname_offset !== undefined) {
					return [depth, varname_offset]
				}
				fn = fn.parent
				depth++
			}
			const primitive_offset = primitive_ids.get(varname)
			if (primitive_offset !== undefined) return [-1, primitive_offset]
			throw new Error(`Variable '${varname}' was not found on the current scope`)
		}

		function visit(node) {
			switch (node.kind) {
				case 'literal':  return visit_literal(node)
				case 'ident':  return visit_ident(node)
				case 'sequence': return visit_sequence(node)
				case 'assignment': return visit_assignment(node)
				case 'field-access': return visit_field_access(node)
				case 'call': return visit_call(node)
				case 'lambda': return visit_lambda(node)
				case 'cond': return visit_cond(node)
				case 'loop': return visit_loop(node)
				default: throw new Error(`visit_${node.kind} is unimplemented`)
			}
		}

		function start_index_for(token_or_rule) {
			if (token_or_rule.children !== undefined)
				return start_index_for(token_or_rule.children[0])
			return token_or_rule.index_start
		}

		function end_index_for(token_or_rule) {
			if (token_or_rule.children !== undefined)
				return end_index_for(token_or_rule.children[token_or_rule.children.length - 1])
			return token_or_rule.index_end
		}

		function visit_comments_between_end_start(the_end_of_a_node, the_start_of_a_node) {
			let from = end_index_for(the_end_of_a_node)
			let to = start_index_for(the_start_of_a_node)
			return visit_comments_in_range(from, to)
		}

		function visit_comments_between_start_start(the_start_of_a_node, the_start_of_another_node) {
			let from = start_index_for(the_start_of_a_node)
			let to = start_index_for(the_start_of_another_node)
			return visit_comments_in_range(from, to)
		}

		function visit_comments_between_end_end(the_end_of_a_node, the_end_of_another_node) {
			let from = end_index_for(the_end_of_a_node)
			let to = end_index_for(the_end_of_another_node)
			return visit_comments_in_range(from, to)
		}

		function visit_comments_in_range(from, to) {
			// FIXME: We can do binary search here
			for (let i = 0; i < comments.length; i++) {
				const comment = comments[i]
				if (from <= comment.index_start && comment.index_start < to)
					add_op(op.COMMENT, i)
			}
		}

		function visit_literal(node) {
			const data_offset = reserve_data(node.literal.data)
			add_op(op.PUSH, data_offset)
		}

		function visit_sequence(node) {
			visit_comments_between_start_start(node, node.statements[0])
			visit(node.statements[0])
			let previous = node.statements[0]
			for (let i = 1; i < node.statements.length; i++) {
				const statement = node.statements[i]
				visit_comments_between_end_start(previous, statement)
				previous = statement
				add_op(op.DROP)
				visit(statement)
			}
			visit_comments_between_end_end(previous, node)
		}

		function visit_assignment(node) {
			visit_comments_between_start_start(node, node.destinations[0])
			let previous = node.destinations[0]
			if (node.destinations[0].kind === 'field-access')
				visit(node.destinations[0].source)
			for (let i = 1; i < node.destinations.length; i++) {
				const destination = node.destinations[i]
				visit_comments_between_end_start(previous, destination)
				previous = destination
				if (destination.kind === 'field-access')
					visit(destination.source)
			}
			visit_comments_between_end_start(previous, node.source)
			visit(node.source)
			const reversed_destinations = node.destinations.slice().reverse()
			for (const destination of reversed_destinations)
				if (destination.kind === 'field-access') {
					const data_offset = reserve_data(destination.field.data)
					add_op(op.STORE_PROP, data_offset)
				} else {
					const [depth, offset] = variable_lookup(destination.data)
					add_op(op.STORE, depth, offset)
				}
			visit_comments_between_end_end(node.source, node)
		}

		function visit_field_access(node) {
			visit_comments_between_start_start(node, node.source)
			visit(node.source)
			visit_comments_between_end_start(node.source, node.field)
			const data_offset = reserve_data(node.field.data)
			add_op(op.LOAD_PROP, data_offset)
			visit_comments_between_end_end(node.field, node)
		}

		function visit_ident(node) {
			const [depth, offset] = variable_lookup(node.data)
			add_op(op.LOAD, depth, offset)
		}

		function visit_call(node) {
			visit_comments_between_start_start(node, node.func)
			visit(node.func)
			let previous = node.func
			for (const arg of node.args) {
				visit_comments_between_end_start(previous, arg)
				previous = arg
				visit(arg)
			}
			add_op(op.CALL, node.args.length)
			visit_comments_between_end_end(previous, node)
		}

		function visit_lambda(node) {
			remaining_functions.push({
				name: '[[anonymous]]',
				func: node,
				parent: debug_info,
			})
			add_op(op.CREATE_FUNC, ++function_id)
		}

		function visit_cond(node) {
			// FIXME: This is a buggy
			//        if <cond> [[LOG:thing]] then 1 else 2.
			//        doesn't do unconditional logging.
			const after_then_label = label()
			visit_comments_between_end_start(node, node.cond)
			visit(node.cond)
			add_op(op.JNO, after_then_label)
			visit_comments_between_end_start(node.cond, node.then_body)
			visit(node.then_body)
			if (node.else_body !== null) {
				const after_else_label = label()
				add_op(op.JMP, after_else_label)
				resolve(after_then_label)
				visit_comments_between_end_start(node.then_body, node.else_body)
				visit(node.else_body)
				resolve(after_else_label)
				visit_comments_between_end_end(node.else_body, node)
			} else {
				resolve(after_then_label)
				visit_comments_between_end_end(node.then_body, node)
			}
		}

		function visit_loop(node) {
			const condition_label = label()
			const after_loop_label = label()
			resolve(condition_label)
			visit_comments_between_start_start(node, node.cond)
			visit(node.cond)
			visit_comments_between_end_start(node.cond, node.body)
			add_op(op.JNO, after_loop_label)
			visit(node.body)
			add_op(op.JMP, condition_label)
			resolve(after_loop_label)
			visit_comments_between_end_end(node.body, node)
		}


		let previous
		if (func.params.length !== 0) {
			visit_comments_between_start_start(func, func.params[0])
			previous = func.params[0]
			for (let i = func.params.length - 1; i >= 0; i--) {
				add_op(op.STORE, 0, i)
				add_op(op.DROP)
				const param = func.params[i]
				visit_comments_between_end_start(previous, param)
				previous = param
			}
			visit_comments_between_end_start(previous, func.body)
		} else
			visit_comments_between_start_start(func, func.body)
		visit(func.body)
		visit_comments_between_end_end(func.body, func)
		add_op(op.RET)
		debug_info.end = bytecode.length
		function_list.push(debug_info)
	}

	function add_op(opcode, ...args) {
		if (opcode === undefined)
			throw new Error('ICE, got `undefined` instead of an opcode descriptor')
		if (opcode[2].length !== args.length)
			throw new Error(`ICE, opcode arity mismatch for ${opcode[0]}, expected ${opcode[2].length} got ${args.length}`)

		bytecode.push(opcode[1])
		for (let i = 0; i < opcode[2].length; i++) {
			const arg_type = opcode[2][i]
			if (arg_type === 'label') {
				const offset = bytecode.length
				bytecode.push(0x1ABE1)
				args[i].referenced(v => bytecode[offset] = v)
			} else
				bytecode.push(args[i])
		}
	}

	function reserve_data(data) {
		let offset = rodata_offsets.get(data)
		if (offset !== undefined) return offset
		offset = rodata.length 
		rodata.push(data)
		rodata_offsets.set(data, offset)
		return offset
	}

	while (remaining_functions.length !== 0) {
		const fn = remaining_functions.shift()
		compile_function(fn.name, fn.func, fn.parent)
	}
	return { bytecode, rodata, comments: comments, functions: function_list }
}

function disassemble(compilation_result) {
	let result = ''
	const formatter = {
		data_ref(v) { return JSON.stringify(compilation_result.rodata[v]) },
		int(v) { return v.toString() },
		label(v) { return '0x'+(v+1).toString(16).padStart(4, '0') },
		comment_ref(v) { return compilation_result.comments[v].matched }
	}
	for (let i = 0; i < compilation_result.bytecode.length; i++) {
		const op_offset = i
		const opcode = compilation_result.bytecode[i]
		const opcode_description = opcodes[opcode]
		const opcode_arity = opcode_description[2].length
		if (opcode_arity !== 0) {
			const arguments = compilation_result.bytecode
				.slice(i + 1, i + 1 + opcode_arity)
				.map((argument, i) => formatter[opcode_description[2][i]](argument));
			i += opcode_arity
			result += `0x${op_offset.toString(16).padStart(4, '0')} | ${opcode_description[0]}(${arguments.join(', ')})\n`
		} else {
			result += `0x${op_offset.toString(16).padStart(4, '0')} | ${opcode_description[0]}\n`
		}
	}
	return result
}

function run(compilation_result, pc=0, func, args) {
	const stack = [...args]
	function arg() {
		return compilation_result.bytecode[++pc]
	}
	function rodata_arg() {
		return compilation_result.rodata[arg()]
	}
	const active_scope = {
		parent: func.parent_scope,
		locals: Array(func.debug_info.scope.offset_to_variable.length).fill(RedactedNone)
	}
	while (pc <= compilation_result.bytecode.length) {
		let opcode = compilation_result.bytecode[pc]
		switch(opcode) {
			case operation.PUSH: {
				stack.push(redactify(rodata_arg()))
				break
			}
			case operation.DROP: {
				stack.pop()
				break
			}
			case operation.LOAD: {
				let depth = arg()
				const offset = arg()
				if (depth === -1) {
					stack.push(primitive_list[offset])
				} else {
					let scope = active_scope
					while (depth > 0) {
						scope = scope.parent
						depth--
					}
					stack.push(scope.locals[offset])
				}
				break
			}
			case operation.STORE: {
				let depth = arg()
				const offset = arg()
				let scope = active_scope
				while (depth > 0) {
					scope = scope.parent
					depth--
				}
				scope.locals[offset] = stack[stack.length-1]
				break
			}
			case operation.LOAD_PROP: {
				stack.push(stack.pop().load_prop(rodata_arg()))
				break
			}
			case operation.STORE_PROP: {
				const value = stack.pop()
				const obj = stack.pop()
				obj.store_prop(rodata_arg(), value)
				stack.push(value)
				break
			}
			case operation.CALL: {
				let argcount = arg()
				const args = []
				for (let i = 0; i < argcount; i++)
					args.push(stack.pop())
				const fn = stack.pop()
				stack.push(fn.call(...args))
				break
			}
			case operation.CREATE_FUNC: {
				const func_idx = arg()
				const debug_info = compilation_result.functions[func_idx]
				const redacted_func = new RedactedFunction(debug_info, compilation_result, active_scope)
				stack.push(redacted_func)
				break
			}
			case operation.JNO: {
				const dst = arg()
				if (stack.pop().is_false()) pc = dst
				break
			}
			case operation.JMP: {
				pc = arg()
				break
			}
			case operation.RET: {
				return stack.pop() || RedactedNone
			}
			case operation.COMMENT: {
				const comment = compilation_result.comments[arg()].data
				if (comment.kind === 'naked') {
					const handler = comment_handlers.get('naked')
					if (handler !== undefined) handler.call(redactify(comment.text))
				} else if (comment.kind === 'tagged') {
					const handler = comment_handlers.get(comment.tag)
					if (handler !== undefined) handler.call(redactify(comment.text))
				} else {
					const handler = comment_handlers.get(comment.tag)
					if (handler !== undefined) handler.call(redactify(comment.text), ...comment.parameters.map(redactify).slice().reverse())
				}
			}
		}
		++pc
	}
	throw new Error('Got to the end of the program without a RET')
}

function main(args) {
    const source = require('fs').readFileSync(args.filename, 'utf8')
	const parser = build_parser().for_input(source, args.filename)
	const parser_results = parser.parse('sequence')
	if (parser_results.parsetree === null || parser_results.parsetree === undefined)
		throw new Error('Parsing failed')

    if (args.dump_ast) console.log(pprint_ast(parser_results.parsetree))
    const compiler_results = compile(parser_results.parsetree, parser_results.comments, args.filename)
    
    if (args.dump_bytecode) console.log(disassemble(compiler_results))

    if (args.execute) {
	    const main = new RedactedFunction(compiler_results.functions[0], compiler_results, null)
        return main.call()
    }
    return RedactedNone
}

function parse_args() {
    const args = {
        filename: null,
        dump_ast: false,
        dump_bytecode: false,
        execute: true
    }
    for (const arg of process.argv.slice(2)) {
        if (arg === '--no-execute') args.execute = false
        else if (arg === '--dump-ast') args.dump_ast = true
        else if (arg === '--dump-bytecode') args.dump_bytecode = true
        else args.filename = arg
    }
    if (args.filename === null) {
        console.log(
`Usage: ${process.argv[0]} ${process.argv[1]} [options] <filename>
    --no-execute:    Perform all the neccesary tasks but don't execute the program
    --dump-ast:      Dump the AST after parsing
    --dump-bytecode: Dump the generated bytecode after compilation`)
        process.exit(1)
    }
    return args
}

main(parse_args())
