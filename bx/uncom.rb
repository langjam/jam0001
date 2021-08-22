#!/usr/bin/env ruby

# string.undump

class UnFunc
	def initialize(name, checks, func, arity: false)
		@name = name
		@checks = checks
		@func = func
		@arity = func.arity
		@arity = arity if arity.is_a? Integer
		raise "type checks must match func arity" if checks.length != @arity
	end

	def is_callable?(stack)
		return false if stack.length < @arity
		stack[-@arity, @arity].each_with_index do |item, idx|
			check = @checks[idx]
			return false if check == :num and not item.is_a? Float
			return false if check == :bool and not (item.is_a?(FalseClass) or item.is_a?(TrueClass))
			return false if check == :str and not item.is_a? String
			return false if check == :quote and not item.is_a? UnQuote
			return false if check == :comment and not item.is_a? UnComment
			# raise "i don't kow what type #{check} is" if check != :any
		end
		return true
	end

	def call(uncom)
		# uncom.data.push(@func.call(*uncom.data.pop(@func.arity)))
		val = (@func.call(*uncom.data.pop(@arity)))
		uncom.data.push val if val != nil
	end

	def inspect
		@name
	end
end

$uncom_words = {}
[
["uncom-v", [], lambda { "uncom ver 2208.2021" }],

["false", [], lambda { false }],
["true", [], lambda { true }],

["+", [:num, :num], lambda {|a, b| a + b }],
["-", [:num, :num], lambda {|a, b| a - b }],
["*", [:num, :num], lambda {|a, b| a * b }],
["/", [:num, :num], lambda {|a, b| a / b }],
["%", [:num, :num], lambda {|a, b| a % b }],
["inv", [:num], lambda {|a| -a }],
["neg", [:num], lambda {|a| -a }],

[">", [:num, :num], lambda {|a, b| a > b }],
["<", [:num, :num], lambda {|a, b| a < b }],
["==", [:any, :any], lambda {|a, b| a == b }],

["and", [:bool, :bool], lambda {|a, b| a and b }],
["or", [:bool, :bool], lambda {|a, b| a or b }],
["not", [:bool], lambda {|a| not a }],

["any?", [:any], lambda {|a| true }],
["num?", [:any], lambda {|a| a.is_a?(Float) }],
["bool?", [:any], lambda {|a| a.is_a?(FalseClass) or a.is_a?(TrueClass) }],
["str?", [:any], lambda {|a| a.is_a?(String) }],
["quote?", [:any], lambda {|a| a.is_a?(UnQuote) }],
["comment?", [:any], lambda {|a| a.is_a?(UnComment) }],

["puts", [:any], lambda {|a| puts a }],

].each {|word|
	$uncom_words[word[0]] = UnFunc.new(*word)
}

$uncom_words["def"] = Class.new(UnFunc) do
	def call(uncom)
		body = uncom.data.pop
		checks = uncom.data.pop.to_s.split(" ").map {|x| x.to_sym }
		name = uncom.data.pop.to_s.split(" ")[0]

		uncom.dict_set(name, Class.new(UnFunc) do
			def initialize(name, checks, body)
				@name = name
				@body = body
				@checks = checks
				@arity = checks.length
			end
			def call(uncom)
				uncom.call_quote(@body)
			end
		end.new(name, checks, body))
	end
end.new("def", [:quote, :quote, :quote], lambda {|a, b, c|})

$uncom_words["if"] = Class.new(UnFunc) do
	def call(uncom)
		
	end
end.new("def", [:bool, :quote], lambda {|a, b|})

class UnComment
	# NOTE: MAKE SURE THAT THE LENGTH OF THE SOURCE STRING NEVER GETS CHANGED

	def initialize(source, start, len)
		@source = source
		@start = start
		@len = len

		# TODO: save what it looks like at start, so we know what it's variable is called

		name = ""
		@name = source[start + 1, len - 2].split("").each_with_index do |c, idx|
			name += c
			break "" if " \n\t".include? c
			break name[..-2] if c == ":"
			break "" if idx == len - 3
		end
	end

	def name
		@name
	end

	def is_commented?
		return @source[@start] ==  "#"
	end

	def comment!
		# TODO: install comment
	end

	def uncomment!
		# TODO: un do the comment
	end

	def inspect
		@source[@start, @len].inspect
	end

	def to_s
		@source[@start, @len].inspect
	end
end

class UnQuote
	def initialize(source, start, len)
		@source = source
		@start = start
		@len = len
	end
	def start() @start end

	def to_s
		@source[@start + 1, @len - 2]
	end

	def inspect
		@source[@start, @len]
	end
end

class Uncom
	def initialize(source = "")
		@source = source
		@func_stacks = [[]]
		@data_stacks = [[]]
		# first one is global, rest are used for func locals, where last is for current func
		@dict = [$uncom_words.merge({}), {}]
		# storage for locals, mirrors dict, should get pushed / popped when dict does
		@vars = [{}, {}]
		# ip is start of word to be executed when run() is called
		@instruction_ptr = 0
		@return_stack = []
	end

	def func
		return @func_stacks.last
	end

	def data
		return @data_stacks.last
	end

	def dict_set(k, v)
		@dict.first[k] = v
	end

	def run(steps: true, print_words: false)
		while (steps.is_a? TrueClass or steps > 0) do
			steps -= 1 if steps.is_a? Integer
			w = next_word
			puts w.inspect if print_words
			if w == "halt" then
				break
			end
			do_word w
		end

		self
	end

	# NOTE: WHEN WE DO A CALL, CHECK IF WE JUMPED INTO A COMMENT, IF SO COMEBACK FROM CALL

	def call_quote(q)
		@return_stack.push(@instruction_ptr)
		@instruction_ptr = q.start + 1
	end

	def next_word
		def cur_char() @source[@instruction_ptr] end
		def eof?() @instruction_ptr >= @source.length end

		whitespace = [" ", "\t", "\n"]
		one_chars = ["(", ")", "{", "}", "[", "]", "#"]

		while (whitespace.include? cur_char) do
			@instruction_ptr += 1
		end
		return "halt" if eof?

		if cur_char == "#" then
			start = @instruction_ptr
			len = 0
			while cur_char != "\n" and not eof? do
				len += 1 # comment += cur_char
				@instruction_ptr += 1
			end
			if cur_char == "\n" then
				len += 1 # comment += cur_char
				@instruction_ptr += 1
			end
			return UnComment.new @source, start, len
		end

		if cur_char == "{" then
			start = @instruction_ptr
			len = 0
			nest = 0
			loop do
				raise "unclosed { cannot parse" if eof?
				nest += 1 if cur_char == "{"
				nest -= 1 if cur_char == "}"
				len += 1
				@instruction_ptr += 1
				break if nest == 0
			end
			return UnQuote.new @source, start, len
		end

		if one_chars.include? cur_char then
			word = cur_char
			@instruction_ptr += 1
			return word
		end

		word = ""
		while (not (whitespace + one_chars + [nil]).include? cur_char) do
			word += cur_char
			@instruction_ptr += 1
		end

		begin
			num = Float(word)
			word = num
		rescue ArgumentError
			# not number
		end

		return word
	end

	# retuns false when nothing was done, true when func(s) were called
	def try_apply_stacks() # -> bool
		return false if func.length < 1
		if func.last.is_callable? data then
			func.pop.call(self)
			try_apply_stacks()
			return true
		end
		return false
	end

	def do_word(word)
		# number -> push
		# quote-> push
		if word.is_a?(Float) or word.is_a?(UnQuote) then
			data.push word
			return try_apply_stacks()
		end

		# comment -> check name and set variable
		if word.is_a?(UnComment) then
			return false if word.name == ""
			data.push(word)
			return do_word(word.name + "=")
		end

		# found local -> do word
		if @dict.last.member? word then
			func.push @dict.last[word]
			return try_apply_stacks()
		elsif word[0] != "$" and word.length > 1 and word[-1] == "=" then
		# matches local= -> make local
			@dict.last.merge! gen_variable_funcs(word[0..-2], @vars.last)
			return do_word(word) # word we just made has to get pushed now
		end

		# found global -> do word
		if @dict.first.member? word then
			func.push @dict.first[word]
			return try_apply_stacks()
		# matches $global= -> make global
		elsif word[0] == "$" and word[-1] == "=" then
			@dict.first.merge! gen_variable_funcs(word[0..-2], @vars.first)
			return do_word(word) # word we just made has to get pushed now
		end

		return do_word_special word
	end

	def do_word_special(word)
		# ( -> push data + func stacks
		if word == "(" then
			@data_stacks.push([])
			@func_stacks.push([])
			return false # no funcs called
		# ) -> pop data + func stacks, save last from data, put ontop of prev data
		elsif word == ")" then
			if @data_stacks.length < 2 then
				raise "UNBALENCED ) THIS WOULDVE TRIGGERED A STACK UNDERFLOW"
				return false # no funcs called
			end
			s = @data_stacks.pop()
			data.push(s.last) if s.length > 0
			@func_stacks.pop()
			return try_apply_stacks()

		# [ -> push dict + vars stacks
		elsif word == "["
			@dict.push({})
			@vars.push({})
			return false # no funcs called
		# ] -> pop dict + vars stacks
		elsif word == "]"
			if @dict.length < 3 then
				raise "UNBALENCED ]"
				return false # no funcs called
			end
			@dict.pop
			@vars.pop
			return false # no funcs called

		# } -> pop return stack and return from quote
		elsif word == "}" then
			if @return_stack.length < 1 then
				raise "UNBALENCED }"
				return false
			end
			@instruction_ptr = @return_stack.pop()
			return false # no funcs called

		# . -> clear func and data stacks
		elsif word == "." then
			data.pop(data.length)
			func.pop(func.length)
			return false # no funcs called
		end

		raise "word #{word} not found"
	end

	def gen_variable_funcs(name, vars)
		vars[name] = 0 # TODO: change default value ?
		{name + "=" => UnFunc.new(name + "=", [:any], lambda {|x| vars[name] = x ; nil }),
		name => UnFunc.new(name, [], lambda { vars[name] })}
	end

	def read_words() # good for degging reader
		words = []
		loop do
			w = next_word
			words.push(w)
			break if w == "halt"
		end
		return words
	end
end

def do_source(source)
	#puts
	#puts "DO SOURCE"
	uncom = Uncom.new source
	uncom.run()
	#puts
	uncom

	#if file_path != "" then
	#	f = File.new file_path
	#	@source = f.read
	#	f.close
	#end
end

# t e s t s #
(lambda {
	[
	["+ 1 2 3 +", [6]],
	["+ + 1 2 3", [6]],
	["1 2 3 + +", [6]],
	["1 + 2 + 3", [6]],
	["( 1 + 2 + 3 ) 4", [6, 4]],
	["$x= 5 $x= ( $x + $x + $x ) $x", [15]],
	["5 $x= ( $x + $x + $x ) $x= $x", [15]],
	["x= 5 x= ( x + x + x ) x", [15]],
	["5 x= ( x + x + x ) x= x", [15]],
	["1 2 3 .", []],
	["1 # 2 3 4", [1]],
	["{1 2 3} quote?", [true]],
	["{1 {2} 3} quote?", [true]],
	["{1 {2} 3} quote?", [true]],
	["x= 1 [x= 2 [x= 3 x] x] x", [3, 2, 1]],
	["$x= 0 x= 1 [x= 2 [x= 3 $x x] x] x", [0, 3, 2, 1]],
	['1 2 3 #name: dookies
	name comment?', [1, 2, 3, true]],
	['name= 42 [#name: dookies
	name comment?] name', [true, 42]],
	['#$name: dookies
	[$name comment?]', [true]],
	].each_with_index {|t, idx|
		raise "failed test number #{idx + 1}" if do_source(t[0]).data != t[1]
	}
}).call




































