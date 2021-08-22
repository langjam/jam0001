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
			return false if check == :array and not item.is_a? Array
			# raise "i don't kow what type #{check} is" if check != :any
		end
		return true
	end

	def call(uncom)
		# uncom.data.push(@func.call(*uncom.data.pop(@func.arity)))
		val = (@func.call(*uncom.data.pop(@arity)))
		uncom.data.push val if val != :no
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
["nil", [], lambda { nil }],

["+", [:num, :num], lambda {|a, b| a + b }],
["-", [:num, :num], lambda {|a, b| a - b }],
["*", [:num, :num], lambda {|a, b| a * b }],
["/", [:num, :num], lambda {|a, b| a / b }],
["%", [:num, :num], lambda {|a, b| a % b }],
["inv", [:num], lambda {|a| -a }],
["neg", [:num], lambda {|a| -a }],

[">", [:num, :num], lambda {|a, b| a > b }],
["<", [:num, :num], lambda {|a, b| a < b }],
[">=", [:num, :num], lambda {|a, b| a >= b }],
["<=", [:num, :num], lambda {|a, b| a <= b }],
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
["array?", [:any], lambda {|a| a.is_a?(Array) }],
["nil?", [:any], lambda {|a| a.is_a?(NilClass) }],

["print", [:any], lambda {|a| print a ; STDOUT.flush ; :no }],
["puts", [:any], lambda {|a| puts a ; :no }],
["p", [:any], lambda {|a| p a ; :no }],
["inspect", [:any], lambda {|a| a.inspect }],
["to_s", [:any], lambda {|a| a.to_s }],

["floor", [:num], lambda {|a| a.floor }],

["get", [:array, :num], lambda {|a, i| a[i] }],
["set", [:array, :num, :any], lambda {|a, i, v| a[i] = v ; :no }],
["length", [:array], lambda {|a| a.length }],

["commented?", [:comment], lambda {|a| a.is_commented? }],
["uncomment", [:comment], lambda {|a| a.uncomment! ; :no }],
["comment", [:comment], lambda {|a| a.comment! ; :no }],

["bytes", [:str], lambda {|a| a.bytes}],
["pack", [:array], lambda {|a| a.pack "C*"}],
["readline", [], lambda { readline }],
["read", [:num], lambda {|a| STDIN.read(a) }],

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
		body = uncom.data.pop()
		pred = uncom.data.pop()
		uncom.call_quote(body) if pred
	end
end.new("if", [:bool, :quote], lambda {|a, b|})

$uncom_words["ifelse"] = Class.new(UnFunc) do
	def call(uncom)
		f_body = uncom.data.pop()
		t_body = uncom.data.pop()
		pred = uncom.data.pop()
		uncom.call_quote(t_body) if pred
		uncom.call_quote(f_body) if not pred
	end
end.new("ifelse", [:bool, :quote, :quote], lambda {|a, b, c|})

$uncom_words["do"] = Class.new(UnFunc) do
	def call(uncom)
		uncom.call_quote uncom.data.pop()
	end
end.new("do", [:quote], lambda {|a|})

$uncom_words["array"] = Class.new(UnFunc) do
	def call(uncom)
		uncom.data.push(uncom.data.pop(uncom.data.length))
	end
end.new("array", [], lambda {})

$uncom_words["print-source"] = Class.new(UnFunc) do
	def call(uncom)
		e = uncom.data.pop - 1
		start = uncom.data.pop - 1
		lines = uncom.source.lines
		lines.each_with_index do |l, idx|
			puts "#{(idx + 1).to_s.rjust lines.length.to_s.length} #{l}" if idx >= start
			break if idx >= e
		end
	end
end.new("print-source", [:num, :num], lambda {|a, b|})

$uncom_words["print-stack"] = Class.new(UnFunc) do
	def call(u)
		puts " -> (data #{u.data.inspect} / func #{u.func.inspect})"
	end
end.new("print-stack", [], lambda {})

class UnComment
	def initialize(source, start, len)
		@source = source
		@start = start
		@len = len

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
		return @source[@start] == "#"
	end

	def comment!
		l = @source.length
		@source[@start, @name.length + 2] = "#" + @name + ":" if @name != ""
		raise "errr, source changed length" if @source.length != l
	end

	def uncomment!
		l = @source.length
		@source[@start, @name.length + 2] = " " * (@name.length + 2) if @name != ""
		raise "errr, source changed length" if @source.length != l
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
	def initialize(source = "", file: "")
		@source = source
		if file != "" then
			f = File.new file
			@source = f.read
			f.close
		end
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

	def source() @source end
	def func() @func_stacks.last end
	def data() @data_stacks.last end
	def instruction_ptr=(v) @instruction_ptr = v end

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
		vars[name] = 0 # change default value ?
		{name + "=" => UnFunc.new(name + "=", [:any], lambda {|x| vars[name] = x ; :no }),
		name => UnFunc.new(name, [], lambda { vars[name] })}
	end

	def read_words() # good for debugging reader
		words = []
		loop do
			w = next_word
			words.push(w)
			break if w == "halt"
		end
		return words
	end
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
	["if false {1}",[]],
	["if true {1}",[1]],
	["ifelse true {1} {2}",[1]],
	["ifelse false {1} {2}",[2]],
	["do { 1 2 }",[1, 2]],
	["(1 2 3 4 array) length",[4]],
	["(1 2 3 9 array) get 3",[9]],
	["x= (1 2 3 9 array) set x 2 47 x",[[1, 2, 47, 9]]],
	['def {five} {} {
		#push5: 5
		#push6: (1 + 5)
	}
	five
	uncomment push5
	five
	comment push5 uncomment push6
	five', [5, 6]],
	['#com: ~something cool~ :D
	commented? com
	uncomment com commented? com', [true, false]],
	['#$lol: its pasta time
	[commented? $lol]', [true]]
	].each_with_index {|t, idx|
		u = Uncom.new(t[0]).run
		raise "failed test number #{idx + 1} data_stack #{u.data}" if u.data != t[1]
	}
}).call

def do_source(source) # nice for debuggin in pry
	uncom = Uncom.new source
	uncom.run().data
end

# TODO

begin
	require "sdl2"

	$has_sdl2 = true

	[ # sdl2 words
	["has-sdl2?", [], lambda { true }],
	["make-window", [], lambda {
		SDL2.init(SDL2::INIT_EVERYTHING)
		$window = SDL2::Window.create("Uncom",
			SDL2::Window::POS_UNDEFINED,
			SDL2::Window::POS_UNDEFINED,
			480, 480, SDL2::Window::Flags::RESIZABLE)
		$renderer = $window.create_renderer(-1, SDL2::Renderer::Flags::TARGETTEXTURE)
		$framebuffer = $renderer.create_texture(
			SDL2::PixelFormat::RGBA8888, SDL2::Texture::ACCESS_TARGET, 480, 480)

		# TODO: event loop go here
		:no
	}]

	].each {|word|
		$uncom_words[word[0]] = UnFunc.new(*word)
	}
rescue LoadError
	$uncom_words["has-sdl2"] = UnFunc.new("has-sdl2", [], lambda { false })
end

# run file or use repl #
if ARGV[0] then
	u = Uncom.new(file: ARGV[0])
	u.run()
	print "[done.] "
	readline
else
	source = ""
	u = Uncom.new(source)
	puts 'Welcome to Uncom\'s REPL https://github.com/bxwtf/jam0001/tree/main/bx'
	loop do
		puts " -> (data #{u.data.inspect} / func #{u.func.inspect})"
		line = ""
		while (line == "") do
			begin
				print "  > "
				line = readline
			rescue EOFError
				#
			end
		end

		if line.strip() == "help" then
			puts ' type quit to leave repl'
		elsif line.strip() == "quit" then
			break
		else
			u.instruction_ptr = source.length
			source[source.length, 0] = line
		end

		begin
			u.run()
		rescue RuntimeError => e
			puts "err, #{e.message}"
		end
	end
end































