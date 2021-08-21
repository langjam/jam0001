#!/usr/bin/env ruby

# string.undump

class UnFunc
	def initialize(name, checks, func)
		raise "type checks must match func arity" if checks.length != func.arity
		@name = name
		@checks = checks
		@func = func
	end

	def is_callable?(stack)
		return false if stack.length < @func.arity
		stack[-@func.arity, @func.arity].each_with_index do |item, idx|
			check = @checks[idx]
			return false if check == :num and not item.is_a? Integer
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
		val = (@func.call(*uncom.data.pop(@func.arity)))
		uncom.data.push val if val != nil
	end

	def inspect
		@name
	end
end

$uncom_words = {}
[
["uncom-v", [], lambda { "uncom ver 2108.2021" }],
["shrek!", [], lambda { puts "shrek!" ; nil }],

["false", [], lambda { false }],
["true", [], lambda { true }],

["+", [:num, :num], lambda {|a, b| a + b }],
["-", [:num, :num], lambda {|a, b| a - b }],
["*", [:num, :num], lambda {|a, b| a * b }],
["/", [:num, :num], lambda {|a, b| a / b }],
["%", [:num, :num], lambda {|a, b| a % b }],

[">", [:num, :num], lambda {|a, b| a > b }],
["<", [:num, :num], lambda {|a, b| a < b }],

["==", [:any, :any], lambda {|a, b| a == b }],

["and", [:bool, :bool], lambda {|a, b| a and b }],
["or", [:bool, :bool], lambda {|a, b| a or b }],
["not", [:bool], lambda {|a| not a }],

["any?", [:any], lambda {|a| true }],
["num?", [:any], lambda {|a| a.is_a?(Integer) }],
["bool?", [:any], lambda {|a| a.is_a?(FalseClass) or a.is_a?(TrueClass) }],
["str?", [:any], lambda {|a| a.is_a?(String) }],
["quote?", [:any], lambda {|a| a.is_a?(UnQuote) }],
["comment?", [:any], lambda {|a| a.is_a?(UnComment) }],

].each {|word|
	$uncom_words[word[0]] = UnFunc.new(*word)
}

class UnComment
	# NOTE: MAKE SURE THAT THE LENGTH OF THE SOURCE STRING NEVER GETS CHANGED
	def initialize(source, start, len)
		@source = source
		@start = start
		@len = len
		# TODO: save what it looks like at start, so we know what it's variable is called
	end

	def is_commented?
		# TODO: check source to see if we're commnted
	end

	def comment
		# TODO: install comment
	end

	def uncomment
		# TODO: un do the comment
	end

	def inspect
		@source[@start, @len]
	end
end

class UnQuote
	def initialize(source, start, len)
		@source = source
		@start = start
		@len = len
	end

	def string
		@source[@start + 1, @len - 2]
	end

	def inspect
		@source[@start, @len]
	end
end

def do_words(words)
	uncom = Uncom.new
	words.split(" ").each do |word|
		uncom.do_word word
	end
	# puts "-> " + uncom.data.inspect
	uncom
end


# TODO:
# we need a way to set their check functions and arity
# for simlicity let's not have any local functions

# functions are declared with `def`, def takes 2 quotes, {name pred-1 pred-2 ...} { body }

class Uncom
	def initialize(source = "")
		@source = source
		#if file_path != "" then
		#	f = File.new file_path
		#	@source = f.read
		#	f.close
		#end

		@func_stacks = [[]]
		@data_stacks = [[]]
		# first one is global, rest are used for func locals, where last is for current func
		@dict = [$uncom_words.merge({}), {}]
		# storage for locals, mirrors dict, should get pushed / popped when dict does
		@vars = [{}, {}]

		# ip should be place at the start of NEXT word to be executed when step() is called
		@instruction_ptr = 0

		# TODO: call and return functions
		@return_stack = []
	end

	def func
		return @func_stacks.last
	end

	def data
		return @data_stacks.last
	end

	def next_word
		# get next word, starting at instruction_ptr advancing forwards
		# can be a string, number, quote or comment

		# !!!!!!!!!!!!!! YOU WERE HERE !!!!!!!!!!!!!!!!!
		# 1. skip whitespace, make brackets, spaces, curly-braces delimmiters
		# 2. get to the point that we can replace array.split with this function
		# 3. when at end of string return special `end` word that sets end of prog
		# 4. write `step` function that calls this to run the @source program
		#    step should take an arg for how many words you want to run for
		#    step should return both? stacks
		# 5. re-work test function to use step
		# 6. add support for quotes, making sure to keep track of nested quotes
		# 7. add support for comments, decide on when their vars get bound
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
		# special words #

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
		# . -> clear func and data stacks
		elsif word == "." then
			data.pop(data.length)
			func.pop(func.length)
			return false # no funcs called
		end

		# quotation -> push
			# TODO

		# number -> push
		begin
			num = Integer(word)
			data.push num
			return try_apply_stacks()
		rescue ArgumentError
			# not number
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

		# found local -> do word
		if @dict.last.member? word then
			func.push @dict.last[word]
			return try_apply_stacks()
		elsif word.length > 1 and word[-1] == "=" then
		# matches local= -> make local
			@dict.last.merge! gen_variable_funcs(word[0..-2], @vars.last)
			return do_word(word) # word we just made has to get pushed now
		end

		raise "word #{word} not found"
	end

	def gen_variable_funcs(name, vars)
		vars[name] = 0 # TODO: change default value ?
		{name + "=" => UnFunc.new(name + "=", [:any], lambda {|x| vars[name] = x ; nil }),
		name => UnFunc.new(name, [], lambda { vars[name] })}
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
	].each_with_index {|t, idx|
		raise "failed test number #{idx + 1}" if do_words(t[0]).data != t[1]
	}
}).call




































