#!/usr/bin/env ruby

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

].each {|word|
	$uncom_words[word[0]] = UnFunc.new(*word)
}

def do_words(words)
	uncom = Uncom.new
	words.split(" ").each do |word|
		uncom.do_word word
	end
	puts "-> " + uncom.data.inspect
	uncom
end

class Uncom
	def initialize(file_path = "")
		@source = ""
		if file_path != "" then
			f = File.new file_path
			@source = f.read
			f.close
		end

		@func_stacks = [[]]
		@data_stacks = [[]]
		# first one is global, rest are used for func locals, where last is for current func
		@dict = [$uncom_words.merge({}), {}]
		# storage for locals, mirrors dict, should get pushed / popped when dict does
		@vars = [{}, {}]

		# TODO:
		# place "instruction pointer" at the start of the string
		# instruction pointer

		# decide on how functions are declared

		# return stack, saves locations in source to come back to
	end

	def func
		return @func_stacks.last
	end

	def data
		return @data_stacks.last
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

		# number -> push
		begin
			num = Integer(word)
			data.push num
			return try_apply_stacks()
		rescue ArgumentError
			# not number
		end

		# string -> lookup word

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


# TODO:
# create some kind of test

# expresions
# 1 + 2 + 3 -> 6
# + 1 2 + 3
# [ 1 0 ] vecdot [ 0.5 0.5 ]
# average-of-3 + + + 1 1 2 2 3 3

# reverse rpn + prn
# function stack (), data stack []
# + 1 2 3 + -> 6
# + + 1 2 3 -> 6
# 1 2 3 + + -> 6
# 1 + 2 + 3 -> 6
# 1 + . -> () {} # `.` clears both stacks, acting as a statement separtator
# when either function or data is pushed:
#	check if there are enough things on the top of data stack to match the arity of top function
#	if so pop and call top function with top elements of stack and push the result to stack

# global variables
# begin with $ can be assigned to with $var=
# $var is a function that pushes the current value
# $var= is a function that sets the value, calling $undefined= creates the variable
# $var= and $var are created at read time, as long as nothing else matches




































