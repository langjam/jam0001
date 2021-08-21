
=begin

def print-hello
	#$com-a puts "hello world" # anonymouse comment
end

uncomment $com-a

print-hello # prints hello

comment $com-a

print-hello # does nothing

=end

# TODO:
# read in file as string
# place "instruction pointer" at the start of the string
# decide on how functions are declared
# declare some variables

# DONE decide on how variables are declared

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

class UnFunc
	def initialize
	end
	#def is_callable? (data_stack) # -> bool
	#	return true
	#end
	# def call (uncom)
end

$uncom_words = {}
$uncom_words["+"] = Class.new(UnFunc) do
	def is_callable?(s)
		return false if s.length < 2
		return false if not s[-1].is_a? Integer
		return false if not s[-2].is_a? Integer
		return true
	end

	def call(uncom)
		uncom.data.push(uncom.data.pop + uncom.data.pop)
	end

	def inspect() "+" end
end.new

$uncom_words["-"] = Class.new(UnFunc) do
	def is_callable?(s)
		return false if s.length < 2
		return false if not s[-1].is_a? Integer
		return false if not s[-2].is_a? Integer
		return true
	end

	def call(uncom)
		uncom.data.push(-(uncom.data.pop) + uncom.data.pop)
	end

	def inspect() "-" end
end.new

$uncom_words[">"] = Class.new(UnFunc) do
	def is_callable?(s)
		return false if s.length < 2
		return false if not s[-1].is_a? Integer
		return false if not s[-2].is_a? Integer
		return true
	end

	def call(uncom)
		uncom.data.push(uncom.data.pop <= uncom.data.pop)
	end

	def inspect() ">" end
end.new

def do_words(words)
	uncom = Uncom.new
	words.split(" ").each do |word|
		uncom.do_word word
	end
	puts "-> " + uncom.data.inspect
	uncom
end

class Uncom
	def initialize()
		@func_stacks = [[]]
		@data_stacks = [[]]
		# first one is global, rest are used for func locals, where last is for current func
		@dict = [$uncom_words.merge({}), {}]
		# TODO:
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
				# TODO create setter / getter pairs
				puts "That'd have made a global"
			end
			# found local -> do word
				# TODO
			# matches local= -> make local
				# TODO
		puts "did nothing"
	end
end







































