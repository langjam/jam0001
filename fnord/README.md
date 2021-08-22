comment-record
==============

a silly little language

* building it
* using it
* writing it
* extending it

building it
-----------

cr is built with Rust.  once you have Rust installed, navigate to the
comment-record directory and run:

> cargo build

this puts the executable in the `target/debug` directory.  You can also
just run it directly with:

> cargo run <input file>

using it
--------

write some code in a file, save it as (e.g.) `test.cr` and then run:

> cargo run test.cr

if the last statement of the file is a value, it is printed.

writing it
----------

cr allows two statement types: a type definition and a value definition.
cr has a simple data model, with numbers, text, structs, and comment types.

	# this is a comment

	# this is a number
	number = 42;

	# this is text
	text = "foobar";

	# this is an anonymous struct
	object = {
		name: "My Great Object",
		age: 42,
	};

comments can be attached to struct & field types and instances.  they flow
down from types to instances.

	# struct definition
	struct MyStruct {
		# field definition
		my_field: Number,
	};

	# struct instance
	value = MyStruct {
		# field instance
		my_field: 42,
	};

comments can be incorporated by reference.

	# Here is a lengthy comment describing some important things
	# that we might want to share among several items.
	parent = {};

	#! parent
	child = {};

comments can be inspected at run time, which we use to implement an
idiomatic version of hello world.

	# hello, world!
	x = {};

	result = x!!;

any fields listed in the comments are extracted for use.

	# NAME: Bort
	x = {};

	result = "We need more " + x!NAME + " license plates in the gift shop.";

extending it
------------

if we added procedures, it would open some interesting avenues to explore
around how comments would flow alongside the data they are connected to.
