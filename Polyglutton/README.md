
# Polyglutton

Polyglutton is for those who believe that it's possible to be maximally inefficient when writing code in a language. How about writing a program that works in at least three? But don't worry, you have a wide choice in which subset of languages to write in!

Programs in Polyglutton must be polyglots (programs that do the same thing in multiple languages) in at least 3 out of the 5 supported languages:

* Python 3
* Haskell
* Javascript (through Node.js)
* C (through tcc)
* Perl

Programs must have the exact same output and exit code (including trailing whitespace!), though any errors or warnings sent to STDERR are ignored. Some example "Hello, World!" programs are included in this directory, but most importantly, the Polyglutton interpreter/compiler **is itself a Polyglutton program**! Yes, that's right, the polyglutton.poly file can be run in any of Javascript, Python 3 and Haskell. Truly sticking to the principle of doing three times the work for one program!

# Setup

To run Polyglutton, you have to install all five of its parent languages (plus Bash if you don't have that on your system). You can do this through the requirements script like so: `sudo ./requirements`. The program will probably complain if a language is not present.

You can run a Polyglutton program with any of

* `nodejs polyglutton.poly hello_world1.poly` (Javascript)
* `python3 polyglutton.poly hello_world1.poly` (Python 3)
* `ghc -x hs polyglutton.poly` and then running the resulting executable with `./polyglutton hello_world1.poly`  (Haskell)

Both input and further arguments will be passed to the program (though input will be taken all at once at the start of the program, so if it appears to hang try sending EOF, usually Ctrl-D). 

# Justification

The theme of the Jam was "First class comments", and I thought to myself, what's more first class then actual code? Thus, this idea was born, where the best strategy for writing code in this language is to hide code within the comments of other language's code.

Why these languages? Well, I started with Python 3, Haskell, and Javascript, since they all had different types of comments. I added in C and Perl after realising that just three languages was kinda boring (even though these language have similar comments). I'm pretty sure any size three combination of these languages should be polyglottable, though some might be difficult.

Why 3 out of 5? The subset needs to be more than half of the total, otherwise which output to chose might be ambiguous. Three seemed like a good place to stop, though I would have like to have more languages to choose from. 

# Todo (but ~~started too late~~ ran out of time):

* Move the haskell compiler script into the program itself (I couldn't find an option for ghc to run the compiled code automatically like tcc does)
* Remove all the OS dependant stuff (i.e. file path separators, bash scripts)
* Find some way to buffer input into all subprocesses at once to avoid having to wait on the user even if the program doesn't require input.
* Add in one of the other languages into the interpreter (probably Perl, I don't think C would be very compatible)
* Maybe change C or Perl to a different language (the comments overlap with Javascript/Python a bit much)
* Write some more hello world scripts for each combination of languages (also, number 2 isn't very generalisable, fix that)
* Add some flags for ignoring languages, ignoring exit codes, etc.