# COLAN-21 User Guide
### Overview
COLAN-21 is an interpreted imperative and structured programming language with first-class comments. The name derives from how older languages were named; COLAN stands for **co**mment **lan**guage.

COLAN-21 was developped by Ishidex (iothesys) and Jacob (jacobsebek) with help of misceanalous nature from Doigt (realdoigt) for the August 2021 LangJam.

COLAN-21 is relatively simple to learn, especially for those who are familiar with C-like languages. It's a bit rough on the edges, but keep in mind that the developers only had 48 hours with little sleep to make it. However you'll find a language that can run simple programs easily and which has a very helpful error handler that can help you spot errors in your code.

### Running Scripts
Once you have the COLAN-21 project files on your system, open the terminal in the *c-chads* folder and type *make all*. This will build the interpreter in the *bin* folder.

To run a COLAN-21 program, you have to open a terminal and type: `path\to\COLAN-21 path\to\program`. COLAN-21 source files have either the *.colan* or *.c21* extension. The former being the dejure extension while the latter is the defacto extension. That's because when Doigt started writing test scripts, he wasn't told what the developers had decided for that matter and everyone just went along with the *.c21* extension he had made up, probably because it's shorter to type. Anyways, whichever you choose to use is up to you.

### Program Structure
The minimal program structure of the language is very simple because it can consist of a single variable called main.
A typical program consists of...
* Comments
* variables, of which one is called `main`
* Statements and Expressions

A Hello World program looks like this:
```c
main := proc () void {

    print("Hello, World!");
};
```
A few things to note:
* Functions are stored in variables like numbers or strings would. Therefore, it is important not to forget to type a semi-colon at the end of a function declaration.
* The assignment operator seems to be `:=`, but later you will notice that  `=` is also used for assignement. The difference between both assignment operators will be explained later, but for now, just trust this guide.
* The return type of the function is just before the block opening brace.
* The proc keyword is used to tell the interpreter that the variable is a function.

### Syntax

A typical COLAN-21 program will have a number of Tokens in it, which are either keywords, glyphs, identifiers or literals.

#### Semicolons
In COLAN-21, statements end with a semicolon. This fact will not seem strange to you if you have used a C-like language in the past, but for those coming from a BASIC or Python dialect (and other languages not using statement terminators), the proper way to terminate a statement in COLAN-21 is by typing a `;` at the end of the statement. If you refer to our previous example, you will notice that both `main`'s declaration and the `print` statement were terminated by a `;`. In darker times, the semicolon used to be mandatory for every kind of statement, but it is now no longer the case for those related control flow like `while` and `if`. So if you see some older examples with superfluous semicolons, don't be surprised, but please do ignore them.

#### Comments
Just like any other language, COLAN-21 supports these helpful lines of text left by programmers for other programmers to understand the code better. Usually, a comment will be ignored by ignored in the execution and will not affect the outcome of the progrm. However, unlike other languages, Comments are First-Class and thus not are really ignored and can actually be used to affect the outcome of a program, thus killing two birds with one stone... or well at least respecting the theme of the August 2021 LangJam. The feature's usefulnes is up to how you use it.

Comments are single line and are indicated with the `#` glyph like in the following example:
```c
# This prints 'Hello, World!' to the terminal
print("Hello, World!");
```

A *First-Class Comment* looks like this:
```c
#Hello world
x := 0;

# This prints "Hello world"
print([x | tostring]);
```

```c
# ^increment
x := 0;

# This prints 1
print([x | isset increment]);

if ([x | isset increment]) {
    x = x + 1;   
}

# try removing the ^increment in the comment
print(x);
```

#### Variables
Variables names are case sensitive and can have letters, numbers and underscores.

#### Keywords
COLAN-21 has a few reserved keywords that you cannot use for variable names;
* else
* float
* if
* int
* print
* proc
* return
* string
* void
* while

### Types And Variables
There are a few types in the language that one must be aware of;
1. *float* is a floating point number.
2. *int* is a signed integer.
3. *proc* is a function.
4. *string* is... ...a... string!
5. *void* is used exclusively as the return type of functions that return nothing.
6. There is no dedicated boolean type, you use integers for boolean operations like in C.

Declaring a variable is a pretty straightforward. First you type the variable's name, then you declare it's type. You can also assign it a value. for example:
```c
my_int : int = 5;
```

Will have the effect of declaring an integer variable with a value of five.
Do note that the language is gifted with some form of inference and as such you can omit the type from the declaration:
```c
my_int := 5;
```

Declaring a string is not too different:
```c
my_string_a : string;
my_string_b := "Hello";
```

However, functions require a bit more work. The first difference is that the type is written just after the assignment operator and also the form may vary depending on the return value and number of parameters the function has. If we return to our hello world example, we can see how parameter-less functions with no return value are declared (`my_function := proc () void { code here };`), but you can also have functions that take arguments and return values. Here's how you do that.

First, you type the name of your parameters followed by their types and separated by commas if there are more than one. Then after the parentheses you type out the type of the return value. In the function body, to return a value, just type the keyword `return` then the value you want to return next to it. Theoritically, the last expression evaluated can be used as return value. However, it is unclear at this time if that has been implemented or if it will be part of a post-jam update.

A simple *add* function:
```c
add := proc (a : int, b : int) int { return a + b; };
```

#### A Remark Concerning Floats
A floating point value in COLAN-21 is almost *ANYTHING* that has a `.` which is not within string litterals. This is important to remember, because you can use this fact to make your life a little bit easier. As is typical in other programming languages, something like `.5` means 0.5, but in COLAN-21, `5.` means a float with a value of 5 and single `.` alone is a float with a value of 0.

#### Type Casting
It is unavoidable, especially if you write programs that use a lot of math, that a time will come when you need to mix ints with floats. However, the COLAN-21 interpreter doesn't really like that and it may generate garbage values being generated. To prevent this, you will need to cast your values and variables. Casting is fairly simple, just write the type you want to cast to with parentheses within which you enclose the value or variable you want to cast.

For example:

```c
result : float = float(add(20, 30)) * .5;
```

The integer value of *50* is casted into a `float` before being multiplied by *.5*.

### Operators
COLAN-21 has all or most of the tradionational operators usually present in a programming language:
* +  add
* -  substract
* *  multiply
* /  divide
* %  modulo
* |  bitwise or
* ^  bitwise xor
* &  bitwise and
* <<     left bitshift
* >>     right bitshift
* ||     or
* &&     and
* !  unary not

### Conditions
In COLAN-21, `if`, `else if` and `else` work in an identical manner to C.

Example:
```c
# Doigt's classical "If Else If Spam", man I don't even know what *else* to say...
main := proc () void {

    temperature_celsius = 5; # this would ideally be random but right now it's just always cold
    if (temperature_celsius < .) { print("Welcome to Sibera!"); }
    else if (temperature_celsius < 5) { print("It's cold!"); }
    else if (temperature_celsius < 10) { print("It's cold."); }
    else if (temperature_celsius < 15) { print("It's slightly cold."); }
    else if (temperature_celsius < 25) { print("The weather is comfortable."); }
    else if (temperature_celsius < 30) { print("It's warming up."); }
    else if (temperature_celsius < 35) { print("It's hot."); }
    else { print("Hey, your hair is on fire, I'm on fire too! Is this fine? Yeah this is fine."); }
};
```

### Loops
In COLAN-21, we have the `while` which works exactly like in C, however we only have that type of loop.

### Other Features
COLAN-21 alos supports arrays, lists, structs, recursion and other features, however those were implemented while this guide was being written and the writer of this guide is thus unaware of how those work in concrete detail. It is advised to check the example scripts and test scripts in *tests/scripts*
