# Documentation

> Unlike the other markdown files in this repository, you cannot run this markdown file. This is because the code snippets in here actually include markdown inside code blocks which would confuse the parser. I apologize for the inconvenience.

## Language Structure

MarkSideways uses the overall structure of your markdown file to determine the structure of your program.

If you include full-block code snippets in your markdown file, this code is executed by MarkSideways. The location of these code blocks in relation to headers determines what their purpose is and what classes and methods they belong to.

### Declaring Classes 

To create a class, create a heading with `##`. The name of the heading will be the name of the class.

For example, if you wanted to create a class called `Foo`, you would create a heading called `## Foo`.

Any code blocks located *directly* below the class heading will be part of the constructor for that class.

### Declaring Methods

You can declare a method for a class by creating a sub-heading with `###`. The name of the heading will be the name of the method. For example, if you have a heading called `## Person` and a sub-heading called `### Say Hello`, this would create a class called `Person` with a method called `sayHello()`. Any code blocks that appears below the sub-heading Say Hello will be the code for the `sayHello()` method.

> All names are case/space/punctuation-insensitive. This is to ensure that the parsing is unambiguous but also allows for normal human-readable headings.

### <s>Inserting comments</s> Inserting Code

Comments are **FIRST CLASS**. That is to say that everything in the MarkDown is a comment and you specifically insert code into your program by using the code block (triple backtick) markdown syntax. If you want to insert a comment into your code, you can stop the code block with another triple backtick and include any markdown you want. Then you can resume the code with another triple backtick. All code blocks that exist under a heading or sub-heading will be concatenated together. 

Don't think of it as adding comments to code, think of it as adding code to comments.

### Functions outside of classes

To create a function outside of a class, you can create a sub-heading under the main title before any classes are declared. This means that there'll be a `###` sub-heading below your `#` title. These functions can be called without being a member of a class.

### Arguments to functions and methods

You can declare arguments to methods, functions, and constructors by including a bulleted list under the corresponding heading/subheading. This bulleted list item must include an inline code snippet using backtick syntax (this will be the name of the argument). Anything after the inline code snippet is just documentation for the purpose of the argument.
- `argument1` - the first argument
- `argument2` - ...and his friend

The bulleted list doesn't have to be contiguous. You can add more arguments after more markdown.

- `argument3` - this is perfectly valid and will be included as a third argument after the first two even though there's text between them.

Arguments can be used by their methods/functions/constructors like normal variables:

```
print("The arguments passed in were " + argument1 + ", " + argument2 + ", and " + argument3);
```

### Where does program execution begin?

The code snippets included directly under the main title before any functions or classes are declared are executed upon the start of your program. All class and function declarations are "hoisted" and so you can refer to them in this initial code.

Additionally, you can use the `args` variable to access the command line arguments passed to the program. This variable is automatically declared for you in your top-level variable scope (but only in the local scope of the top level, NOT the global scope).

## The Scripting Language Syntax

We've covered the markdown side of things and the fact that you can include scripting code in code blocks surrounded by markdown. Now let's learn about the scripting language itself.

The MarkSideways code block scripting language is a simple curly brace language that should feel familiar to anyone that's used JavaScript, PHP, C, C#, Java, etc with some influence from Swift/Rust/Go.

### Variables

Variables do not follow code block lexical scoping. The local scope only exists as a single instance for each function's stack frame. Variables do not need to be explicitly declared, just simply used. This is similar to Python or PHP. 

`myValue = "Hello";`

The value types that are available are as follows:
- **Booleans**: `true` or `false` are the constant values that you can use.
- **Integers**: whole numbers. The underlying interpreter is Python and so these don't have a bound.
- **Floats**: decimal numbers. These are a separate type from integers **even if** the decimal happens to end with a .0. If you want to convert a float to an integer, you must explicitly call `floor()`.
- **Strings**: text values. You can create a text value by using single or double quotes. Standard escape sequences can be used: `\n`, `\r`, `\\`, `\t`, `\'`, `\"`, `\0`
- **Arrays**: linear lists of information. These are expandable, not fixed-length.
- **Dictionaries**: collections of key-value-pairs. 
- **Null**: this is its own singleton value. You can use the `null` constant in your code.
- **Classes**: these represent classes. The only thing you can do with these is call `.init()` on them to invoke the constructor to create a new instance.
- **Instances**: these are instances of classes. Instances do not have fixed fields so you can assign any field to these.
- **Functions/Methods/Built-In Functions/Constructors** - These are the invokable values. They can be passed around as function pointers.

### Control Flow

If statements work similarly to other curly brace languages. It is the word `if` followed by a boolean condition. If the condition is not a boolean, this will cause an error. The boolean condition does NOT need to be surrounded by parentheses. If the condition is true, the next block will execute. The code block MUST have curly braces (similar to Go).

```
if calories > 2000 {
    print("You've had enough today.");
}
```

`else` statements can also be used and chained in multiple `if`/`else` statements:

```
if temperature > 85 {
    openWindow();
} else if temperature > 75 {
    complain();
} else {
    doNothing();
}
```

There are three kinds of loops: `while`, `for`, and `do while`.

`while` and `do while` work similarly to other curly brace languages.

A `while` loop works just like an if statement that repeats until the condition is no longer true. If the condition is NOT a boolean, this will cause an error to be thrown.

```
i = 1;
while i <= 10 {
    print(i);
    i += 1;
}
```

A `do while` loop is like a backwards while loop, where the condition is checked at the end.

```
i = 1;
do {
    print(i);
    i += 1;
} while (i <= 10);
```

A `for` loop takes in a starting integer and an ending integer and assigns it to an iterating variable.

```
for i = 1 thru 10 {
    print(i);
}
```
This loop will also count to 10 because we have used the `thru` operator. However, we can also use the `till` operator to not include 10:
```
for i = 1 till 10 {
    print(i);
}
```
This loop only goes up to 9.

If the first number is smaller than the second number, then the loop will count backwards.

You can break out of loops by using the `break` statement. You can also use `continue` to cause the loop to begin its next iteration and skip the rest of the loop body.

```
x = 1;
while true {
    x *= 2;
    if x > 1000 {
        break;
    }
}
```

### Data Structures

The two built-in data structures are arrays and dictionaries. 
You can declare arrays by using `[` and `]` and listing the starting values with commas.

```
things = ['cats', 'dogs', 'another cat'];
```

Including a comma after the last element is allowed.

You are also free to just create an empty list:

```
things = [];
```

Dictionaries are created using the `{` and `}` characters. Dictionaries require a list of keys and values separated by colons where each pair is separated by a comma.

```
countries = {
    'us': "United States",
    'ca': "Canada",
    'cn': "China",
    'uk': "United Kingdom",
    'mx': "Mexico",
    'in': "India",
};
```
Like arrays, a comma after the last element is allowed.

Strings are the most common type of value for keys but almost all data types can be used with the exceptions of arrays, dictionaries, nulls, and floats.

You can access items in arrays and dictionaries by using square brackets.

```
print(countries['ca']);
print(things[0]);
```

### Using classes and methods

To create a class, you can invoke the class by name followed by a `.init()`. This will call the constructor. The constructor's code is defined in the code blocks directly under the class' heading but not under any sub-heading.

````

# Sample Program

Create a person and make them say hello.

```
billy = Person.init("Billy", 40);
```
Remember, code does not need to be contiguous. As long as it falls under the
same header, it will be treated as continugous by the parser.
```
billy.sayGreeting();
```

Invoking the function can be done with `.sayGreeting()` or `.SayGreeting()` or `.sAyGrEtInG()` since most entities are case/space/puncutation insensitive.

## Person

Creates a new person instance.

- `name` - the person's name
- `age` - the person's age

```
this.name = name;
this.age = age;
```

### Say Greeting

Creates a greeting and prints it to `STDOUT`.
```
print("Hello, my name is " + this.age + " and I am " + this.age + " years old.");
```
````

If a function requires arguments, you must provide the exact correct number of arguments. There are no implicit fallback values (There are some exceptions to this such as specific built in functions like `print` which can take in any number of arguments).

## Built In Library

There is a variety of built-in functions for you to use.

### General purpose functions
- `assert(condition, message)` - throws an error with the given message if the condition is false.
- `floor(number)` - takes in a number and returns the integer directly below or equal to that number.
- `json_parse(string)` - parses a JSON string and turns it into nested data (using dictionaries, arrays, etc)
- `json_serialize(value)` - takes in an arbitrary value and converts it into a JSON string. The values must be of appropriate JSON types such as arrays, dictionaries, strings, integers, etc.
- `parse_int(string)` - takes in a string and tries to convert it into a number. Returns `null` upon failure.
- `print(...)` - can take in any number of arguments and prints them to `STDOUT`.
- `random_float()` - returns a float between 0 (inclusive) and 1 (exclusive).
- `read_input(optionalPrompt)` - reads input from `STDIN`. If you pass in a string, it will use the string as the prefix for the prompt.
- `sqrt(number)` - returns the square root of a number as a float, even if it is a whole number, mathematically.
- `unix_time()` - returns the current unix timestamp as a float.

### Methods and properties on built-in types

Due to time restrictions these are somewhat limited than what you would typically find in a programming language. These were basically implemented on an as-needed basis for each of the demo apps.

- `string.length` - returns the length of a string as an integer
- `array.length` - returns the length of an array
- `array.add(items...)` - adds the items to the end of an array
- `array.pop()` - removes items from the end of an array
- `dictionary.length` - returns the size of a dictionary
- `dictionary.get(key, optionalFallback)` - returns the value in the dictionary at the given key. You can pass in an optional 2nd parameter to return in the event that the key does not exist. If not provided, `null` is returned instead. This is the safe alternative to using bracket syntax directly, (which will cause an error if the key doesn't exist).
- `dictionary.remove(key)` - removes the value at the given key.
- `dictionary.keys()` - returns a new array that contains all the keys in the dictionary.
- `dictionary.values()` - returns a new array that contains all the values in the dictionary.
- `class.init(...)` - this is the constructor syntax. The arguments must match the actual constructor as defined.

### Game Development functions

MarkSideways provides limited interop with SDL2 (via PyGame because the interpreter is written in Python). In order for these functions to work, you may have to run `pip install pygame` before running the interpreter.

- `game_create_window(title, width, height, fps)` - creates a new SDL-based game window of the given size and title. The game will attempt to run at the given frames per second.
- `game_draw_rectangle(x, y, width, height, red, green, blue)` - draws a rectangle to the game window screen with the left and top corner positioned at `x` and `y` and has the given `width` and `height`. The color is RGB values between 0 and 255.
- `game_end_frame()` - This concludes the current frame and flushes all the drawing events to the window and pauses just long enough to maintain a consistent frame rate. You must call this in your game loop, otherwise the window will become unresponsive to the OS.
- `game_fill_screen(r, g, b)` - fills the screen with a solid color. These are RGB color values from 0 to 255.
- `game_get_events()` - gets a list of keyboard events that have occurred since the previous frame. These events are in the format of `keyname:press` for key presses and `keyname:release` for key releases. The values for keys can be the following:
  - `a` through `z` for the letter keys
  - `num0` through `num9` for the number keys
  - `f1` through `f12` for the F1 to F12 keys
  - `up`, `down`, `left`, `right` for arrow keys
  - `space`, `enter`, `escape` for those keys
  - `ctrl`, `shift`, `alt` for modifier keys
- `game_is_key_pressed(keyname)` - returns a boolean indicating whether the given key is currently pressed. The key name can be seen in the list for `game_get_events()`
- `game_is_quit()` - returns a boolean for if the user has tried to close the window. This is a good function to use for your main game loop's while condition.
- `game_set_title(string)` - this lets you set the title of the window after it has been opened.

### HTTP Server functions

MarkSideways has a built-in HTTP server. Creating a server is mostly a matter of creating handlers for specific HTTP paths/method combinations and then calling a function to start serving traffic.

- `http_server_create_handler(method, path, handler)` - This register the `handler` function in the server so that it will be invoked when an incoming HTTP request comes for the `path` with the given `method`. This handler method must take in exactly 2 arguments: `method` and `content` which will be set to the HTTP method and the body of the HTTP request's content, if present. This function must also return a dictionary with 4 keys: 
  - `status` - the status message of the response, such as `OK` or `Not Found`
  - `statusCode` - the numeric status code of the response, such as `200` or `404`
  - `contentType` - the content type of the response, such as `text/html` or `application/json`
  - `content` - the actual body of the response as a string, such as `<html><body>hello</body></html>`
- `http_server_start(port)` - once all the handlers have been registered, this function will block for the lifetime of the server and start handling requests on the given port number.
