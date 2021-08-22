# DJs

DJs is an interpreter that works on JS files and add sound effects based on comments configuration.

# Prerequisites

- You'll need to have [node](https://nodejs.org/en/download/) installed.

- You'll need to install the [SoX](http://sox.sourceforge.net/) library:
```
sudo apt-get install sox
```
Or download its binary on [SoX source forge](https://sourceforge.net/projects/sox/files/sox/14.4.2/).

# Usage
DJS is usable on any single .js file. It will execute the file normally, and if the file contains DJS comment will play a melody during the execution.

To test the program with one of our example you can run the following `.sh` :

If you have sox in your path

`./nodjs examples/example.js`

If you have no instance of sox in your path, just specify the path too the binary

`./nodjs examples/example.js ./lib/sox`


# How the comments work

Comments should be placed before a function that must be defined with the syntax `function name() {`.
The comments can contains multiples lines. There is multiple kinds of effects available to configure the DJS program.

## @var

When using the tag __var__, the following argument must be the name of a variable.
After the variable, the following args goes in pair of two, one note, and the octave of the desired note.

```
@var [variable] [note] [octave]
```

Multiple notes can be mapped to the same variable to create chords.

```
@var bar mi 3
@var foo re 4 do 2
```

The available notes are __C__, __C#__, __D__, __E#__, __E__, __F__, __G#__, __G__, __A#__, __A__, __B#__, __B__, __do__, __reB__, __re__, __miB__, __mi__, __fa__, __solB__, __sol__, __laB__, __la__, __siB__, __si__ and __silence__ (for a pause)

When encountered in the function, each variable will then play the note or chord that was configured. If multiple variable are on the same line, they'll be played in the order seen from left ro right.

The _duration_ of the note is defined by the value of the variable.

For arrays and strings, it depends on the length of the variable %6
For integers, the value of the variable %6

The smallest the value, the fastest the note / chord.

## @waveType

When using the tag __waveType__, only one argument is needed to indicate the type of wave used to make the sound.
It can be one of the following: _sine_, _square_, _triangle_, _sawtooth_, _trapezium_, _exp_, [white]_noise_, _tpdfnoise_, _pinknoise_, _brownnoise_, _pluck_, _sine_.
If no wavetype is precised, sine will be used.
