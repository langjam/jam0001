# Polite-C
## Description
The polite-c compiler hates rude people, so you have to be kind in order to make it work as intended.
To do so, you have to put "first class" comments before most of the statements.

## Language features
The base syntax is that of C, with the exception that do-while is forbidden and parenthesis after for/while loops are mandatory.
On top of that, you have to be polite.
### Comparison
```c
//please, don't get this comparison wrong
if(a<b){

}
```
### For/while loops
```c
//please, don't get this comparison wrong
//please, loop without doing weird stuff
for(int i = 0; i<10; i++){
}
//thank you
```
Notice that we also have to add the comparison comment (since we are doing a comparison) and that we have to thank the compiler for looping
### Logic operators
```c
//please, operate following logic
if(a&&b||c){
}
```

### Bitwise operations
```c
//please, this stuff is already complicated
a = b&c;
```
## Requirements
In order to use this compiler, you need to have python and gcc installed

## Usage
Simply run the python script passing your file name as a command line parameter
