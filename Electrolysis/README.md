# Auskommentiert (German for commented out)

Auskommentiert is a language based on comments in a format, that can be found on
pages like Reddit. You can create a Topic (a Program if you want), and can
comment on the Topic. On every comment, you can leave your answers as another
comment.

This was originally intended as a Reddit bot, but the rules of the langjam state 
that the language must be easy to install, which would have been impossible 
because of API keys etc. This is why we made our own tiny Reddit clone.

You can swap comments, change the content of comments and store data in form of
comments.

## Execution

First, you have to install `npm`.

In order do execute the language interpreter, you have to do the following:

1. Go to Auskommentiert/Backend and run `npm install && npm run start_webserver`
2. Open another Terminal
3. Go to Auskommentiert/Frontend and `npm install && npm start`

After you have installed all dependencies and started the server, you can pick
one topic in the frontend. You may find some example Topics. If you want to
create your own, simply click on `Create Topic`. Sadly, you cannot edit your
comments after creation, so be aware of what you are typing. You can however
delete comments and write them again.

Everything you do (including program execution) will get written to
`state.json`. If you want to store you current state before execution, consider
creating a copy of `state.json`.

In order to execute a topic, comment `run` directly under the topic body.

One thing to be aware of is: you can see all modifications on comments live.
Meaning, your program will execute very slowly.

## Grammar

All code and data is inside comments. There are no variables, instead you just
use a comment field of your liking and refer to it's contents relative to the
current comment. If a comment contains non-valid data, it is only an error if the
comment gets executed.

Important are the comment getters; a comment getter is an expression that returns one or an array of comments. Examples: 

 - {get 2 comments above 1 up} returns an array with the two comments above the current one
 - {get comment} returns the current comment
 - {get comment 1 left 2 up 1 down} gets the comment you get when navigating one left, then two up and then 1 down

Operators are mostly normal, the only special operator is $: it evalutes the comment after it, so ${get comment} is an endless loop. If you stored a number two comments above, you can get it with ${get comment 2 up}

There are a few basics comment types:

 - swap _expression_
   - _expression_ must return an array of two WrappedComments which are then swapped
 - set _targetExpression_ to _valueExpression_
   - Sets the content of _targetExpression_ to _valueExpression_; _valueExpression_ is turned into a string; this is basically self-modifying code
   - Example: set {get comment} to 'log(5)'; upon executing this statement the next time, it will log 5
- move _targetExpression_ _navigation_
  - Moves all comments specified by _targetExpression_ using the _navigation_ directions.
  - Example: move {get comment} 1 left; moves the current comment one to the left; this is the only direction that's currently implemented
- if _expression_:
  - Only evaluates the children of this comment if the _expression_ is true; remember the :
- while _expression_:
  - Evaluates all children in a loop while _expression_ is true; remember the :

Because the language is self-modifying, the code you've written might get overwritten after running it!
