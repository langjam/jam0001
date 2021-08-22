# Auskommentiert

Auskommentiert is a language based on comments in a format, that can be found on
pages like Reddit. You can create a Topic (a Program if you want), and can
comment on the Topic. On every comment, you can leave your answers as another
comment.

You can swap comments, change the content of comments and store data in form of
comments.

## Execution

In order do execute the language interpreter, you have to do the following:

1. Go to Auskommentiert/Backend and run `npm install && npm run start_webserver`
2. Go to Auskommntiert/Frontend and `npm install && npm start`

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
