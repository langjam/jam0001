- [x] Create File Structure
- [x] The Theme: "first class comments"
- [x] Decide for a name `comstruct` and theme `the constructor is named comment and a comment is defined using comment: This is an example`
- [x] Decide for Tokens
- [x] Decide for a Language tree
- [x] Implement the Lexer
- [ ] Work on the Parser and the Executor
- [ ] Have FUN!
- [ ] Submit a PR (before the end of the jam)


```
typeclass = class({
    NUM=0?
    STR=1?
    OBJ=2?
})?
TYPE = typeclass()?


myclass = class({ 
    comment = function({
        comment: Kommentar
    }, TYPE.NUM, TYPE.OBJ, TYPE.STR)?
})?
```
