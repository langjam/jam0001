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
class = c(){ 
    comment = f(env_for_class, arg1, arg2){
        comment: Kommentar
    }
    
    comment = f(env_for_class, arg1){
        comment: Kommentar
    }
}
```


BaseNode#appendStatement(StatementNode(*args -> <type: Str, *runargs | SubtreeNode -extends BaseNode- >))
BaseNode.appendStatement(PrintNode("Hello World!!1!"))
