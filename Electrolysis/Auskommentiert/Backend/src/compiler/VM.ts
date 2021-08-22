import * as AST from './grammar';
import {CommentProvider} from "./CommentProvider";
import { inspect } from 'util';
import { WrappedComment } from './WrappedComment';

class VMFunction {
    private mParamCount : number;
    private mCallback : Function;
    constructor(paramCount : number, callback : Function) {
        this.mParamCount = paramCount;
        this.mCallback = callback;
    }
    get paramCount() {
        return this.mParamCount;
    }
    get callback() {
        return this.mCallback;
    }
}

class FoundComments {
    private mComments : WrappedComment[];
    constructor(comments : WrappedComment[]) {
        this.mComments = comments;
    }
    get comments() {
        return this.mComments;
    }
}
class VMValueArray {
    private mArray : VMValue[];
    constructor(array : VMValue[]) {
        this.mArray = array;
    }
    get array() {
        return this.mArray;
    }
}

type VMValue = number | string | boolean | undefined | VMFunction | VMValueArray | FoundComments;

export class VM {
    private mASTProvider : CommentProvider;
    constructor(astProvider : CommentProvider) {
        this.mASTProvider = astProvider;
    }
    public async run() {
        let currentComment = this.mASTProvider.getFirstComment();
        
        while(currentComment !== undefined) {
            this.traverse(currentComment);
            currentComment = this.mASTProvider.getNextComment(currentComment.id);
        }
    }
    private stringify(value : VMValue) : string {
        if(value instanceof VMValueArray) {
            return "[" + value.array.map(c => this.stringify(c)).join(", ") + "]";
        }
        return String(value);
    }
    private traverse(comment : WrappedComment) : VMValue | undefined {
        if(comment.content === "")
            return;
        let ast = comment.parseAST();
        //console.log(ast);
        if(ast.kind === AST.ASTKinds.WhileComment) {
            while(true) {
                let cond = this.evaluateExpression(comment, ast.whileExpression);
                if(cond !== true) {
                    break;
                }
                this.evaluteChildren(comment);
            }
        } else if(ast.kind === AST.ASTKinds.SetComment) {
            let comments = this.findComments(comment, ast.target);
            let target = this.evaluateExpression(comment, ast.value);
            for(let c of comments.comments) {
                c.content = this.stringify(target);
            }
            
        } else if(ast.kind === AST.ASTKinds.IfComment) {
            if(this.evaluateExpression(comment, ast.condition) === true) {
                this.evaluteChildren(comment);
            }
        } else {
            return this.evaluateExpression(comment, ast as AST.Expression);
        }
    }
    private evaluteChildren(comment : WrappedComment) {
        let current = this.mASTProvider.getFirstChildComment(comment.id);
        while(current !== undefined) {
            this.traverse(current);
            current = this.mASTProvider.getNextComment(current.id);
        }
    }
    private evalFunctionParams(parentComment : WrappedComment, params : AST.FunctionParameters | null):VMValue[] {
        let ret : VMValue[] = [];
        if(params === null) {
            return ret;
        }
        ret.push(this.evaluateExpression(parentComment, params.value));
        let restOfParams = params.next?.nextParam;
        if(restOfParams === undefined) {
            return ret;
        }
        return ret.concat(this.evalFunctionParams(parentComment, restOfParams));
    }
    private evalFunction(parentComment : WrappedComment, funcCall : AST.FunctionCall) : VMValue {
        let func = this.evaluateExpression(parentComment, funcCall.funcName);
        let params = this.evalFunctionParams(parentComment, funcCall.params);
        if(func instanceof VMFunction) {
            return func.callback(params);
        }
        throw new Error(func + " is not a function!");
    }
    private evaluateExpression(parentComment : WrappedComment, expression : AST.Expression) : VMValue {
        if(expression.kind === AST.ASTKinds.AtomicExpression_1) {
            return true;
        } else if(expression.kind === AST.ASTKinds.AtomicExpression_2) {
            return false;
        } else if(expression.kind === AST.ASTKinds.AtomicExpression_3) {

            if(expression.varName === "log") {
                return new VMFunction(1, (p : VMValue[]) => {
                    console.log(p.map(p => this.stringify(p)).join(" "));
                });
            } else if(expression.varName === "sqrt") {
                return new VMFunction(1, (p : VMValue[]) => {
                    return Math.sqrt(p[0] as number);
                });
            }
            throw new Error(expression.varName + " is not defined");
        } else if(expression.kind === AST.ASTKinds.AtomicExpression_4) {
            // numeric literal
            return Number(expression.num);
        } else if(expression.kind === AST.ASTKinds.AtomicExpression_5) {
            // sub expression
            return this.evaluateExpression(parentComment, expression.sub);
        } else if(expression.kind === AST.ASTKinds.AtomicExpression_6) {
            // list creation
            return new VMValueArray(this.evalFunctionParams(parentComment, expression.listParams));
        } else if(expression.kind === AST.ASTKinds.AddExpression) {
            let lhs = this.evaluateExpression(parentComment, expression.lhs);
            let rhs = this.evaluateExpression(parentComment, expression.rhs);
            if(lhs instanceof VMValueArray && rhs instanceof VMValueArray) {
                return new VMValueArray(lhs.array.concat(rhs.array));
            }
            return lhs as any + (rhs as any);
        } else if(expression.kind === AST.ASTKinds.SubExpression) {
            return this.evaluateExpression(parentComment, expression.lhs) as any - (this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.MulExpression) {
            return this.evaluateExpression(parentComment, expression.lhs) as any * (this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.DivExpression) {
            return this.evaluateExpression(parentComment, expression.lhs) as any / (this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.LessThanExpression) {
            return this.evaluateExpression(parentComment, expression.lhs) as any < (this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.MoreThanExpression) {
            return this.evaluateExpression(parentComment, expression.lhs) as any > (this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.LessEqualExpression) {
            return this.evaluateExpression(parentComment, expression.lhs) as any <= (this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.MoreEqualExpression) {
            return this.evaluateExpression(parentComment, expression.lhs) as any >= (this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.FunctionCall) {
            return this.evalFunction(parentComment, expression);
        } else if(expression.kind === AST.ASTKinds.GetLengthExpression) {
            let l = this.evaluateExpression(parentComment, expression.list);
            if(!(l instanceof VMValueArray)) {
                throw new Error("Not an array!");
            }
            return (l as VMValueArray).array.length;
        } else if(expression.kind === AST.ASTKinds.IndexExpression) {
            let l = this.evaluateExpression(parentComment, expression.list);
            if(!(l instanceof VMValueArray)) {
                throw new Error("Not an array!");
            }
            let index = this.evaluateExpression(parentComment, expression.index);
            return l.array[Number(index)];
        } else if(expression.kind === AST.ASTKinds.CommentSelector) {
            return this.findComments(parentComment, expression);
        } else if(expression.kind === AST.ASTKinds.EvalExpression) {
            let comments = this.evaluateExpression(parentComment, expression.toEval);
            if(!(comments instanceof FoundComments)) {
                throw new Error("You can only evaluate comments, not " + comments);
            }
            if(comments.comments.length == 1) {
                let ret = this.traverse(comments.comments[0]);
                return ret;
            }
            return new VMValueArray(comments.comments.map((c) => this.traverse(c)));
        }
        throw new Error("Unknown kind " + expression["kind"]);
        return false;
    }
    findComments(parentComment : WrappedComment, expression : AST.CommentSelector): FoundComments {
        let count = Number(expression.count);
        let current : WrappedComment = parentComment;
        let next : WrappedComment | undefined;
        for(let nav of expression.navigations) {
            for(let i = 0; i < Number(nav.distance); ++i) {
                switch (nav.dir) {
                    case "up":
                        next = this.mASTProvider.getPrevComment(current.id);
                        break;
                    case "down":
                        next = this.mASTProvider.getNextComment(current.id);
                        break;
                    case "left":
                        next = this.mASTProvider.getParentComment(current.id);
                        break;
                    case "right":
                        next = this.mASTProvider.getFirstChildComment(current.id);
                        break;
                
                    default:
                        throw new Error("This shouldn't happen!");
                }
                if(next === undefined) {
                    throw new Error("Can't navigate there!");
                }
                current = next;
            }
        }
        return new FoundComments([current]);
    }
}