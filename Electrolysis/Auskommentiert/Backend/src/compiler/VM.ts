import * as AST from './grammar';
import {CommentProvider} from "./CommentProvider";
import { inspect } from 'util';
import { WrappedComment } from './WrappedComment';
import { sleep } from './Util';

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

class VMValueArray {
    private mArray : VMValue[];
    constructor(array : VMValue[]) {
        this.mArray = array;
    }
    get array() {
        return this.mArray;
    }
}

type VMValue = number | string | boolean | undefined | VMFunction | VMValueArray | WrappedComment;

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
    private async traverse(comment : WrappedComment) : Promise<VMValue | undefined> {
        if(comment.content === "")
            return;
        await sleep(100);
        let ast = comment.parseAST();
        //console.log(ast);
        if(ast.kind === AST.ASTKinds.WhileComment) {
            while(true) {
                let cond = await this.evaluateExpression(comment, ast.whileExpression);
                if(cond !== true) {
                    break;
                }
                await this.evaluteChildren(comment);
            }
        } else if(ast.kind === AST.ASTKinds.SetComment) {
            let comments = this.findComments(comment, ast.target);
            let target = await this.evaluateExpression(comment, ast.value);
            for(let c of comments.array) {
                if(!(c instanceof WrappedComment)) {
                    throw new Error("Not a comment!");
                }
                c.content = this.stringify(target);
            }
            this.mASTProvider.notifyContentChanged();
            
        } else if(ast.kind === AST.ASTKinds.IfComment) {
            if(await this.evaluateExpression(comment, ast.condition) === true) {
                await this.evaluteChildren(comment);
            }
        } else if(ast.kind === AST.ASTKinds.ManipulationCommentSwap) {
            let target = this.findComments(comment, ast.target);
            if(!(target instanceof VMValueArray)) {
                throw new Error("Not an array!");
            }
            if(target.array.length != 2) {
                throw new Error("Can only swap 2 comments, not " + target.array.length);
            }
            let commentA = target.array[0] as WrappedComment;
            let commentB = target.array[1] as WrappedComment;
            this.mASTProvider.swapFull(commentA.id, commentB.id);
        } else if(ast.kind === AST.ASTKinds.ManipulationCommentMove) {
            let target = this.findComments(comment, ast.target);
            if(!(target instanceof VMValueArray)) {
                throw new Error("Not an array!");
            }
            for(let comment of target.array) {
                if(!(comment instanceof WrappedComment)) {
                    throw new Error(comment + " is not a comment!");
                }
                for(let n of ast.nav.navigations) {
                    for(let i = 0; i < Number(n.distance); ++i ){
                        switch (n.dir) {
                            case "up":
                                this.mASTProvider.moveCommentUp(comment.id);
                                break;
                            
                            case "down":
                                this.mASTProvider.moveCommentDown(comment.id);
                                break;
                                
                            case "left":
                                this.mASTProvider.moveCommentLeft(comment.id);
                                break;
                            
                            case "right":
                                this.mASTProvider.moveCommentRight(comment.id);
                                break;
                        
                            default:
                                break;
                        }
                    }
                }
            }
            
        } else {
            return this.evaluateExpression(comment, ast as AST.Expression);
        }
    }
    private async evaluteChildren(comment : WrappedComment) {
        let current = this.mASTProvider.getFirstChildComment(comment.id);
        while(current !== undefined) {
            await this.traverse(current);
            current = this.mASTProvider.getNextComment(current.id);
        }
    }
    private async evalFunctionParams(parentComment : WrappedComment, params : AST.FunctionParameters | null):Promise<VMValue[]> {
        let ret : VMValue[] = [];
        if(params === null) {
            return ret;
        }
        ret.push(await this.evaluateExpression(parentComment, params.value));
        let restOfParams = params.next?.nextParam;
        if(restOfParams === undefined) {
            return ret;
        }
        return ret.concat(await this.evalFunctionParams(parentComment, restOfParams));
    }
    private async evalFunction(parentComment : WrappedComment, funcCall : AST.FunctionCall) : Promise<VMValue> {
        let func = await this.evaluateExpression(parentComment, funcCall.funcName);
        let params = await this.evalFunctionParams(parentComment, funcCall.params);
        if(func instanceof VMFunction) {
            return func.callback(params);
        }
        throw new Error(func + " is not a function!");
    }
    private async evaluateExpression(parentComment : WrappedComment, expression : AST.Expression) : Promise<VMValue> {
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
            } else if(expression.varName === "getUpvotes") {
                return new VMFunction(1, (p : VMValue[]) => {
                    return parentComment.upvotes;
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
            return new VMValueArray(await this.evalFunctionParams(parentComment, expression.listParams));
        } else if(expression.kind === AST.ASTKinds.AtomicExpression_8) {
            // list creation
            return expression.str.map((ch) => ch.char).join("");
        } else if(expression.kind === AST.ASTKinds.AddExpression) {
            let lhs = await this.evaluateExpression(parentComment, expression.lhs);
            let rhs = await this.evaluateExpression(parentComment, expression.rhs);
            if(lhs instanceof VMValueArray && rhs instanceof VMValueArray) {
                return new VMValueArray(lhs.array.concat(rhs.array));
            }
            return lhs as any + (rhs as any);
        } else if(expression.kind === AST.ASTKinds.SubExpression) {
            return (await this.evaluateExpression(parentComment, expression.lhs) as any) - (await this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.MulExpression) {
            return (await this.evaluateExpression(parentComment, expression.lhs) as any) * (await this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.DivExpression) {
            return (await this.evaluateExpression(parentComment, expression.lhs) as any) / (await this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.LessThanExpression) {
            return (await this.evaluateExpression(parentComment, expression.lhs) as any) < (await this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.MoreThanExpression) {
            return (await this.evaluateExpression(parentComment, expression.lhs) as any) > (await this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.LessEqualExpression) {
            return (await this.evaluateExpression(parentComment, expression.lhs) as any) <= (await this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.MoreEqualExpression) {
            return (await this.evaluateExpression(parentComment, expression.lhs) as any) >= (await this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.EqualsExpression) {
            return (await this.evaluateExpression(parentComment, expression.lhs) as any) == (await this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.NotEqualsExpression) {
            return (await this.evaluateExpression(parentComment, expression.lhs) as any) != (await this.evaluateExpression(parentComment, expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.FunctionCall) {
            return (await this.evalFunction(parentComment, expression));
        } else if(expression.kind === AST.ASTKinds.GetLengthExpression) {
            let l = await this.evaluateExpression(parentComment, expression.list);
            if(!(l instanceof VMValueArray)) {
                throw new Error("Not an array!");
            }
            return (l as VMValueArray).array.length;
        } else if(expression.kind === AST.ASTKinds.IndexExpression) {
            let l = await this.evaluateExpression(parentComment, expression.list);
            if(!(l instanceof VMValueArray)) {
                throw new Error("Not an array!");
            }
            let index = await this.evaluateExpression(parentComment, expression.index);
            return l.array[Number(index)];
        } else if(expression.kind === AST.ASTKinds.CommentSelector) {
            return this.findComments(parentComment, expression);
        } else if(expression.kind === AST.ASTKinds.EvalExpression) {
            let comments : VMValue = await this.evaluateExpression(parentComment, expression.toEval);
            if(!(comments instanceof VMValueArray)) {
                throw new Error("You can only evaluate comments, not " + comments);
            }
            if(comments.array.length == 1) {
                if(!(comments.array[0] instanceof WrappedComment)) {
                    throw new Error("Not a comment!");
                }
                let ret = await this.traverse(comments.array[0]);
                return ret;
            }
            let ret = [];
            for(let c of comments.array) {
                if(!(c instanceof WrappedComment)) {
                    throw new Error("Not a comment!");
                }
                ret.push(await this.traverse(c));
            }
            return new VMValueArray(ret);
        }
        throw new Error("Unknown kind " + expression["kind"]);
        return false;
    }
    findComments(parentComment : WrappedComment, expression : AST.CommentSelector): VMValueArray {
        let count = 0;
        let aboveBelow = "";
        if(expression.countSelector.kind === AST.ASTKinds.CommentSelector_$0_2) {
            count = Number(expression.countSelector.count);
            aboveBelow = expression.countSelector.aboveBelow;
        }
        let current : WrappedComment = parentComment;
        let next : WrappedComment | undefined;
        for(let nav of expression.navigations.navigations) {
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
                    throw new Error("Can't navigate further " + nav.dir + ", should have moved " + nav.distance + ", but only could move " + i);
                }
                current = next;
            }
        }
        let ret = [current];
        if(aboveBelow === "above" || aboveBelow === "below") {
            for(let i = 1; i < count; ++i) {
                let c = undefined;
                if(aboveBelow === "above") {
                    c = this.mASTProvider.getPrevComment(current.id);
                } else if(aboveBelow === "below") {
                    c = this.mASTProvider.getNextComment(current.id);
                }
                if(c === undefined) {
                    throw new Error("Can't navigate there!");
                }
                current = c;
                ret.push(current);
            }
        }
        return new VMValueArray(ret);
    }
}