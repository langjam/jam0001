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

type VMValue = number | string | boolean | null | VMFunction | VMValue[];

export class VM {
    private mASTProvider : CommentProvider;
    private mVariables : Map<String, VMValue>[] = [];
    constructor(astProvider : CommentProvider) {
        this.mASTProvider = astProvider;
    }
    public async run() {
        this.mVariables.push(new Map());
        let currentComment = this.mASTProvider.getFirstComment();
        
        while(currentComment !== undefined) {
            this.traverse(currentComment);
            currentComment = this.mASTProvider.getNextComment(currentComment.id);
        }
    }
    private traverse(comment : WrappedComment) {
        if(comment.ast.kind === AST.ASTKinds.WhileComment) {
            while(true) {
                if(this.evaluateExpression(comment.ast.whileExpression) !== true) {
                    break;
                }
                this.evaluteChildren(comment);
            }
        } else if(comment.ast.kind === AST.ASTKinds.AssignmentComment) {
            // check for index
            if(comment.ast.indexPart === null) {
                this.mVariables[this.mVariables.length - 1].set(comment.ast.varName, this.evaluateExpression(comment.ast.rhsExpr));
                return;
            }
            let list = this.mVariables[this.mVariables.length - 1].get(comment.ast.varName) as VMValue[];
            let index = this.evaluateExpression(comment.ast.indexPart.index);
            list[Number(index)] = this.evaluateExpression(comment.ast.rhsExpr);
        } else if(comment.ast.kind === AST.ASTKinds.FunctionCall) {
            this.evalFunction(comment.ast);
        } else if(comment.ast.kind === AST.ASTKinds.IfComment) {
            if(this.evaluateExpression(comment.ast.condition) === true) {
                this.evaluteChildren(comment);
            }
        } else {
            throw new Error("Unknown kind " + comment.ast["kind"]);
        }
    }
    private evaluteChildren(comment : WrappedComment) {
        let current = this.mASTProvider.getFirstChildComment(comment.id);
        while(current !== undefined) {
            this.traverse(current);
            current = this.mASTProvider.getNextComment(current.id);
        }
    }
    private evalFunctionParams(params : AST.FunctionParameters | null):VMValue[] {
        let ret : VMValue[] = [];
        if(params === null) {
            return ret;
        }
        ret.push(this.evaluateExpression(params.value));
        let restOfParams = params.next?.nextParam;
        if(restOfParams === undefined) {
            return ret;
        }
        return ret.concat(this.evalFunctionParams(restOfParams));
    }
    private evalFunction(funcCall : AST.FunctionCall) : VMValue {
        let func = this.evaluateExpression(funcCall.funcName);
        let params = this.evalFunctionParams(funcCall.params);
        if(func instanceof VMFunction) {
            return func.callback(params);
        }
        throw new Error(func + " is not a function!");
    }
    private evaluateExpression(expression : AST.Expression) : VMValue {
        if(expression.kind === AST.ASTKinds.AtomicExpression_1) {
            return true;
        } else if(expression.kind === AST.ASTKinds.AtomicExpression_2) {
            return false;
        } else if(expression.kind === AST.ASTKinds.AtomicExpression_3) {
            let val = this.mVariables[this.mVariables.length - 1].get(expression.varName);
            if(val === undefined) {
                if(expression.varName === "log") {
                    return new VMFunction(1, (p : VMValue[]) => {
                        console.log(p);
                    });
                } else if(expression.varName === "sqrt") {
                    return new VMFunction(1, (p : VMValue[]) => {
                        return Math.sqrt(p[0] as number);
                    });
                }
                throw new Error(val + " is not defined");
            }
            return val;
        } else if(expression.kind === AST.ASTKinds.AtomicExpression_4) {
            // numeric literal
            return Number(expression.num);
        } else if(expression.kind === AST.ASTKinds.AtomicExpression_5) {
            // sub expression
            return this.evaluateExpression(expression.sub);
        } else if(expression.kind === AST.ASTKinds.AtomicExpression_6) {
            // list creation
            return this.evalFunctionParams(expression.listParams);
        } else if(expression.kind === AST.ASTKinds.AddExpression) {
            return this.evaluateExpression(expression.lhs) as any + (this.evaluateExpression(expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.SubExpression) {
            return this.evaluateExpression(expression.lhs) as any - (this.evaluateExpression(expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.MulExpression) {
            return this.evaluateExpression(expression.lhs) as any * (this.evaluateExpression(expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.DivExpression) {
            return this.evaluateExpression(expression.lhs) as any / (this.evaluateExpression(expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.LessThanExpression) {
            return this.evaluateExpression(expression.lhs) as any < (this.evaluateExpression(expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.MoreThanExpression) {
            return this.evaluateExpression(expression.lhs) as any > (this.evaluateExpression(expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.LessEqualExpression) {
            return this.evaluateExpression(expression.lhs) as any <= (this.evaluateExpression(expression.rhs) as any);
        } else if(expression.kind === AST.ASTKinds.MoreEqualExpression) {
            return this.evaluateExpression(expression.lhs) as any >= (this.evaluateExpression(expression.rhs) as any);
        } else if(expression.kind == AST.ASTKinds.FunctionCall) {
            return this.evalFunction(expression);
        } else if(expression.kind == AST.ASTKinds.GetLengthExpression) {
            let l = this.evaluateExpression(expression.list);
            return (l as VMValue[]).length;
        } else if(expression.kind == AST.ASTKinds.IndexExpression) {
            let l = this.evaluateExpression(expression.list) as VMValue[];
            let index = this.evaluateExpression(expression.index);
            return l[Number(index)];
        }
        throw new Error("Unknown kind " + expression["kind"]);
        return false;
    }
}