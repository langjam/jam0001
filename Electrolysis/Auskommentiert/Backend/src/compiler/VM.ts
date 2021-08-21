import * as AST from './grammar';
import {CommentProvider} from "./CommentProvider";
import { inspect } from 'util';

type VMValue = number | string | boolean | void | Function;

export class VM {
    private mASTProvider : CommentProvider;
    private mVariables : Map<String, VMValue>[] = [];
    constructor(astProvider : CommentProvider) {
        this.mASTProvider = astProvider;
    }
    public async run() {
        let firstComment = this.mASTProvider.getFirstComment();
        if(firstComment === undefined) {
            return;
        }
        this.mVariables.push(new Map());
        this.traverse(firstComment.ast);
    }
    private traverse(ast : AST.Comment) {
        if(ast.kind == AST.ASTKinds.WhileComment) {
            while(true) {
                if(this.evaluateExpression(ast.whileExpression) !== true) {
                    break;
                }
                
            }
        }
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
                    return (p : VMValue) => {
                        console.log(p);
                    }
                }
                throw new Error(val + " is not defined");
            }
            return val;
        }
        console.log(inspect(expression, false, null, true));
        return false;
    }
}