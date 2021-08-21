import * as AST from './grammar';
import {CommentProvider} from "./CommentProvider";
import { inspect } from 'util';

type VMValue = number | string | boolean;

export class VM {
    private mASTProvider : CommentProvider;
    constructor(astProvider : CommentProvider) {
        this.mASTProvider = astProvider;
    }
    public async run() {
        let firstComment = this.mASTProvider.getFirstComment();
        if(firstComment === undefined) {
            return;
        }
        this.traverse(firstComment.content);
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
            // variable
        }
        console.log(inspect(expression, false, null, true));
        return false;
    }
}