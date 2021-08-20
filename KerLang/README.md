
```text
/*
takes 2 arguments
has a minus
does not has a 3
*/
f;



f(x, y)
{
	returu x - y;
}



type = int/float/thing

ast_nodes = 
	const, input
	+, -, *, /, %,
	==, <,
	?:,




map id -> functions


function
	string name
	int parameter_count
	Node ast


f(x1, x2):
	return (+ 1 2 3 x1 x2)

Nodes


	Arg:
		int index

	Const:
		int value
	
	FunctionCall:
		int function_id
		vec<Node>

	LazyIfElse:
		Node cond
		Node if_node
		Node else_node






```
