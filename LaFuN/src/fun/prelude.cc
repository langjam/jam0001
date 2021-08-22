#include "prelude.h"

namespace fun {

std::string jsPrelude = R"javascript(/* <Prelude> */
class FUNclass_Array {
	constructor() {
		this.data = [];
	}

	push(val) {
		return this.data.push(val);
	}

	pop() {
		return this.data.pop()
	}

	get(idx) {
		return this.data[idx];
	}

	set(idx, val) {
		return this.data[idx] = val;
	}
}

function FUN_Array() {
	return new FUNclass_Array();
}

function FUN_print() {
	console.log.apply(console, arguments);
}

let FUN_true = true;
let FUN_false = false;
let FUN_none = null;
/* </Prelude> */
)javascript";

std::string jsPostlude = R"javascript(
FUN_main();
)javascript";

const std::vector<std::string> preludeNames = {
	"Array", "print", "true", "false", "none",
};

}
