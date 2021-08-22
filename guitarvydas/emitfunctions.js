var ohm = require ('ohm-js');

"use strict"

//var glue = require ('./glue');
// npm install ohm-js
'use strict';

const glueGrammar =
      `
SemanticsSCL {
  semantics = ws* semanticsStatement+
  semanticsStatement = ruleName ws* "[" ws* parameters "]" ws* "=" ws* code? rewrites ws*

  ruleName = letter1 letterRest*
  
  parameters = parameter*
  parameter = treeparameter | flatparameter
  flatparameter = fpws | fpd
  fpws = pname ws+
  fpd = pname delimiter
  treeparameter = "@" tflatparameter
  tflatparameter = tfpws | tfpd
  tfpws = pname ws+
  tfpd = pname delimiter

  pname = letterRest letterRest*
  rewrites = rw1 | rw2
  rw1 = "[[" ws* code? rwstringWithNewlines "]]" ws*
  rw2 = rwstring

  letter1 = "_" | "a" .. "z" | "A" .. "Z"
  letterRest = "0" .. "9" | letter1

  comment = "%%" notEol* eol
  notEol = ~eol any
  
  eol = "\\n"
  ws = comment | eol | " " | "\\t" | "," 
  delimiter = &"]" | &"="

  rwstring = stringchar*
  stringchar = ~"\\n" any

  rwstringWithNewlines = nlstringchar*
  nlstringchar = ~"]]" ~"}}" any
  code = "{{" ws* codeString "}}" ws* 
  codeString = rwstringWithNewlines

}
`;


function ohm_parse (grammar, text) {
    var parser = ohm.grammar (grammar);
    var cst = parser.match (text);
    if (cst.succeeded ()) {
	return { parser: parser, cst: cst };
    } else {
	// process.stderr.write (cst.message + '\n');
	// process.exit (1);
	console.log (parser.trace (text).toString ());
	throw "grammar matching failed";
    }
}

var varNameStack = [];

function addSemantics (sem) {
    sem.addOperation ('_glue', {
	
	semantics: function (_1s, _2s) { 
	    var __1s = _1s._glue ().join (''); 
	    var __2s = _2s._glue ().join (''); 
	    return `
function addSemantics (sem) { 
  sem.addOperation (
'_glue', 
{
${__2s}
_terminal: function () { return this.primitiveValue; }
}); 
}`; 
	},
	semanticsStatement: function (_1, _2s, _3, _4s, _5, _6, _7s, _8, _9s, _10s, _11, _12s) {
	    varNameStack = [];
	    var __1 = _1._glue ();
	    var __2s = _2s._glue ().join ('');
	    var __3 = _3._glue ();
	    var __4s = _4s._glue ().join ('');
	    var __5 = _5._glue ();
	    var __6 = _6._glue ();
	    var __7s = _7s._glue ().join ('');
	    var __8 = _8._glue ();
	    var __9s = _9s._glue ().join ('');
	    var __10s = _10s._glue ().join ('');
	    var __11 = _11._glue ();
	    var __12s = _12s._glue ().join ('');
	    return `
${__1} : function (${__5}) { 
_ruleEnter ("${__1}");
${__10s}
${varNameStack.join ('\n')}
var _result = \`${__11}\`; 
_ruleExit ("${__1}");
return _result; 
},
            `;
	},
	ruleName: function (_1, _2s) { var __1 = _1._glue (); var __2s = _2s._glue ().join (''); return __1 + __2s; },
	parameters: function (_1s) {  var __1s = _1s._glue ().join (','); return __1s; },
	
	parameter: function (_1) { 
	    var __1 = _1._glue ();
	    return `${__1}`;
	},
	flatparameter: function (_1) { 
	    var __1 = _1._glue (); 
	    varNameStack.push (`var ${__1} = _${__1}._glue ();`);
	    return `_${__1}`;
	},
	fpws: function (_1, _2s) { var __1 = _1._glue (); var __2s = _2s._glue ().join (''); return __1; },
	fpd: function (_1, _2) { var __1 = _1._glue (); var __2 = _2._glue (); return __1; },
	
	treeparameter: function (_1, _2) { 
	    var __1 = _1._glue (); 
	    var __2 = _2._glue (); 
	    varNameStack.push (`var ${__2} = _${__2}._glue ().join ('');`);
	    return `_${__2}`; 
	},
	tflatparameter: function (_1) { 
	    var __1 = _1._glue (); 
	    return `${__1}`;
	},
	tfpws: function (_1, _2s) { var __1 = _1._glue (); var __2s = _2s._glue ().join (''); return __1; },
	tfpd: function (_1, _2) { var __1 = _1._glue (); var __2 = _2._glue (); return __1; },

	pname: function (_1, _2s) { var __1 = _1._glue (); var __2s = _2s._glue ().join (''); return __1 + __2s;},
	rewrites: function (_1) { var __1 = _1._glue (); return __1; },
	rw1: function (_1, _2s, codeQ, _3, _4, _5s) {
	    var code = codeQ._glue ();
	    var __3 = _3._glue ();
	    if (0 === code.length) {
  		return __3;
	    } else {
//		process.stderr.write ('code is NOT empty\n');
  		return `${code}${__3}`;
	    }
	},
	rw2: function (_1) { var __1 = _1._glue (); return __1; },
	letter1: function (_1) { var __1 = _1._glue (); return __1; },
	letterRest: function (_1) { var __1 = _1._glue (); return __1; },

	ws: function (_1) { var __1 = _1._glue (); return __1; },
	delimiter: function (_1) { return ""; },

	rwstring: function (_1s) { var __1s = _1s._glue ().join (''); return __1s; },
	stringchar: function (_1) { var __1 = _1._glue (); return __1; },
	rwstringWithNewlines: function (_1s) { var __1s = _1s._glue ().join (''); return __1s; },
	nlstringchar: function (_1) { var __1 = _1._glue (); return __1; },

	code: function (_1, _2s, _3, _4, _5s) { return _3._glue (); },
	codeString: function (_1) { return _1._glue (); },

	_terminal: function () { return this.primitiveValue; }
    });
}


var glueSemantics = {	
    semantics: function (_1s, _2s) { 
	var __1s = _1s._glue ().join (''); 
	var __2s = _2s._glue ().join (''); 
	return `
{
${__2s}
_terminal: function () { return this.primitiveValue; }
}`; 
    },
    semanticsStatement: function (_1, _2s, _3, _4s, _5, _6, _7s, _8, _9s, _10s, _11, _12s) {
	varNameStack = [];
	var __1 = _1._glue ();
	var __2s = _2s._glue ().join ('');
	var __3 = _3._glue ();
	var __4s = _4s._glue ().join ('');
	var __5 = _5._glue ();
	var __6 = _6._glue ();
	var __7s = _7s._glue ().join ('');
	var __8 = _8._glue ();
	var __9s = _9s._glue ().join ('');
	var __10s = _10s._glue ().join ('');
	var __11 = _11._glue ();
	var __12s = _12s._glue ().join ('');
	return `
${__1} : function (${__5}) { 
_ruleEnter ("${__1}");
${__10s}
${varNameStack.join ('\n')}
var _result = \`${__11}\`; 
_ruleExit ("${__1}");
return _result; 
},
            `;
    },
    ruleName: function (_1, _2s) { var __1 = _1._glue (); var __2s = _2s._glue ().join (''); return __1 + __2s; },
    parameters: function (_1s) {  var __1s = _1s._glue ().join (','); return __1s; },
    
    parameter: function (_1) { 
	var __1 = _1._glue ();
	return `${__1}`;
    },
    flatparameter: function (_1) { 
	var __1 = _1._glue (); 
	varNameStack.push (`var ${__1} = _${__1}._glue ();`);
	return `_${__1}`;
    },
    fpws: function (_1, _2s) { var __1 = _1._glue (); var __2s = _2s._glue ().join (''); return __1; },
    fpd: function (_1, _2) { var __1 = _1._glue (); var __2 = _2._glue (); return __1; },
    
    treeparameter: function (_1, _2) { 
	var __1 = _1._glue (); 
	var __2 = _2._glue (); 
	varNameStack.push (`var ${__2} = _${__2}._glue ().join ('');`);
	return `_${__2}`; 
    },
    tflatparameter: function (_1) { 
	var __1 = _1._glue (); 
	return `${__1}`;
    },
    tfpws: function (_1, _2s) { var __1 = _1._glue (); var __2s = _2s._glue ().join (''); return __1; },
    tfpd: function (_1, _2) { var __1 = _1._glue (); var __2 = _2._glue (); return __1; },

    pname: function (_1, _2s) { var __1 = _1._glue (); var __2s = _2s._glue ().join (''); return __1 + __2s;},
    rewrites: function (_1) { var __1 = _1._glue (); return __1; },
    rw1: function (_1, _2s, codeQ, _3, _4, _5s) {
	var code = codeQ._glue ();
	var __3 = _3._glue ();
	if (0 === code.length) {
  	    return __3;
	} else {
	    //		process.stderr.write ('code is NOT empty\n');
  	    return `${code}${__3}`;
	}
    },
    rw2: function (_1) { var __1 = _1._glue (); return __1; },
    letter1: function (_1) { var __1 = _1._glue (); return __1; },
    letterRest: function (_1) { var __1 = _1._glue (); return __1; },

    ws: function (_1) { var __1 = _1._glue (); return __1; },
    delimiter: function (_1) { return ""; },

    rwstring: function (_1s) { var __1s = _1s._glue ().join (''); return __1s; },
    stringchar: function (_1) { var __1 = _1._glue (); return __1; },
    rwstringWithNewlines: function (_1s) { var __1s = _1s._glue ().join (''); return __1s; },
    nlstringchar: function (_1) { var __1 = _1._glue (); return __1; },

    code: function (_1, _2s, _3, _4, _5s) { return _3._glue (); },
    codeString: function (_1) { return _1._glue (); },

    _terminal: function () { return this.primitiveValue; }
};

function transpiler (scnText, grammar, semOperation, semanticsObject) {
    var { parser, cst } = ohm_parse (grammar, scnText);
    var sem = {};
    try {
	if (cst.succeeded ()) {
	    sem = parser.createSemantics ();
	    sem.addOperation (semOperation, semanticsObject);
	    let result = sem (cst)[semOperation]();
	    return result;
	} else {
	    throw "grammar matching failed";
	}
    }
    catch (err) {
	throw err;
    }
}

exports.transpiler = transpiler;
exports.glueGrammar = glueGrammar;
exports.glueSemantics = glueSemantics;

//var scope = require ('./scope');
'use strict'
var _scope;

function scopeStack () {
    this._stack = [];
    this.pushNew = function () {this._stack.push ([])};
    this.pop = function () {this._stack.pop ()};
    this._topIndex = function () {return this._stack.length - 1;};
    this._top = function () { return this._stack[this._topIndex ()]; };
    this.scopeAdd = function (key, val) {
	this._top ().push ({key: key, val: val});
    };
    this._lookup = function (key, a) { 
	return a.find (obj => {return obj && obj.key && (obj.key === key)}); };
    this.scopeGet = function (key) {
	var i = this._topIndex ();
	for (; i >= 0 ; i -= 1) {
	    var obj = this._lookup (key, this._stack [i]);
	    if (obj) {
		return obj.val;
	    };
	};
        console.log ('*** scopeGet error ' + key + ' ***');
	console.log (this._stack);
	console.log (key);
	throw "scopeGet internal error - can't find /" + key + "/";
    };
    this.scopeModify = function (key, val) {
	var i = this._topIndex ();
	for (; i >= 0 ; i -= 1) {
	    var obj = this._lookup (key, this._stack [i]);
	    if (obj) {
              obj.val = val;
              return val;
	    };
	};
        console.log ('*** scopeModify error ' + key + ' ***');
	console.log (this._stack);
	console.log (key);
	throw "scopeModify internal error " + key;
    };
}

function scopeAdd (key, val) {
  return _scope.scopeAdd (key, val);
}

function scopeModify (key, val) {
  return _scope.scopeModify (key, val);
}

function scopeGet (key) {
  return _scope.scopeGet (key);
}

function _ruleInit () {
    _scope = new scopeStack ();
}

function _ruleEnter (ruleName) {
    _scope.pushNew ();
}

function _ruleExit (ruleName) {
    _scope.pop ();
}



//require ('./support');
var atob = require ('atob'); // npm install atob
var pako = require ('pako'); // npm install pako
function decodeMxDiagram (encoded) {
    var data = atob (encoded);
    var inf = pako.inflateRaw (
	Uint8Array.from (data, c=>c.charCodeAt (0)), {to: 'string'})
    var str = decodeURIComponent (inf);
    return str;
}

function expandStyle (s) {
    var sx = s
	.replace(/"/g,'')
	.replace(/ellipse;/g,'kind=ellipse;')
	.replace(/text;/g,'kind=text;')
	.replace (/([^=]+)=([^;]+);/g, '$1="$2" ');
    return sx;
}
function strMangle (s) {
        // remove HTML junk added by drawio
    var ret = s
	.replace (/&[^ ]+;/g, '\n')
	.replace (/\\\\/g, '');

    return ret
        // convert names to be acceptable to SWIPL
	.replace (/-/g, '__')
	.replace (/\\/g, '\\\\')

	.replace (/ __g /g, ' -g ')
	.replace (/__q/g, '-q');
}


var nameIndexTable = [];
var counter = 1;

function resetNames () {
    nameIndexTable = [];
    counter = 1;
}

/// generic
function newID (name, quoteds) {
    var s = stripQuotes (quoteds);
    scopeModify (name, s);
    nameIndexTable[s] = counter;
    counter += 1;
    return '';
}

function pushID (name, s) {
    scopeModify (name, stripQuotes (s));
    return '';
}

function getID (name) {
    var s = scopeGet (name);
    return refID (s);
}


/// cells
function newCellID (s) {
    return newID ('cellid', s);
}

function pushCellID (s) {
    return pushID ('cellid', s);
}

function getCellID () {
    return getID ('cellid');
}

function refCellID (s) {
    return refID (s);
}

/// diagrams
function newDiagramID (s) {
    return newID ('diagramid', s);
}

function pushDiagramID (s) {
    return pushID ('diagramid', s);
}

function getDiagramID () {
    return getID ('diagramid');
}



// //function refIDFat (s) {
// function refID (s) {
//     return "id_" + reallyStripQuotes(s);
// }

// function refIDTiny (s) {
//     return "id" + s.replace(/"/g,"");
// }

// function refIDBoth (s) {
function refID (s) {
    // use refIDTiny to produce smaller ID's (useful for debugging workbench)
    var n = nameIndexTable[s];
    if (n) {
	return "id" + n.toString();
    } else {
	return s;
    }
}    


function stripQuotes (s) {
    return s;
    //return reallyStringQuotes(s);
}

function reallyStripQuotes (s) {
    return s.replace (/"/g,"");
}


function setDiagram () {
    var diagramID = getID ();
    scopeAdd ('diagram', diagramID);
}


//////// details transpiler //////////

function namify (s) {
    return s
	.trim ()
	.replace (/"/g,'')
	.replace (/ /g,'__');
}

function stripQuotes (s) {
    return s
	.replace (/"/g,'');
}

function stripQuotesAddNewlines (s) {
    var str = stripQuotes (s);
    if (s === '') {
	return s;
    } else {
	return '\n' + stripQuotes (s) + '\n';
    }
}

var fs = require ('fs');

const sequenceGrammar = fs.readFileSync ('emitfunctions.ohm', 'utf-8');
const sequenceGlue = fs.readFileSync ('emitfunctions.glue', 'utf-8');
const jsonfactbase = fs.readFileSync ('details.json', 'utf-8');

function execTranspiler (grammar, semantics, source) {
    // first pass - transpile glue code to javascript
    let generatedSCNSemantics = transpiler (semantics, glueGrammar, "_glue", glueSemantics);
    
    _ruleInit(); // part of support.js
    try {
        let semObject = eval('(' + generatedSCNSemantics + ')');
        let tr = transpiler(source, grammar, "_glue", semObject);
	return tr;
    }
    catch (err) {
	throw err;
    }
}


function generatePipeline () {
    var functions = execTranspiler (sequenceGrammar, sequenceGlue, jsonfactbase)
	.replace (/~~/g,'\n');
    console.log (functions);
}
generatePipeline ();
