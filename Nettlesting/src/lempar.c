<!DOCTYPE html>
<html>
<head>
<base href="https://sqlite.org/src/doc/tip/tool/lempar.c" />
<meta charset="UTF-8">
<meta http-equiv="Content-Security-Policy" content="default-src 'self' data:; script-src 'self' 'nonce-960a969e91c34582ca05e3464ab655d905b3999ae17dc2b1'; style-src 'self' 'unsafe-inline'; img-src * data:" />
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>SQLite: lempar.c</title>
<link rel="alternate" type="application/rss+xml" title="RSS Feed"  href="/src/timeline.rss" />
<link rel="stylesheet" href="/src/style.css?id=a13b36cb" type="text/css" />
</head>
<body class="artifact">
<div class="header">
<div class="title">
<img id='sqlitelogo' src='/src/logo' style='height:50px;'> 
<span style='vertical-align:super;'><span id='titlesep'>/</span> lempar.c</span>
</div>
<div class="status"><a href='/src/login'>Login</a>
</div>
</div>
<div class="mainmenu">
<a id='hbbtn' href='/src/sitemap' aria-label='Site Map'>&#9776;</a><a href='https://sqlite.org/' class=''>Home</a>
<a href='/src/timeline' class=''>Timeline</a>
<a href='/src/dir?ci=trunk' class='desktoponly'>Files</a>
<a href='https://sqlite.org/forum/forum' class='desktoponly'>Forum</a>
</div>
<div id='hbdrop'></div>
<form id='f01' method='GET' action='/src/file'>
<input type='hidden' name='udc' value='1'>
<div class="submenu">
<a class="label" href="/src/annotate?filename=tool/lempar.c&amp;checkin=tip">Annotate</a>
<a class="label" href="/src/artifact/757e4ec6">Artifact</a>
<a class="label" href="/src/blame?filename=tool/lempar.c&amp;checkin=tip">Blame</a>
<a class="label" href="/src/timeline?uf=757e4ec699b99e0b7e78ffd8ec2063d9fa198b9102d2009dbe08369dc5775dcb">Check-ins Using</a>
<a class="label" href="/src/raw/757e4ec699b99e0b7e78ffd8ec2063d9fa198b9102d2009dbe08369dc5775dcb?at=lempar.c">Download</a>
<a class="label" href="/src/hexdump?name=757e4ec699b99e0b7e78ffd8ec2063d9fa198b9102d2009dbe08369dc5775dcb">Hex</a>
<label class='submenuctrl submenuckbox'><input type='checkbox' name='ln' id='submenuctrl-0' >Line Numbers</label>
</div>
<input type="hidden" name="name" value="tool/lempar.c">
</form>
<div class="content"><span id="debugMsg"></span>
<h2>File <a data-href='/src/finfo?name=tool/lempar.c&m&ci=tip' href='/src/honeypot'>tool/lempar.c</a>
from the <a data-href='/src/info/tip' href='/src/honeypot'>latest check-in</a></h2>
<hr />
<blockquote class="file-content">
<pre>
/*
** 2000-05-29
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
*************************************************************************
** Driver template for the LEMON parser generator.
**
** The &quot;lemon&quot; program processes an LALR(1) input grammar file, then uses
** this template to construct a parser.  The &quot;lemon&quot; program inserts text
** at each &quot;%%&quot; line.  Also, any &quot;P-a-r-s-e&quot; identifer prefix (without the
** interstitial &quot;-&quot; characters) contained in this template is changed into
** the value of the %name directive from the grammar.  Otherwise, the content
** of this template is copied straight through into the generate parser
** source file.
**
** The following is the concatenation of all %include directives from the
** input grammar file:
*/
/************ Begin %include sections from the grammar ************************/
%%
/**************** End of %include directives **********************************/
/* These constants specify the various numeric values for terminal symbols.
***************** Begin token definitions *************************************/
%%
/**************** End token definitions ***************************************/

/* The next sections is a series of control #defines.
** various aspects of the generated parser.
**    YYCODETYPE         is the data type used to store the integer codes
**                       that represent terminal and non-terminal symbols.
**                       &quot;unsigned char&quot; is used if there are fewer than
**                       256 symbols.  Larger types otherwise.
**    YYNOCODE           is a number of type YYCODETYPE that is not used for
**                       any terminal or nonterminal symbol.
**    YYFALLBACK         If defined, this indicates that one or more tokens
**                       (also known as: &quot;terminal symbols&quot;) have fall-back
**                       values which should be used if the original symbol
**                       would not parse.  This permits keywords to sometimes
**                       be used as identifiers, for example.
**    YYACTIONTYPE       is the data type used for &quot;action codes&quot; - numbers
**                       that indicate what to do in response to the next
**                       token.
**    ParseTOKENTYPE     is the data type used for minor type for terminal
**                       symbols.  Background: A &quot;minor type&quot; is a semantic
**                       value associated with a terminal or non-terminal
**                       symbols.  For example, for an &quot;ID&quot; terminal symbol,
**                       the minor type might be the name of the identifier.
**                       Each non-terminal can have a different minor type.
**                       Terminal symbols all have the same minor type, though.
**                       This macros defines the minor type for terminal 
**                       symbols.
**    YYMINORTYPE        is the data type used for all minor types.
**                       This is typically a union of many types, one of
**                       which is ParseTOKENTYPE.  The entry in the union
**                       for terminal symbols is called &quot;yy0&quot;.
**    YYSTACKDEPTH       is the maximum depth of the parser&#39;s stack.  If
**                       zero the stack is dynamically sized using realloc()
**    ParseARG_SDECL     A static variable declaration for the %extra_argument
**    ParseARG_PDECL     A parameter declaration for the %extra_argument
**    ParseARG_PARAM     Code to pass %extra_argument as a subroutine parameter
**    ParseARG_STORE     Code to store %extra_argument into yypParser
**    ParseARG_FETCH     Code to extract %extra_argument from yypParser
**    ParseCTX_*         As ParseARG_ except for %extra_context
**    YYERRORSYMBOL      is the code number of the error symbol.  If not
**                       defined, then do no error processing.
**    YYNSTATE           the combined number of states.
**    YYNRULE            the number of rules in the grammar
**    YYNTOKEN           Number of terminal symbols
**    YY_MAX_SHIFT       Maximum value for shift actions
**    YY_MIN_SHIFTREDUCE Minimum value for shift-reduce actions
**    YY_MAX_SHIFTREDUCE Maximum value for shift-reduce actions
**    YY_ERROR_ACTION    The yy_action[] code for syntax error
**    YY_ACCEPT_ACTION   The yy_action[] code for accept
**    YY_NO_ACTION       The yy_action[] code for no-op
**    YY_MIN_REDUCE      Minimum value for reduce actions
**    YY_MAX_REDUCE      Maximum value for reduce actions
*/
#ifndef INTERFACE
# define INTERFACE 1
#endif
/************* Begin control #defines *****************************************/
%%
/************* End control #defines *******************************************/
#define YY_NLOOKAHEAD ((int)(sizeof(yy_lookahead)/sizeof(yy_lookahead[0])))

/* Define the yytestcase() macro to be a no-op if is not already defined
** otherwise.
**
** Applications can choose to define yytestcase() in the %include section
** to a macro that can assist in verifying code coverage.  For production
** code the yytestcase() macro should be turned off.  But it is useful
** for testing.
*/
#ifndef yytestcase
# define yytestcase(X)
#endif


/* Next are the tables used to determine what action to take based on the
** current state and lookahead token.  These tables are used to implement
** functions that take a state number and lookahead value and return an
** action integer.  
**
** Suppose the action integer is N.  Then the action is determined as
** follows
**
**   0 &lt;= N &lt;= YY_MAX_SHIFT             Shift N.  That is, push the lookahead
**                                      token onto the stack and goto state N.
**
**   N between YY_MIN_SHIFTREDUCE       Shift to an arbitrary state then
**     and YY_MAX_SHIFTREDUCE           reduce by rule N-YY_MIN_SHIFTREDUCE.
**
**   N == YY_ERROR_ACTION               A syntax error has occurred.
**
**   N == YY_ACCEPT_ACTION              The parser accepts its input.
**
**   N == YY_NO_ACTION                  No such action.  Denotes unused
**                                      slots in the yy_action[] table.
**
**   N between YY_MIN_REDUCE            Reduce by rule N-YY_MIN_REDUCE
**     and YY_MAX_REDUCE
**
** The action table is constructed as a single large table named yy_action[].
** Given state S and lookahead X, the action is computed as either:
**
**    (A)   N = yy_action[ yy_shift_ofst[S] + X ]
**    (B)   N = yy_default[S]
**
** The (A) formula is preferred.  The B formula is used instead if
** yy_lookahead[yy_shift_ofst[S]+X] is not equal to X.
**
** The formulas above are for computing the action when the lookahead is
** a terminal symbol.  If the lookahead is a non-terminal (as occurs after
** a reduce action) then the yy_reduce_ofst[] array is used in place of
** the yy_shift_ofst[] array.
**
** The following are the tables generated in this section:
**
**  yy_action[]        A single table containing all actions.
**  yy_lookahead[]     A table containing the lookahead for each entry in
**                     yy_action.  Used to detect hash collisions.
**  yy_shift_ofst[]    For each state, the offset into yy_action for
**                     shifting terminals.
**  yy_reduce_ofst[]   For each state, the offset into yy_action for
**                     shifting non-terminals after a reduce.
**  yy_default[]       Default action for each state.
**
*********** Begin parsing tables **********************************************/
%%
/********** End of lemon-generated parsing tables *****************************/

/* The next table maps tokens (terminal symbols) into fallback tokens.  
** If a construct like the following:
** 
**      %fallback ID X Y Z.
**
** appears in the grammar, then ID becomes a fallback token for X, Y,
** and Z.  Whenever one of the tokens X, Y, or Z is input to the parser
** but it does not parse, the type of the token is changed to ID and
** the parse is retried before an error is thrown.
**
** This feature can be used, for example, to cause some keywords in a language
** to revert to identifiers if they keyword does not apply in the context where
** it appears.
*/
#ifdef YYFALLBACK
static const YYCODETYPE yyFallback[] = {
%%
};
#endif /* YYFALLBACK */

/* The following structure represents a single element of the
** parser&#39;s stack.  Information stored includes:
**
**   +  The state number for the parser at this level of the stack.
**
**   +  The value of the token stored at this level of the stack.
**      (In other words, the &quot;major&quot; token.)
**
**   +  The semantic value stored at this level of the stack.  This is
**      the information used by the action routines in the grammar.
**      It is sometimes called the &quot;minor&quot; token.
**
** After the &quot;shift&quot; half of a SHIFTREDUCE action, the stateno field
** actually contains the reduce action for the second half of the
** SHIFTREDUCE.
*/
struct yyStackEntry {
  YYACTIONTYPE stateno;  /* The state-number, or reduce action in SHIFTREDUCE */
  YYCODETYPE major;      /* The major token value.  This is the code
                         ** number for the token at this stack level */
  YYMINORTYPE minor;     /* The user-supplied minor token value.  This
                         ** is the value of the token  */
};
typedef struct yyStackEntry yyStackEntry;

/* The state of the parser is completely contained in an instance of
** the following structure */
struct yyParser {
  yyStackEntry *yytos;          /* Pointer to top element of the stack */
#ifdef YYTRACKMAXSTACKDEPTH
  int yyhwm;                    /* High-water mark of the stack */
#endif
#ifndef YYNOERRORRECOVERY
  int yyerrcnt;                 /* Shifts left before out of the error */
#endif
  ParseARG_SDECL                /* A place to hold %extra_argument */
  ParseCTX_SDECL                /* A place to hold %extra_context */
#if YYSTACKDEPTH&lt;=0
  int yystksz;                  /* Current side of the stack */
  yyStackEntry *yystack;        /* The parser&#39;s stack */
  yyStackEntry yystk0;          /* First stack entry */
#else
  yyStackEntry yystack[YYSTACKDEPTH];  /* The parser&#39;s stack */
  yyStackEntry *yystackEnd;            /* Last entry in the stack */
#endif
};
typedef struct yyParser yyParser;

#ifndef NDEBUG
#include &lt;stdio.h&gt;
#include &lt;assert.h&gt;
static FILE *yyTraceFILE = 0;
static char *yyTracePrompt = 0;
#endif /* NDEBUG */

#ifndef NDEBUG
/* 
** Turn parser tracing on by giving a stream to which to write the trace
** and a prompt to preface each trace message.  Tracing is turned off
** by making either argument NULL 
**
** Inputs:
** &lt;ul&gt;
** &lt;li&gt; A FILE* to which trace output should be written.
**      If NULL, then tracing is turned off.
** &lt;li&gt; A prefix string written at the beginning of every
**      line of trace output.  If NULL, then tracing is
**      turned off.
** &lt;/ul&gt;
**
** Outputs:
** None.
*/
void ParseTrace(FILE *TraceFILE, char *zTracePrompt){
  yyTraceFILE = TraceFILE;
  yyTracePrompt = zTracePrompt;
  if( yyTraceFILE==0 ) yyTracePrompt = 0;
  else if( yyTracePrompt==0 ) yyTraceFILE = 0;
}
#endif /* NDEBUG */

#if defined(YYCOVERAGE) || !defined(NDEBUG)
/* For tracing shifts, the names of all terminals and nonterminals
** are required.  The following table supplies these names */
static const char *const yyTokenName[] = { 
%%
};
#endif /* defined(YYCOVERAGE) || !defined(NDEBUG) */

#ifndef NDEBUG
/* For tracing reduce actions, the names of all rules are required.
*/
static const char *const yyRuleName[] = {
%%
};
#endif /* NDEBUG */


#if YYSTACKDEPTH&lt;=0
/*
** Try to increase the size of the parser stack.  Return the number
** of errors.  Return 0 on success.
*/
static int yyGrowStack(yyParser *p){
  int newSize;
  int idx;
  yyStackEntry *pNew;

  newSize = p-&gt;yystksz*2 + 100;
  idx = p-&gt;yytos ? (int)(p-&gt;yytos - p-&gt;yystack) : 0;
  if( p-&gt;yystack==&amp;p-&gt;yystk0 ){
    pNew = malloc(newSize*sizeof(pNew[0]));
    if( pNew ) pNew[0] = p-&gt;yystk0;
  }else{
    pNew = realloc(p-&gt;yystack, newSize*sizeof(pNew[0]));
  }
  if( pNew ){
    p-&gt;yystack = pNew;
    p-&gt;yytos = &amp;p-&gt;yystack[idx];
#ifndef NDEBUG
    if( yyTraceFILE ){
      fprintf(yyTraceFILE,&quot;%sStack grows from %d to %d entries.\n&quot;,
              yyTracePrompt, p-&gt;yystksz, newSize);
    }
#endif
    p-&gt;yystksz = newSize;
  }
  return pNew==0; 
}
#endif

/* Datatype of the argument to the memory allocated passed as the
** second argument to ParseAlloc() below.  This can be changed by
** putting an appropriate #define in the %include section of the input
** grammar.
*/
#ifndef YYMALLOCARGTYPE
# define YYMALLOCARGTYPE size_t
#endif

/* Initialize a new parser that has already been allocated.
*/
void ParseInit(void *yypRawParser ParseCTX_PDECL){
  yyParser *yypParser = (yyParser*)yypRawParser;
  ParseCTX_STORE
#ifdef YYTRACKMAXSTACKDEPTH
  yypParser-&gt;yyhwm = 0;
#endif
#if YYSTACKDEPTH&lt;=0
  yypParser-&gt;yytos = NULL;
  yypParser-&gt;yystack = NULL;
  yypParser-&gt;yystksz = 0;
  if( yyGrowStack(yypParser) ){
    yypParser-&gt;yystack = &amp;yypParser-&gt;yystk0;
    yypParser-&gt;yystksz = 1;
  }
#endif
#ifndef YYNOERRORRECOVERY
  yypParser-&gt;yyerrcnt = -1;
#endif
  yypParser-&gt;yytos = yypParser-&gt;yystack;
  yypParser-&gt;yystack[0].stateno = 0;
  yypParser-&gt;yystack[0].major = 0;
#if YYSTACKDEPTH&gt;0
  yypParser-&gt;yystackEnd = &amp;yypParser-&gt;yystack[YYSTACKDEPTH-1];
#endif
}

#ifndef Parse_ENGINEALWAYSONSTACK
/* 
** This function allocates a new parser.
** The only argument is a pointer to a function which works like
** malloc.
**
** Inputs:
** A pointer to the function used to allocate memory.
**
** Outputs:
** A pointer to a parser.  This pointer is used in subsequent calls
** to Parse and ParseFree.
*/
void *ParseAlloc(void *(*mallocProc)(YYMALLOCARGTYPE) ParseCTX_PDECL){
  yyParser *yypParser;
  yypParser = (yyParser*)(*mallocProc)( (YYMALLOCARGTYPE)sizeof(yyParser) );
  if( yypParser ){
    ParseCTX_STORE
    ParseInit(yypParser ParseCTX_PARAM);
  }
  return (void*)yypParser;
}
#endif /* Parse_ENGINEALWAYSONSTACK */


/* The following function deletes the &quot;minor type&quot; or semantic value
** associated with a symbol.  The symbol can be either a terminal
** or nonterminal. &quot;yymajor&quot; is the symbol code, and &quot;yypminor&quot; is
** a pointer to the value to be deleted.  The code used to do the 
** deletions is derived from the %destructor and/or %token_destructor
** directives of the input grammar.
*/
static void yy_destructor(
  yyParser *yypParser,    /* The parser */
  YYCODETYPE yymajor,     /* Type code for object to destroy */
  YYMINORTYPE *yypminor   /* The object to be destroyed */
){
  ParseARG_FETCH
  ParseCTX_FETCH
  switch( yymajor ){
    /* Here is inserted the actions which take place when a
    ** terminal or non-terminal is destroyed.  This can happen
    ** when the symbol is popped from the stack during a
    ** reduce or during error processing or when a parser is 
    ** being destroyed before it is finished parsing.
    **
    ** Note: during a reduce, the only symbols destroyed are those
    ** which appear on the RHS of the rule, but which are *not* used
    ** inside the C code.
    */
/********* Begin destructor definitions ***************************************/
%%
/********* End destructor definitions *****************************************/
    default:  break;   /* If no destructor action specified: do nothing */
  }
}

/*
** Pop the parser&#39;s stack once.
**
** If there is a destructor routine associated with the token which
** is popped from the stack, then call it.
*/
static void yy_pop_parser_stack(yyParser *pParser){
  yyStackEntry *yytos;
  assert( pParser-&gt;yytos!=0 );
  assert( pParser-&gt;yytos &gt; pParser-&gt;yystack );
  yytos = pParser-&gt;yytos--;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,&quot;%sPopping %s\n&quot;,
      yyTracePrompt,
      yyTokenName[yytos-&gt;major]);
  }
#endif
  yy_destructor(pParser, yytos-&gt;major, &amp;yytos-&gt;minor);
}

/*
** Clear all secondary memory allocations from the parser
*/
void ParseFinalize(void *p){
  yyParser *pParser = (yyParser*)p;
  while( pParser-&gt;yytos&gt;pParser-&gt;yystack ) yy_pop_parser_stack(pParser);
#if YYSTACKDEPTH&lt;=0
  if( pParser-&gt;yystack!=&amp;pParser-&gt;yystk0 ) free(pParser-&gt;yystack);
#endif
}

#ifndef Parse_ENGINEALWAYSONSTACK
/* 
** Deallocate and destroy a parser.  Destructors are called for
** all stack elements before shutting the parser down.
**
** If the YYPARSEFREENEVERNULL macro exists (for example because it
** is defined in a %include section of the input grammar) then it is
** assumed that the input pointer is never NULL.
*/
void ParseFree(
  void *p,                    /* The parser to be deleted */
  void (*freeProc)(void*)     /* Function used to reclaim memory */
){
#ifndef YYPARSEFREENEVERNULL
  if( p==0 ) return;
#endif
  ParseFinalize(p);
  (*freeProc)(p);
}
#endif /* Parse_ENGINEALWAYSONSTACK */

/*
** Return the peak depth of the stack for a parser.
*/
#ifdef YYTRACKMAXSTACKDEPTH
int ParseStackPeak(void *p){
  yyParser *pParser = (yyParser*)p;
  return pParser-&gt;yyhwm;
}
#endif

/* This array of booleans keeps track of the parser statement
** coverage.  The element yycoverage[X][Y] is set when the parser
** is in state X and has a lookahead token Y.  In a well-tested
** systems, every element of this matrix should end up being set.
*/
#if defined(YYCOVERAGE)
static unsigned char yycoverage[YYNSTATE][YYNTOKEN];
#endif

/*
** Write into out a description of every state/lookahead combination that
**
**   (1)  has not been used by the parser, and
**   (2)  is not a syntax error.
**
** Return the number of missed state/lookahead combinations.
*/
#if defined(YYCOVERAGE)
int ParseCoverage(FILE *out){
  int stateno, iLookAhead, i;
  int nMissed = 0;
  for(stateno=0; stateno&lt;YYNSTATE; stateno++){
    i = yy_shift_ofst[stateno];
    for(iLookAhead=0; iLookAhead&lt;YYNTOKEN; iLookAhead++){
      if( yy_lookahead[i+iLookAhead]!=iLookAhead ) continue;
      if( yycoverage[stateno][iLookAhead]==0 ) nMissed++;
      if( out ){
        fprintf(out,&quot;State %d lookahead %s %s\n&quot;, stateno,
                yyTokenName[iLookAhead],
                yycoverage[stateno][iLookAhead] ? &quot;ok&quot; : &quot;missed&quot;);
      }
    }
  }
  return nMissed;
}
#endif

/*
** Find the appropriate action for a parser given the terminal
** look-ahead token iLookAhead.
*/
static YYACTIONTYPE yy_find_shift_action(
  YYCODETYPE iLookAhead,    /* The look-ahead token */
  YYACTIONTYPE stateno      /* Current state number */
){
  int i;

  if( stateno&gt;YY_MAX_SHIFT ) return stateno;
  assert( stateno &lt;= YY_SHIFT_COUNT );
#if defined(YYCOVERAGE)
  yycoverage[stateno][iLookAhead] = 1;
#endif
  do{
    i = yy_shift_ofst[stateno];
    assert( i&gt;=0 );
    assert( i&lt;=YY_ACTTAB_COUNT );
    assert( i+YYNTOKEN&lt;=(int)YY_NLOOKAHEAD );
    assert( iLookAhead!=YYNOCODE );
    assert( iLookAhead &lt; YYNTOKEN );
    i += iLookAhead;
    assert( i&lt;(int)YY_NLOOKAHEAD );
    if( yy_lookahead[i]!=iLookAhead ){
#ifdef YYFALLBACK
      YYCODETYPE iFallback;            /* Fallback token */
      assert( iLookAhead&lt;sizeof(yyFallback)/sizeof(yyFallback[0]) );
      iFallback = yyFallback[iLookAhead];
      if( iFallback!=0 ){
#ifndef NDEBUG
        if( yyTraceFILE ){
          fprintf(yyTraceFILE, &quot;%sFALLBACK %s =&gt; %s\n&quot;,
             yyTracePrompt, yyTokenName[iLookAhead], yyTokenName[iFallback]);
        }
#endif
        assert( yyFallback[iFallback]==0 ); /* Fallback loop must terminate */
        iLookAhead = iFallback;
        continue;
      }
#endif
#ifdef YYWILDCARD
      {
        int j = i - iLookAhead + YYWILDCARD;
        assert( j&lt;(int)(sizeof(yy_lookahead)/sizeof(yy_lookahead[0])) );
        if( yy_lookahead[j]==YYWILDCARD &amp;&amp; iLookAhead&gt;0 ){
#ifndef NDEBUG
          if( yyTraceFILE ){
            fprintf(yyTraceFILE, &quot;%sWILDCARD %s =&gt; %s\n&quot;,
               yyTracePrompt, yyTokenName[iLookAhead],
               yyTokenName[YYWILDCARD]);
          }
#endif /* NDEBUG */
          return yy_action[j];
        }
      }
#endif /* YYWILDCARD */
      return yy_default[stateno];
    }else{
      assert( i&gt;=0 &amp;&amp; i&lt;(int)(sizeof(yy_action)/sizeof(yy_action[0])) );
      return yy_action[i];
    }
  }while(1);
}

/*
** Find the appropriate action for a parser given the non-terminal
** look-ahead token iLookAhead.
*/
static YYACTIONTYPE yy_find_reduce_action(
  YYACTIONTYPE stateno,     /* Current state number */
  YYCODETYPE iLookAhead     /* The look-ahead token */
){
  int i;
#ifdef YYERRORSYMBOL
  if( stateno&gt;YY_REDUCE_COUNT ){
    return yy_default[stateno];
  }
#else
  assert( stateno&lt;=YY_REDUCE_COUNT );
#endif
  i = yy_reduce_ofst[stateno];
  assert( iLookAhead!=YYNOCODE );
  i += iLookAhead;
#ifdef YYERRORSYMBOL
  if( i&lt;0 || i&gt;=YY_ACTTAB_COUNT || yy_lookahead[i]!=iLookAhead ){
    return yy_default[stateno];
  }
#else
  assert( i&gt;=0 &amp;&amp; i&lt;YY_ACTTAB_COUNT );
  assert( yy_lookahead[i]==iLookAhead );
#endif
  return yy_action[i];
}

/*
** The following routine is called if the stack overflows.
*/
static void yyStackOverflow(yyParser *yypParser){
   ParseARG_FETCH
   ParseCTX_FETCH
#ifndef NDEBUG
   if( yyTraceFILE ){
     fprintf(yyTraceFILE,&quot;%sStack Overflow!\n&quot;,yyTracePrompt);
   }
#endif
   while( yypParser-&gt;yytos&gt;yypParser-&gt;yystack ) yy_pop_parser_stack(yypParser);
   /* Here code is inserted which will execute if the parser
   ** stack every overflows */
/******** Begin %stack_overflow code ******************************************/
%%
/******** End %stack_overflow code ********************************************/
   ParseARG_STORE /* Suppress warning about unused %extra_argument var */
   ParseCTX_STORE
}

/*
** Print tracing information for a SHIFT action
*/
#ifndef NDEBUG
static void yyTraceShift(yyParser *yypParser, int yyNewState, const char *zTag){
  if( yyTraceFILE ){
    if( yyNewState&lt;YYNSTATE ){
      fprintf(yyTraceFILE,&quot;%s%s &#39;%s&#39;, go to state %d\n&quot;,
         yyTracePrompt, zTag, yyTokenName[yypParser-&gt;yytos-&gt;major],
         yyNewState);
    }else{
      fprintf(yyTraceFILE,&quot;%s%s &#39;%s&#39;, pending reduce %d\n&quot;,
         yyTracePrompt, zTag, yyTokenName[yypParser-&gt;yytos-&gt;major],
         yyNewState - YY_MIN_REDUCE);
    }
  }
}
#else
# define yyTraceShift(X,Y,Z)
#endif

/*
** Perform a shift action.
*/
static void yy_shift(
  yyParser *yypParser,          /* The parser to be shifted */
  YYACTIONTYPE yyNewState,      /* The new state to shift in */
  YYCODETYPE yyMajor,           /* The major token to shift in */
  ParseTOKENTYPE yyMinor        /* The minor token to shift in */
){
  yyStackEntry *yytos;
  yypParser-&gt;yytos++;
#ifdef YYTRACKMAXSTACKDEPTH
  if( (int)(yypParser-&gt;yytos - yypParser-&gt;yystack)&gt;yypParser-&gt;yyhwm ){
    yypParser-&gt;yyhwm++;
    assert( yypParser-&gt;yyhwm == (int)(yypParser-&gt;yytos - yypParser-&gt;yystack) );
  }
#endif
#if YYSTACKDEPTH&gt;0 
  if( yypParser-&gt;yytos&gt;yypParser-&gt;yystackEnd ){
    yypParser-&gt;yytos--;
    yyStackOverflow(yypParser);
    return;
  }
#else
  if( yypParser-&gt;yytos&gt;=&amp;yypParser-&gt;yystack[yypParser-&gt;yystksz] ){
    if( yyGrowStack(yypParser) ){
      yypParser-&gt;yytos--;
      yyStackOverflow(yypParser);
      return;
    }
  }
#endif
  if( yyNewState &gt; YY_MAX_SHIFT ){
    yyNewState += YY_MIN_REDUCE - YY_MIN_SHIFTREDUCE;
  }
  yytos = yypParser-&gt;yytos;
  yytos-&gt;stateno = yyNewState;
  yytos-&gt;major = yyMajor;
  yytos-&gt;minor.yy0 = yyMinor;
  yyTraceShift(yypParser, yyNewState, &quot;Shift&quot;);
}

/* For rule J, yyRuleInfoLhs[J] contains the symbol on the left-hand side
** of that rule */
static const YYCODETYPE yyRuleInfoLhs[] = {
%%
};

/* For rule J, yyRuleInfoNRhs[J] contains the negative of the number
** of symbols on the right-hand side of that rule. */
static const signed char yyRuleInfoNRhs[] = {
%%
};

static void yy_accept(yyParser*);  /* Forward Declaration */

/*
** Perform a reduce action and the shift that must immediately
** follow the reduce.
**
** The yyLookahead and yyLookaheadToken parameters provide reduce actions
** access to the lookahead token (if any).  The yyLookahead will be YYNOCODE
** if the lookahead token has already been consumed.  As this procedure is
** only called from one place, optimizing compilers will in-line it, which
** means that the extra parameters have no performance impact.
*/
static YYACTIONTYPE yy_reduce(
  yyParser *yypParser,         /* The parser */
  unsigned int yyruleno,       /* Number of the rule by which to reduce */
  int yyLookahead,             /* Lookahead token, or YYNOCODE if none */
  ParseTOKENTYPE yyLookaheadToken  /* Value of the lookahead token */
  ParseCTX_PDECL                   /* %extra_context */
){
  int yygoto;                     /* The next state */
  YYACTIONTYPE yyact;             /* The next action */
  yyStackEntry *yymsp;            /* The top of the parser&#39;s stack */
  int yysize;                     /* Amount to pop the stack */
  ParseARG_FETCH
  (void)yyLookahead;
  (void)yyLookaheadToken;
  yymsp = yypParser-&gt;yytos;

  switch( yyruleno ){
  /* Beginning here are the reduction cases.  A typical example
  ** follows:
  **   case 0:
  **  #line &lt;lineno&gt; &lt;grammarfile&gt;
  **     { ... }           // User supplied code
  **  #line &lt;lineno&gt; &lt;thisfile&gt;
  **     break;
  */
/********** Begin reduce actions **********************************************/
%%
/********** End reduce actions ************************************************/
  };
  assert( yyruleno&lt;sizeof(yyRuleInfoLhs)/sizeof(yyRuleInfoLhs[0]) );
  yygoto = yyRuleInfoLhs[yyruleno];
  yysize = yyRuleInfoNRhs[yyruleno];
  yyact = yy_find_reduce_action(yymsp[yysize].stateno,(YYCODETYPE)yygoto);

  /* There are no SHIFTREDUCE actions on nonterminals because the table
  ** generator has simplified them to pure REDUCE actions. */
  assert( !(yyact&gt;YY_MAX_SHIFT &amp;&amp; yyact&lt;=YY_MAX_SHIFTREDUCE) );

  /* It is not possible for a REDUCE to be followed by an error */
  assert( yyact!=YY_ERROR_ACTION );

  yymsp += yysize+1;
  yypParser-&gt;yytos = yymsp;
  yymsp-&gt;stateno = (YYACTIONTYPE)yyact;
  yymsp-&gt;major = (YYCODETYPE)yygoto;
  yyTraceShift(yypParser, yyact, &quot;... then shift&quot;);
  return yyact;
}

/*
** The following code executes when the parse fails
*/
#ifndef YYNOERRORRECOVERY
static void yy_parse_failed(
  yyParser *yypParser           /* The parser */
){
  ParseARG_FETCH
  ParseCTX_FETCH
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,&quot;%sFail!\n&quot;,yyTracePrompt);
  }
#endif
  while( yypParser-&gt;yytos&gt;yypParser-&gt;yystack ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser fails */
/************ Begin %parse_failure code ***************************************/
%%
/************ End %parse_failure code *****************************************/
  ParseARG_STORE /* Suppress warning about unused %extra_argument variable */
  ParseCTX_STORE
}
#endif /* YYNOERRORRECOVERY */

/*
** The following code executes when a syntax error first occurs.
*/
static void yy_syntax_error(
  yyParser *yypParser,           /* The parser */
  int yymajor,                   /* The major type of the error token */
  ParseTOKENTYPE yyminor         /* The minor type of the error token */
){
  ParseARG_FETCH
  ParseCTX_FETCH
#define TOKEN yyminor
/************ Begin %syntax_error code ****************************************/
%%
/************ End %syntax_error code ******************************************/
  ParseARG_STORE /* Suppress warning about unused %extra_argument variable */
  ParseCTX_STORE
}

/*
** The following is executed when the parser accepts
*/
static void yy_accept(
  yyParser *yypParser           /* The parser */
){
  ParseARG_FETCH
  ParseCTX_FETCH
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,&quot;%sAccept!\n&quot;,yyTracePrompt);
  }
#endif
#ifndef YYNOERRORRECOVERY
  yypParser-&gt;yyerrcnt = -1;
#endif
  assert( yypParser-&gt;yytos==yypParser-&gt;yystack );
  /* Here code is inserted which will be executed whenever the
  ** parser accepts */
/*********** Begin %parse_accept code *****************************************/
%%
/*********** End %parse_accept code *******************************************/
  ParseARG_STORE /* Suppress warning about unused %extra_argument variable */
  ParseCTX_STORE
}

/* The main parser program.
** The first argument is a pointer to a structure obtained from
** &quot;ParseAlloc&quot; which describes the current state of the parser.
** The second argument is the major token number.  The third is
** the minor token.  The fourth optional argument is whatever the
** user wants (and specified in the grammar) and is available for
** use by the action routines.
**
** Inputs:
** &lt;ul&gt;
** &lt;li&gt; A pointer to the parser (an opaque structure.)
** &lt;li&gt; The major token number.
** &lt;li&gt; The minor token number.
** &lt;li&gt; An option argument of a grammar-specified type.
** &lt;/ul&gt;
**
** Outputs:
** None.
*/
void Parse(
  void *yyp,                   /* The parser */
  int yymajor,                 /* The major token code number */
  ParseTOKENTYPE yyminor       /* The value for the token */
  ParseARG_PDECL               /* Optional %extra_argument parameter */
){
  YYMINORTYPE yyminorunion;
  YYACTIONTYPE yyact;   /* The parser action. */
#if !defined(YYERRORSYMBOL) &amp;&amp; !defined(YYNOERRORRECOVERY)
  int yyendofinput;     /* True if we are at the end of input */
#endif
#ifdef YYERRORSYMBOL
  int yyerrorhit = 0;   /* True if yymajor has invoked an error */
#endif
  yyParser *yypParser = (yyParser*)yyp;  /* The parser */
  ParseCTX_FETCH
  ParseARG_STORE

  assert( yypParser-&gt;yytos!=0 );
#if !defined(YYERRORSYMBOL) &amp;&amp; !defined(YYNOERRORRECOVERY)
  yyendofinput = (yymajor==0);
#endif

  yyact = yypParser-&gt;yytos-&gt;stateno;
#ifndef NDEBUG
  if( yyTraceFILE ){
    if( yyact &lt; YY_MIN_REDUCE ){
      fprintf(yyTraceFILE,&quot;%sInput &#39;%s&#39; in state %d\n&quot;,
              yyTracePrompt,yyTokenName[yymajor],yyact);
    }else{
      fprintf(yyTraceFILE,&quot;%sInput &#39;%s&#39; with pending reduce %d\n&quot;,
              yyTracePrompt,yyTokenName[yymajor],yyact-YY_MIN_REDUCE);
    }
  }
#endif

  while(1){ /* Exit by &quot;break&quot; */
    assert( yypParser-&gt;yytos&gt;=yypParser-&gt;yystack );
    assert( yyact==yypParser-&gt;yytos-&gt;stateno );
    yyact = yy_find_shift_action((YYCODETYPE)yymajor,yyact);
    if( yyact &gt;= YY_MIN_REDUCE ){
      unsigned int yyruleno = yyact - YY_MIN_REDUCE; /* Reduce by this rule */
      assert( yyruleno&lt;(int)(sizeof(yyRuleName)/sizeof(yyRuleName[0])) );
#ifndef NDEBUG
      if( yyTraceFILE ){
        int yysize = yyRuleInfoNRhs[yyruleno];
        if( yysize ){
          fprintf(yyTraceFILE, &quot;%sReduce %d [%s]%s, pop back to state %d.\n&quot;,
            yyTracePrompt,
            yyruleno, yyRuleName[yyruleno],
            yyruleno&lt;YYNRULE_WITH_ACTION ? &quot;&quot; : &quot; without external action&quot;,
            yypParser-&gt;yytos[yysize].stateno);
        }else{
          fprintf(yyTraceFILE, &quot;%sReduce %d [%s]%s.\n&quot;,
            yyTracePrompt, yyruleno, yyRuleName[yyruleno],
            yyruleno&lt;YYNRULE_WITH_ACTION ? &quot;&quot; : &quot; without external action&quot;);
        }
      }
#endif /* NDEBUG */

      /* Check that the stack is large enough to grow by a single entry
      ** if the RHS of the rule is empty.  This ensures that there is room
      ** enough on the stack to push the LHS value */
      if( yyRuleInfoNRhs[yyruleno]==0 ){
#ifdef YYTRACKMAXSTACKDEPTH
        if( (int)(yypParser-&gt;yytos - yypParser-&gt;yystack)&gt;yypParser-&gt;yyhwm ){
          yypParser-&gt;yyhwm++;
          assert( yypParser-&gt;yyhwm ==
                  (int)(yypParser-&gt;yytos - yypParser-&gt;yystack));
        }
#endif
#if YYSTACKDEPTH&gt;0 
        if( yypParser-&gt;yytos&gt;=yypParser-&gt;yystackEnd ){
          yyStackOverflow(yypParser);
          break;
        }
#else
        if( yypParser-&gt;yytos&gt;=&amp;yypParser-&gt;yystack[yypParser-&gt;yystksz-1] ){
          if( yyGrowStack(yypParser) ){
            yyStackOverflow(yypParser);
            break;
          }
        }
#endif
      }
      yyact = yy_reduce(yypParser,yyruleno,yymajor,yyminor ParseCTX_PARAM);
    }else if( yyact &lt;= YY_MAX_SHIFTREDUCE ){
      yy_shift(yypParser,yyact,(YYCODETYPE)yymajor,yyminor);
#ifndef YYNOERRORRECOVERY
      yypParser-&gt;yyerrcnt--;
#endif
      break;
    }else if( yyact==YY_ACCEPT_ACTION ){
      yypParser-&gt;yytos--;
      yy_accept(yypParser);
      return;
    }else{
      assert( yyact == YY_ERROR_ACTION );
      yyminorunion.yy0 = yyminor;
#ifdef YYERRORSYMBOL
      int yymx;
#endif
#ifndef NDEBUG
      if( yyTraceFILE ){
        fprintf(yyTraceFILE,&quot;%sSyntax Error!\n&quot;,yyTracePrompt);
      }
#endif
#ifdef YYERRORSYMBOL
      /* A syntax error has occurred.
      ** The response to an error depends upon whether or not the
      ** grammar defines an error token &quot;ERROR&quot;.  
      **
      ** This is what we do if the grammar does define ERROR:
      **
      **  * Call the %syntax_error function.
      **
      **  * Begin popping the stack until we enter a state where
      **    it is legal to shift the error symbol, then shift
      **    the error symbol.
      **
      **  * Set the error count to three.
      **
      **  * Begin accepting and shifting new tokens.  No new error
      **    processing will occur until three tokens have been
      **    shifted successfully.
      **
      */
      if( yypParser-&gt;yyerrcnt&lt;0 ){
        yy_syntax_error(yypParser,yymajor,yyminor);
      }
      yymx = yypParser-&gt;yytos-&gt;major;
      if( yymx==YYERRORSYMBOL || yyerrorhit ){
#ifndef NDEBUG
        if( yyTraceFILE ){
          fprintf(yyTraceFILE,&quot;%sDiscard input token %s\n&quot;,
             yyTracePrompt,yyTokenName[yymajor]);
        }
#endif
        yy_destructor(yypParser, (YYCODETYPE)yymajor, &amp;yyminorunion);
        yymajor = YYNOCODE;
      }else{
        while( yypParser-&gt;yytos &gt; yypParser-&gt;yystack ){
          yyact = yy_find_reduce_action(yypParser-&gt;yytos-&gt;stateno,
                                        YYERRORSYMBOL);
          if( yyact&lt;=YY_MAX_SHIFTREDUCE ) break;
          if( yyact&gt;=YY_MIN_REDUCE &amp;&amp; yyRuleInfoNRhs[yyact-YY_MIN_REDUCE] ){
            yyact -= YY_MIN_REDUCE - YY_MIN_SHIFTREDUCE;
            break;
          }
          yy_pop_parser_stack(yypParser);
        }
        if( yypParser-&gt;yytos &lt;= yypParser-&gt;yystack || yymajor==0 ){
          yy_destructor(yypParser,(YYCODETYPE)yymajor,&amp;yyminorunion);
          yy_parse_failed(yypParser);
#ifndef YYNOERRORRECOVERY
          yypParser-&gt;yyerrcnt = -1;
#endif
          yymajor = YYNOCODE;
        }else if( yymx!=YYERRORSYMBOL ){
          yy_shift(yypParser,yyact,YYERRORSYMBOL,yyminor);
        }
      }
      yypParser-&gt;yyerrcnt = 3;
      yyerrorhit = 1;
      if( yymajor==YYNOCODE ) break;
      yyact = yypParser-&gt;yytos-&gt;stateno;
#elif defined(YYNOERRORRECOVERY)
      /* If the YYNOERRORRECOVERY macro is defined, then do not attempt to
      ** do any kind of error recovery.  Instead, simply invoke the syntax
      ** error routine and continue going as if nothing had happened.
      **
      ** Applications can set this macro (for example inside %include) if
      ** they intend to abandon the parse upon the first syntax error seen.
      */
      yy_syntax_error(yypParser,yymajor, yyminor);
      yy_destructor(yypParser,(YYCODETYPE)yymajor,&amp;yyminorunion);
      break;
#else  /* YYERRORSYMBOL is not defined */
      /* This is what we do if the grammar does not define ERROR:
      **
      **  * Report an error message, and throw away the input token.
      **
      **  * If the input token is $, then fail the parse.
      **
      ** As before, subsequent error messages are suppressed until
      ** three input tokens have been successfully shifted.
      */
      if( yypParser-&gt;yyerrcnt&lt;=0 ){
        yy_syntax_error(yypParser,yymajor, yyminor);
      }
      yypParser-&gt;yyerrcnt = 3;
      yy_destructor(yypParser,(YYCODETYPE)yymajor,&amp;yyminorunion);
      if( yyendofinput ){
        yy_parse_failed(yypParser);
#ifndef YYNOERRORRECOVERY
        yypParser-&gt;yyerrcnt = -1;
#endif
      }
      break;
#endif
    }
  }
#ifndef NDEBUG
  if( yyTraceFILE ){
    yyStackEntry *i;
    char cDiv = &#39;[&#39;;
    fprintf(yyTraceFILE,&quot;%sReturn. Stack=&quot;,yyTracePrompt);
    for(i=&amp;yypParser-&gt;yystack[1]; i&lt;=yypParser-&gt;yytos; i++){
      fprintf(yyTraceFILE,&quot;%c%s&quot;, cDiv, yyTokenName[i-&gt;major]);
      cDiv = &#39; &#39;;
    }
    fprintf(yyTraceFILE,&quot;]\n&quot;);
  }
#endif
  return;
}

/*
** Return the fallback token corresponding to canonical token iToken, or
** 0 if iToken has no fallback.
*/
int ParseFallback(int iToken){
#ifdef YYFALLBACK
  assert( iToken&lt;(int)(sizeof(yyFallback)/sizeof(yyFallback[0])) );
  return yyFallback[iToken];
#else
  (void)iToken;
  return 0;
#endif
}

</pre>
</blockquote>
</div>
<div class="footer">
This page was generated in about
0.006s by
Fossil 2.17 [edd280c3b6] 2021-08-20 22:41:28
</div>
<script nonce="960a969e91c34582ca05e3464ab655d905b3999ae17dc2b1">
(function() {
var hbButton = document.getElementById("hbbtn");
if (!hbButton) return;
if (!document.addEventListener) {
hbButton.href = "/src/sitemap";
return;
}
var panel = document.getElementById("hbdrop");
if (!panel) return;
if (!panel.style) return;
var panelBorder = panel.style.border;
var panelInitialized = false;
var panelResetBorderTimerID = 0;
var animate = panel.style.transition !== null && (typeof(panel.style.transition) == "string");
var animMS = panel.getAttribute("data-anim-ms");
if (animMS) {
animMS = parseInt(animMS);
if (isNaN(animMS) || animMS == 0)
animate = false;
else if (animMS < 0)
animMS = 400;
}
else
animMS = 400;
var panelHeight;
function calculatePanelHeight() {
panel.style.maxHeight = '';
var es   = window.getComputedStyle(panel),
edis = es.display,
epos = es.position,
evis = es.visibility;
panel.style.visibility = 'hidden';
panel.style.position   = 'absolute';
panel.style.display    = 'block';
panelHeight = panel.offsetHeight + 'px';
panel.style.display    = edis;
panel.style.position   = epos;
panel.style.visibility = evis;
}
function showPanel() {
if (panelResetBorderTimerID) {
clearTimeout(panelResetBorderTimerID);
panelResetBorderTimerID = 0;
}
if (animate) {
if (!panelInitialized) {
panelInitialized = true;
calculatePanelHeight();
panel.style.transition = 'max-height ' + animMS +
'ms ease-in-out';
panel.style.overflowY  = 'hidden';
panel.style.maxHeight  = '0';
}
setTimeout(function() {
panel.style.maxHeight = panelHeight;
panel.style.border    = panelBorder;
}, 40);
}
panel.style.display = 'block';
document.addEventListener('keydown',panelKeydown,true);
document.addEventListener('click',panelClick,false);
}
var panelKeydown = function(event) {
var key = event.which || event.keyCode;
if (key == 27) {
event.stopPropagation();
panelToggle(true);
}
};
var panelClick = function(event) {
if (!panel.contains(event.target)) {
panelToggle(true);
}
};
function panelShowing() {
if (animate) {
return panel.style.maxHeight == panelHeight;
}
else {
return panel.style.display == 'block';
}
}
function hasChildren(element) {
var childElement = element.firstChild;
while (childElement) {
if (childElement.nodeType == 1)
return true;
childElement = childElement.nextSibling;
}
return false;
}
window.addEventListener('resize',function(event) {
panelInitialized = false;
},false);
hbButton.addEventListener('click',function(event) {
event.stopPropagation();
event.preventDefault();
panelToggle(false);
},false);
function panelToggle(suppressAnimation) {
if (panelShowing()) {
document.removeEventListener('keydown',panelKeydown,true);
document.removeEventListener('click',panelClick,false);
if (animate) {
if (suppressAnimation) {
var transition = panel.style.transition;
panel.style.transition = '';
panel.style.maxHeight = '0';
panel.style.border = 'none';
setTimeout(function() {
panel.style.transition = transition;
}, 40);
}
else {
panel.style.maxHeight = '0';
panelResetBorderTimerID = setTimeout(function() {
panel.style.border = 'none';
panelResetBorderTimerID = 0;
}, animMS);
}
}
else {
panel.style.display = 'none';
}
}
else {
if (!hasChildren(panel)) {
var xhr = new XMLHttpRequest();
xhr.onload = function() {
var doc = xhr.responseXML;
if (doc) {
var sm = doc.querySelector("ul#sitemap");
if (sm && xhr.status == 200) {
panel.innerHTML = sm.outerHTML;
showPanel();
}
}
}
xhr.open("GET", "/src/sitemap?popup");
xhr.responseType = "document";
xhr.send();
}
else {
showPanel();
}
}
}
})();

</script>
<script id='href-data' type='application/json'>{"delay":250,"mouseover":1}</script>
<script nonce="960a969e91c34582ca05e3464ab655d905b3999ae17dc2b1">/* style.c:895 */
function debugMsg(msg){
var n = document.getElementById("debugMsg");
if(n){n.textContent=msg;}
}
/* href.js */
function setAllHrefs(){
var anchors = document.getElementsByTagName("a");
for(var i=0; i<anchors.length; i++){
var j = anchors[i];
if(j.hasAttribute("data-href")) j.href=j.getAttribute("data-href");
}
var forms = document.getElementsByTagName("form");
for(var i=0; i<forms.length; i++){
var j = forms[i];
if(j.hasAttribute("data-action")) j.action=j.getAttribute("data-action");
}
}
function antiRobotDefense(){
var x = document.getElementById("href-data");
var jx = x.textContent || x.innerText;
var g = JSON.parse(jx);
var isOperaMini =
Object.prototype.toString.call(window.operamini)==="[object OperaMini]";
if(g.mouseover && !isOperaMini){
document.getElementsByTagName("body")[0].onmousemove=function(){
setTimeout(setAllHrefs, g.delay);
}
}else{
setTimeout(setAllHrefs, g.delay);
}
}
antiRobotDefense();
</script>
<script nonce='960a969e91c34582ca05e3464ab655d905b3999ae17dc2b1'>
/* hbmenu.js *************************************************************/
(function() {
var hbButton = document.getElementById("hbbtn");
if (!hbButton) return;
if (!document.addEventListener) return;
var panel = document.getElementById("hbdrop");
if (!panel) return;
if (!panel.style) return;
var panelBorder = panel.style.border;
var panelInitialized = false;
var panelResetBorderTimerID = 0;
var animate = panel.style.transition !== null && (typeof(panel.style.transition) == "string");
var animMS = panel.getAttribute("data-anim-ms");
if (animMS) {
animMS = parseInt(animMS);
if (isNaN(animMS) || animMS == 0)
animate = false;
else if (animMS < 0)
animMS = 400;
}
else
animMS = 400;
var panelHeight;
function calculatePanelHeight() {
panel.style.maxHeight = '';
var es   = window.getComputedStyle(panel),
edis = es.display,
epos = es.position,
evis = es.visibility;
panel.style.visibility = 'hidden';
panel.style.position   = 'absolute';
panel.style.display    = 'block';
panelHeight = panel.offsetHeight + 'px';
panel.style.display    = edis;
panel.style.position   = epos;
panel.style.visibility = evis;
}
function showPanel() {
if (panelResetBorderTimerID) {
clearTimeout(panelResetBorderTimerID);
panelResetBorderTimerID = 0;
}
if (animate) {
if (!panelInitialized) {
panelInitialized = true;
calculatePanelHeight();
panel.style.transition = 'max-height ' + animMS +
'ms ease-in-out';
panel.style.overflowY  = 'hidden';
panel.style.maxHeight  = '0';
}
setTimeout(function() {
panel.style.maxHeight = panelHeight;
panel.style.border    = panelBorder;
}, 40);
}
panel.style.display = 'block';
document.addEventListener('keydown',panelKeydown,true);
document.addEventListener('click',panelClick,false);
}
var panelKeydown = function(event) {
var key = event.which || event.keyCode;
if (key == 27) {
event.stopPropagation();
panelToggle(true);
}
};
var panelClick = function(event) {
if (!panel.contains(event.target)) {
panelToggle(true);
}
};
function panelShowing() {
if (animate) {
return panel.style.maxHeight == panelHeight;
}
else {
return panel.style.display == 'block';
}
}
function hasChildren(element) {
var childElement = element.firstChild;
while (childElement) {
if (childElement.nodeType == 1)
return true;
childElement = childElement.nextSibling;
}
return false;
}
window.addEventListener('resize',function(event) {
panelInitialized = false;
},false);
hbButton.addEventListener('click',function(event) {
event.stopPropagation();
event.preventDefault();
panelToggle(false);
},false);
function panelToggle(suppressAnimation) {
if (panelShowing()) {
document.removeEventListener('keydown',panelKeydown,true);
document.removeEventListener('click',panelClick,false);
if (animate) {
if (suppressAnimation) {
var transition = panel.style.transition;
panel.style.transition = '';
panel.style.maxHeight = '0';
panel.style.border = 'none';
setTimeout(function() {
panel.style.transition = transition;
}, 40);
}
else {
panel.style.maxHeight = '0';
panelResetBorderTimerID = setTimeout(function() {
panel.style.border = 'none';
panelResetBorderTimerID = 0;
}, animMS);
}
}
else {
panel.style.display = 'none';
}
}
else {
if (!hasChildren(panel)) {
var xhr = new XMLHttpRequest();
xhr.onload = function() {
var doc = xhr.responseXML;
if (doc) {
var sm = doc.querySelector("ul#sitemap");
if (sm && xhr.status == 200) {
panel.innerHTML = sm.outerHTML;
showPanel();
}
}
}
var url = hbButton.href + (hbButton.href.includes("?")?"&popup":"?popup")
xhr.open("GET", url);
xhr.responseType = "document";
xhr.send();
}
else {
showPanel();
}
}
}
})();
/* menu.js *************************************************************/
function toggle_annotation_log(){
var w = document.getElementById("annotation_log");
var x = document.forms["f01"].elements["log"].checked
w.style.display = x ? "block" : "none";
}
function submenu_onchange_submit(){
var w = document.getElementById("f01");
w.submit();
}
(function (){
for(var i=0; 1; i++){
var x = document.getElementById("submenuctrl-"+i);
if(!x) break;
if( !x.hasAttribute('data-ctrl') ){
x.onchange = submenu_onchange_submit;
}else{
var cx = x.getAttribute('data-ctrl');
if( cx=="toggle_annotation_log" ){
x.onchange = toggle_annotation_log;
}
}
}
})();
</script>
</body>
</html>
