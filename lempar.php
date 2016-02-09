<?php # vim:ts=2:sw=2:et:
/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is included that follows the "include" declaration
** in the input grammar file. */
%%
/* Next is all token values, in a form suitable for use by makeheaders.
** This section will be null unless lemon is run with the -m switch.
*/
/* 
** These constants (all generated automatically by the parser generator)
** specify the various kinds of tokens (terminals) that the parser
** understands. 
**
** Each symbol here is a terminal symbol in the grammar.
*/
%%
/* The next thing included is series of defines which control
** various aspects of the generated parser.
**    YYCODETYPE         is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 terminals
**                       and nonterminals.  "int" is used otherwise.
**    YYNOCODE           is a number of type YYCODETYPE which corresponds
**                       to no legal terminal or nonterminal number.  This
**                       number is used to fill in empty slots of the hash 
**                       table.
**    YYFALLBACK         If defined, this indicates that one or more tokens
**                       have fall-back values which should be used if the
**                       original value of the token will not parse.
**    YYACTIONTYPE       is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 rules and
**                       states combined.  "int" is used otherwise.
**    ParseTOKENTYPE     is the data type used for minor tokens given 
**                       directly to the parser from the tokenizer.
**    YYMINORTYPE        is the data type used for all minor tokens.
**                       This is typically a union of many types, one of
**                       which is ParseTOKENTYPE.  The entry in the union
**                       for base tokens is called "yy0".
**    YYSTACKDEPTH       is the maximum depth of the parser's stack.  If
**                       zero the stack is dynamically sized using realloc()
**    ParseARG_SDECL     A static variable declaration for the %extra_argument
**    ParseARG_PDECL     A parameter declaration for the %extra_argument
**    ParseARG_STORE     Code to store %extra_argument into yypParser
**    ParseARG_FETCH     Code to extract %extra_argument from yypParser
**    YYNSTATE           the combined number of states.
**    YYNRULE            the number of rules in the grammar
**    YYERRORSYMBOL      is the code number of the error symbol.  If not
**                       defined, then do no error processing.
*/
%%
/* since we cant use expressions to initialize these as class
 * constants, we do so during parser init. */
var $YY_NO_ACTION;
var $YY_ACCEPT_ACTION;
var $YY_ERROR_ACTION;

/* The yyzerominor constant is used to initialize instances of
** YYMINORTYPE objects to zero. */
static $yyzerominor = 0;

/* Next are the tables used to determine what action to take based on the
** current state and lookahead token.  These tables are used to implement
** functions that take a state number and lookahead value and return an
** action integer.  
**
** Suppose the action integer is N.  Then the action is determined as
** follows
**
**   0 <= N < YYNSTATE                  Shift N.  That is, push the lookahead
**                                      token onto the stack and goto state N.
**
**   YYNSTATE <= N < YYNSTATE+YYNRULE   Reduce by rule N-YYNSTATE.
**
**   N == YYNSTATE+YYNRULE              A syntax error has occurred.
**
**   N == YYNSTATE+YYNRULE+1            The parser accepts its input.
**
**   N == YYNSTATE+YYNRULE+2            No such action.  Denotes unused
**                                      slots in the yy_action[] table.
**
** The action table is constructed as a single large table named yy_action[].
** Given state S and lookahead X, the action is computed as
**
**      yy_action[ yy_shift_ofst[S] + X ]
**
** If the index value yy_shift_ofst[S]+X is out of range or if the value
** yy_lookahead[yy_shift_ofst[S]+X] is not equal to X or if yy_shift_ofst[S]
** is equal to YY_SHIFT_USE_DFLT, it means that the action is not in the table
** and that yy_default[S] should be used instead.  
**
** The formula above is for computing the action when the lookahead is
** a terminal symbol.  If the lookahead is a non-terminal (as occurs after
** a reduce action) then the yy_reduce_ofst[] array is used in place of
** the yy_shift_ofst[] array and YY_REDUCE_USE_DFLT is used in place of
** YY_SHIFT_USE_DFLT.
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
*/
%%

/* The next table maps tokens into fallback tokens.  If a construct
** like the following:
** 
**      %fallback ID X Y Z.
**
** appears in the grammar, then ID becomes a fallback token for X, Y,
** and Z.  Whenever one of the tokens X, Y, or Z is input to the parser
** but it does not parse, the type of the token is changed to ID and
** the parse is retried before an error is thrown.
*/
static $yyFallback = array(
%%
);

/* The following structure represents a single element of the
** parser's stack.  Information stored includes:
**
**   +  The state number for the parser at this level of the stack.
**
**   +  The value of the token stored at this level of the stack.
**      (In other words, the "major" token.)
**
**   +  The semantic value stored at this level of the stack.  This is
**      the information used by the action routines in the grammar.
**      It is sometimes called the "minor" token.
*/
class yyStackEntry {
  var /* YYACTIONTYPE */ $stateno;  /* The state-number */
  var /* YYCODETYPE */ $major;      /* The major token value.  This is the code
                                    ** number for the token at this stack level */
  var /* YYMINORTYPE */ $minor;     /* The user-supplied minor token value.  This
                                    ** is the value of the token  */
};

/* The state of the parser is completely contained in an instance of
** the following structure */
class yyParser {
  var /* int */ $yyidx;                    /* Index of top element in stack */
  var /* int */ $yyidxMax;                 /* Maximum value of yyidx */
  var /* int */ $yyerrcnt;                 /* Shifts left before out of the error */
  // ParseARG_SDECL                        /* A place to hold %extra_argument */
  var /* yyStackEntry */ $yystack = array(
       /* of YYSTACKDEPTH elements */
       ); /* The parser's stack */
};

var $yyTraceFILE = null;
var $yyTracePrompt = null;

/* 
** Turn parser tracing on by giving a stream to which to write the trace
** and a prompt to preface each trace message.  Tracing is turned off
** by making either argument NULL 
**
** Inputs:
** <ul>
** <li> A FILE* to which trace output should be written.
**      If NULL, then tracing is turned off.
** <li> A prefix string written at the beginning of every
**      line of trace output.  If NULL, then tracing is
**      turned off.
** </ul>
**
** Outputs:
** None.
*/
function ParseTrace(/* stream */ $TraceFILE, /* string */ $zTracePrompt){
  $this->yyTraceFILE = $TraceFILE;
  $this->yyTracePrompt = $zTracePrompt;
  if( $this->yyTraceFILE===null ) $this->yyTracePrompt = null;
  else if( $this->yyTracePrompt===null ) $this->yyTraceFILE = null;
}

/* For tracing shifts, the names of all terminals and nonterminals
** are required.  The following table supplies these names */
static $yyTokenName = array( 
%%
);

/* For tracing reduce actions, the names of all rules are required.
*/
static $yyRuleName = array(
%%
);


/*
** Try to increase the size of the parser stack.
*/
function yyGrowStack(){
}

public function __construct(){
  $this->yyidx = -1;
  $this->yyidxMax = 0;

  $this->yyGrowStack();
}

/* The following function deletes the value associated with a
** symbol.  The symbol can be either a terminal or nonterminal.
** "yymajor" is the symbol code, and "yypminor" is a pointer to
** the value.
*/
private function yy_destructor($yymajor,$yypminor){
  ParseARG_FETCH;
  switch( $yymajor ){
    /* Here is inserted the actions which take place when a
    ** terminal or non-terminal is destroyed.  This can happen
    ** when the symbol is popped from the stack during a
    ** reduce or during error processing or when a parser is 
    ** being destroyed before it is finished parsing.
    **
    ** Note: during a reduce, the only symbols destroyed are those
    ** which appear on the RHS of the rule, but which are not used
    ** inside the C code.
    */
%%
    default:  break;   /* If no destructor action specified: do nothing */
  }
}

/*
** Pop the parser's stack once.
**
** If there is a destructor routine associated with the token which
** is popped from the stack, then call it.
**
** Return the major token number for the symbol popped.
*/
private function yy_pop_parser_stack(){
  if( $this->yyidx<0 ) return 0;
  $yytos = $this->yystack[$this->yyidx];

  if( $this->yyTraceFILE && $this->yyidx>=0 ){
    fprintf($this->yyTraceFILE,"%sPopping %s\n",
      $this->yyTracePrompt,
      self::$yyTokenName[$yytos->major]);
  }
  $yymajor = $yytos->major;
  $this->yy_destructor($yymajor, $yytos->minor);
  unset($this->yystack[$this->yyidx]);
  $this->yyidx--;
  return $yymajor;
}

/* 
** Deallocate and destroy a parser.  Destructors are all called for
** all stack elements before shutting the parser down.
**
** Inputs:
** <ul>
** <li>  A pointer to the parser.  This should be a pointer
**       obtained from ParseAlloc.
** <li>  A pointer to a function used to reclaim memory obtained
**       from malloc.
** </ul>
*/
function __destruct(){
  while( $this->yyidx>=0 ) $this->yy_pop_parser_stack();
}

/*
** Return the peak depth of the stack for a parser.
*/
public function ParseStackPeak(){
  return $this->yyidxMax;
}

/*
** Find the appropriate action for a parser given the terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
private function yy_find_shift_action($iLookAhead){
  $i = 0;
  $stateno = $this->yystack[$this->yyidx]->stateno;
 
  if( $stateno>self::YY_SHIFT_COUNT
   || ($i = self::$yy_shift_ofst[$stateno])==self::YY_SHIFT_USE_DFLT ){
    return self::$yy_default[$stateno];
  }
  assert( $iLookAhead!=self::YYNOCODE );
  $i += $iLookAhead;
  if( $i<0 || $i>=count(self::$yy_action) || self::$yy_lookahead[$i]!=$iLookAhead ){
    if( $iLookAhead>0 ){
      if( isset(self::$yyFallback[$iLookAhead])
             && ($iFallback = self::$yyFallback[$iLookAhead])!=0 ){
        if( $this->yyTraceFILE ){
          fprintf($this->yyTraceFILE, "%sFALLBACK %s => %s\n",
             $this->yyTracePrompt, self::$yyTokenName[$iLookAhead],
             self::$yyTokenName[$iFallback]);
        }
        return $this->yy_find_shift_action($iFallback);
      }
      {
        $j = $i - $iLookAhead + self::YYWILDCARD;
        if( 
          $j>=0 &&
          $j<count(self::$yy_action) &&
          self::$yy_lookahead[$j]==self::YYWILDCARD
        ){
          if( $this->yyTraceFILE ){
            fprintf($this->yyTraceFILE, "%sWILDCARD %s => %s\n",
               $this->yyTracePrompt, self::$yyTokenName[$iLookAhead], self::$yyTokenName[self::YYWILDCARD]);
          }
          return self::$yy_action[$j];
        }
      }
    }
    return self::$yy_default[$stateno];
  }else{
    return self::$yy_action[$i];
  }
}

/*
** Find the appropriate action for a parser given the non-terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
private function yy_find_reduce_action(
  $stateno,              /* Current state number */
  $iLookAhead     /* The look-ahead token */
){
  $i = 0;
  if( $stateno>self::YY_REDUCE_COUNT ){
    return self::$yy_default[$stateno];
  }
  assert( $stateno<=self::YY_REDUCE_COUNT );
  $i = self::yy_reduce_ofst[$stateno];
  assert( $i!=self::YY_REDUCE_USE_DFLT );
  assert( $iLookAhead!=self::YYNOCODE );
  $i += $iLookAhead;
  if( $i<0 || $i>=count(self::$yy_action) || self::$yy_lookahead[$i]!=$iLookAhead ){
    return self::$yy_default[$stateno];
  }
  assert( $i>=0 && $i<count(self::$yy_action) );
  assert( self::$yy_lookahead[$i]==$iLookAhead );
  return self::$yy_action[$i];
}

/*
** The following routine is called if the stack overflows.
*/
private function yyStackOverflow($yypMinor){
   ParseARG_FETCH;
   $this->yyidx--;
   if( $this->yyTraceFILE ){
     fprintf($this->yyTraceFILE,"%sStack Overflow!\n",$this->yyTracePrompt);
   }
   while( $this->yyidx>=0 ) yy_pop_parser_stack();
   /* Here code is inserted which will execute if the parser
   ** stack every overflows */
%%
   ParseARG_STORE; /* Suppress warning about unused %extra_argument var */
}

/*
** Perform a shift action.
*/
private function yy_shift(
  $yyNewState,               /* The new state to shift in */
  $yyMajor,                  /* The major token to shift in */
  $yypMinor         /* Pointer to the minor token to shift in */
){
  $this->yyidx++;
  if (isset($this->yystack[$this->yyidx])) {
    $yytos = $this->yystack[$this->yyidx];
  } else {
    $yytos = new yyStackEntry();
    $this->yystack[$this->yyidx] = $yytos;
  }
  $yytos->stateno = $yyNewState;
  $yytos->major = $yyMajor;
  $yytos->minor = $yypMinor;
  if( $this->yyTraceFILE && $this->yyidx>0 ){
    fprintf($this->yyTraceFILE,"%sShift %d\n",$this->yyTracePrompt,$yyNewState);
    fprintf($this->yyTraceFILE,"%sStack:",$this->yyTracePrompt);
    for($i=1; $i<=$this->yyidx; $i++)
      fprintf($this->yyTraceFILE," %s",self::yyTokenName[$this->yystack[$i]->major]);
    fprintf($this->yyTraceFILE,"\n");
  }
}

/* The following table contains information about every rule that
** is used during the reduce.
*/
/*
static const struct {
  YYCODETYPE lhs;         // Symbol on the left-hand side of the rule 
  unsigned char nrhs;     // Number of right-hand side symbols in the rule 
} yyRuleInfo[] = {
*/
static $yyRuleInfo = array(
%%
);

/*
** Perform a reduce action and the shift that must immediately
** follow the reduce.
*/
private function yy_reduce(
  $yyruleno                 /* Number of the rule by which to reduce */
){
  $yygoto = 0;                     /* The next state */
  $yyact = 0;                      /* The next action */
  $yygotominor = null;        /* The LHS of the rule reduced */
  $yymsp = null;            /* The top of the parser's stack */
  $yysize = 0;                     /* Amount to pop the stack */
  ParseARG_FETCH;
  $yymsp = $this->yystack[$this->yyidx];
  if( $this->yyTraceFILE && $yyruleno>=0 
	&& isset(self::$yyRuleName[$yyruleno])) ){
    fprintf($this->yyTraceFILE, "%sReduce [%s].\n", $this->yyTracePrompt,
      self::$yyRuleName[$yyruleno]);
  }

  /* Silence complaints from purify about yygotominor being uninitialized
  ** in some cases when it is copied into the stack after the following
  ** switch.  yygotominor is uninitialized when a rule reduces that does
  ** not set the value of its left-hand side nonterminal.  Leaving the
  ** value of the nonterminal uninitialized is utterly harmless as long
  ** as the value is never used.  So really the only thing this code
  ** accomplishes is to quieten purify.  
  **
  ** 2007-01-16:  The wireshark project (www.wireshark.org) reports that
  ** without this code, their parser segfaults.  I'm not sure what there
  ** parser is doing to make this happen.  This is the second bug report
  ** from wireshark this week.  Clearly they are stressing Lemon in ways
  ** that it has not been previously stressed...  (SQLite ticket #2172)
  */
  /*memset(&yygotominor, 0, sizeof(yygotominor));*/
  $yygotominor = self::$yyzerominor;


  switch( $yyruleno ){
  /* Beginning here are the reduction cases.  A typical example
  ** follows:
  **   case 0:
  **  #line <lineno> <grammarfile>
  **     { ... }           // User supplied code
  **  #line <lineno> <thisfile>
  **     break;
  */
%%
  };
  $yygoto = self::$yyRuleInfo[2*$yyruleno];
  $yysize = self::$yyRuleInfo[(2*$yyruleno)+1];

  $state_for_reduce = $this->yystack[$this->yyidx - $yysize]->stateno;

  $this->yyidx -= $yysize;
  $yyact = $this->yy_find_reduce_action($state_for_reduce,$yygoto);
  if( $yyact < self::YYNSTATE ){
    /* If we are not debugging and the reduce action popped at least
    ** one element off the stack, then we can push the new element back
    ** onto the stack here, and skip the stack overflow test in yy_shift().
    ** That gives a significant speed improvement. */
    $this->yy_shift($yyact,$yygoto,$yygotominor);
  }else{
    assert( $yyact == self::YYNSTATE + self::YYNRULE + 1 );
    $this->yy_accept();
  }
}

/*
** The following code executes when the parse fails
*/
private function yy_parse_failed(
){
  ParseARG_FETCH;
  if( $this->yyTraceFILE ){
    fprintf($this->yyTraceFILE,"%sFail!\n",$this->yyTracePrompt);
  }
  while( $this->yyidx>=0 ) $this->yy_pop_parser_stack();
  /* Here code is inserted which will be executed whenever the
  ** parser fails */
%%
  ParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following code executes when a syntax error first occurs.
*/
private function yy_syntax_error(
  $yymajor,                   /* The major type of the error token */
  $yyminor            /* The minor type of the error token */
){
  ParseARG_FETCH;
%%
  ParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following is executed when the parser accepts
*/
private function yy_accept(
){
  ParseARG_FETCH;
  if( $this->yyTraceFILE ){
    fprintf($this->yyTraceFILE,"%sAccept!\n",$this->yyTracePrompt);
  }
  while( $this->yyidx>=0 ) $this->yy_pop_parser_stack();
  /* Here code is inserted which will be executed whenever the
  ** parser accepts */
%%
  ParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/* The main parser program.
** The first argument is a pointer to a structure obtained from
** "ParseAlloc" which describes the current state of the parser.
** The second argument is the major token number.  The third is
** the minor token.  The fourth optional argument is whatever the
** user wants (and specified in the grammar) and is available for
** use by the action routines.
**
** Inputs:
** <ul>
** <li> A pointer to the parser (an opaque structure.)
** <li> The major token number.
** <li> The minor token number.
** <li> An option argument of a grammar-specified type.
** </ul>
**
** Outputs:
** None.
*/
function Parse(
  $yymajor,                 /* The major token code number */
  $yyminor = null       /* The value for the token */
  ParseARG_PDECL               /* Optional %extra_argument parameter */
){
  $yyact = 0;            /* The parser action. */
  $yyendofinput = 0;     /* True if we are at the end of input */
  $yyerrorhit = 0;   /* True if yymajor has invoked an error */

  /* (re)initialize the parser, if necessary */
  if( $this->yyidx<0 ){
    $this->yyidx = 0;
    $this->yyerrcnt = -1;
    $ent = new yyStackEntry();
    $ent->stateno = 0;
    $ent->major = 0;
    $this->yystack = array( 0 => $ent );
    $this->YY_NO_ACTION = self::YYNSTATE + self::YYNRULE + 2;
    $this->YY_ACCEPT_ACTION = self::YYNSTATE + self::YYNRULE + 1;
    $this->YY_ERROR_ACTION = self::YYNSTATE + self::YYNRULE;
  }
  $yyendofinput = ($yymajor==0);
  ParseARG_STORE;

  if( $this->yyTraceFILE ){
    fprintf($this->yyTraceFILE,"%sInput %s\n",$this->yyTracePrompt,self::$yyTokenName[$yymajor]);
  }

  do{
    $yyact = $this->yy_find_shift_action($yymajor);
    if( $yyact<self::YYNSTATE ){
      assert( !$yyendofinput );  /* Impossible to shift the $ token */
      $this->yy_shift($yyact,$yymajor,$yyminor);
      $this->yyerrcnt--;
      $yymajor = self::YYNOCODE;
    }else if( $yyact < self::YYNSTATE + self::YYNRULE ){
      $this->yy_reduce($yyact-self::YYNSTATE);
    }else{
      assert( $yyact == self::YY_ERROR_ACTION );
      $yymx = 0;
      if( $this->yyTraceFILE ){
        fprintf($this->yyTraceFILE,"%sSyntax Error!\n",$this->yyTracePrompt);
      }
if(self::YYERRORSYMBOL) {
      /* A syntax error has occurred.
      ** The response to an error depends upon whether or not the
      ** grammar defines an error token "ERROR".  
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
      if( $this->yyerrcnt<0 ){
        $this->yy_syntax_error($yymajor,$yyminor);
      }
      $yymx = $this->yystack[$this->yyidx]->major;
      if( $yymx==self::YYERRORSYMBOL || $yyerrorhit ){
        if( $this->yyTraceFILE ){
          fprintf($this->yyTraceFILE,"%sDiscard input token %s\n",
             $this->yyTracePrompt,self::$yyTokenName[$yymajor]);
        }
        $this->yy_destructor($yymajor,$yyminor);
        $yymajor = self::YYNOCODE;
      }else{
         while(
          $this->yyidx >= 0 &&
          $yymx != self::YYERRORSYMBOL &&
          ($yyact = $this->yy_find_reduce_action(
                        $this->yystack[$this->yyidx]->stateno,
                        self::YYERRORSYMBOL)) >= self::YYNSTATE
        ){
          $this->yy_pop_parser_stack();
        }
        if( $this->yyidx < 0 || $yymajor==0 ){
          $this->yy_destructor($yymajor,$yyminor);
          $this->yy_parse_failed();
          $yymajor = self::YYNOCODE;
        }else if( $yymx!=self::YYERRORSYMBOL ){
          $u2 = 0;
          $this->yy_shift($yyact,self::YYERRORSYMBOL,$u2);
        }
      }
      $this->yyerrcnt = 3;
      $yyerrorhit = 1;
} else { /* YYERRORSYMBOL is not defined */
      /* If the YYNOERRORRECOVERY macro is defined, then do not attempt to
      ** do any kind of error recovery.  Instead, simply invoke the syntax
      ** error routine and continue going as if nothing had happened.
      **
      ** Applications can set this macro (for example inside %include) if
      ** they intend to abandon the parse upon the first syntax error seen.
      */
      $this->yy_syntax_error($yymajor,$yyminor);
      $this->yy_destructor($yymajor,$yyminor);
      $yymajor = self::YYNOCODE;
      
      /* This is what we do if the grammar does not define ERROR:
      **
      **  * Report an error message, and throw away the input token.
      **
      **  * If the input token is $, then fail the parse.
      **
      ** As before, subsequent error messages are suppressed until
      ** three input tokens have been successfully shifted.
      */
      if( $this->yyerrcnt<=0 ){
        $this->yy_syntax_error($yymajor,$yyminor);
      }
      $this->yyerrcnt = 3;
      $this->yy_destructor($yymajor,$yyminor);
      if( $yyendofinput ){
        $this->yy_parse_failed();
      }
      $yymajor = self::YYNOCODE;
    }
}
  }while( $yymajor!=self::YYNOCODE && $this->yyidx>=0 );
  return;
}

