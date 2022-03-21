%{
    open Ast
%}

/*functional*
%token FUNC RETURN
%token LINK USE IN STEP AS

/*type*/
%token INT STRING FLOAT BOOLEAN NONE

/*conditional*/
%token IF ELSE ELIF FOR WHILE INFINITE_LOOP BREAK CONTINUE TRY CATCH RAISE 

/*arithmetic */
%token PLUS MINUS MULT DIV MOD ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVEQ MODEQ EQ

/*relational*/ 
%token NEQ GTEQ LTEQ GT LT AND OR NOT QUESTION LPAREN RPAREN LBRACKET

/*delimiters*/
%token RBRACKET LBRACE RBRACE COMMA COLON DOT SEMI ARRAY QUEUE STACK LINKEDNODE TREENODE
%token EOF

%token <bool> BLIT
%token <string> ID 
%token <int> INTLIT 
%token <float> FLOATLIT /* not implemented */
%token <string> STRINGLIT /* not implemented correctly */

%left SEMI
%right ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVEQ MODEQ
%left OR AND
%left EQ NEQ
%left LT
%left PLUS MINUS MULT DIV MOD

/* need more tokens FOR THE LITERALS */

%start program 
%type <Ast.tokenseq> program 

%%

program:
  tokens EOF { $1}

tokens:
 /* nothing */ { [] }
| one_token tokens {$1 :: $2}

one_token:
| SEMI   { "SEMI" }
| LPAREN { "LPAREN" }
| RPAREN { "RPAREN"}

/* reserved keywords */
| FUNC { "FUNC" }
| IF   { "IF" }
| ELSE { "ELSE" }
| ELIF { "ELIF" }
| FOR { "FOR" }
| WHILE { "WHILE" }
| INFINITE_LOOP { "INFINITE_LOOP" }
| RETURN {"RETURN"}
| BREAK {"BREAK"}
| CONTINUE {"CONTINUE"}
| TRY {"TRY"}
| CATCH {"CATCH"}
| RAISE {"RAISE"}
| LINK {"LINK"}
| USE {"USE"}
| IN {"IN"}
| STEP {"STEP"}
| AS {"AS"}

/* our data types */
| INT {"INT"}
| INTLIT {"INTLIT: " ^ string_of_int $1}
| STRING {"STRING"}
| STRINGLIT {"STRINGLIT: " ^ $1}
| FLOAT {"FLOAT"}
| BOOLEAN {"BOOLEAN"}
| BLIT {"BOOL: " ^ string_of_bool $1}
| NONE {"NONE"}
| ID {"ID: " ^ $1}

/* arithmetic operators */
| PLUS {"PLUS"}
| MINUS {"MINUS"}
| MULT {"MULT"}
| DIV {"DIV"}
| MOD {"MOD"}

/* assignment operators */
| ASSIGN {"ASSIGN"}
| PLUSEQ {"PLUSEQ"}
| MINUSEQ {"MINUSEQ"}
| TIMESEQ {"TIMESEQ"}
| DIVEQ {"DIVEQ"}
| MODEQ {"MODEQ"}

/* relational operators */
| EQ {"EQ"}
| NEQ {"NEQ"}
| GTEQ {"GTEQ"}
| LTEQ {"LTEQ"}
| GT {"GT"}
| LT {"LT"}
| AND {"AND"}
| OR {"OR"}
| NOT {"NOT"}
| QUESTION {"QUESTION"}

/* delimiters */
| LPAREN  { "LPAREN" }
| RPAREN  { "RPAREN" }
| LBRACKET {"LBRACKET"}
| RBRACKET {"RBRACKET"}
| LBRACE {"LBRACE"}
| RBRACE {"RBRACE"}
| COMMA {"COMMA"}
| COLON {"COLON"}
| DOT {"DOT"}
| SEMI  { "SEMI" }

/* ADT declarations */
| ARRAY      {"ARRAY"}
| QUEUE      {"QUEUE"}
| STACK      {"STACK"}
| LINKEDNODE {"LINKEDNODE"}
| TREENODE   {"TREENODE"}