%token FUNC IF ELSE ELIF FOR WHILE INFINITE_LOOP RETURN BREAK CONTINUE TRY 
%token CATCH RAISE LINK USE IN STEP AS INT STRING FLOAT BOOLEAN NONE
%token PLUS MINUS MULT DIV MOD ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVEQ MODEQ EQ
%token NEQ GTEQ LTEQ GT LT AND OR NOT QUESTION SEMI LPAREN RPAREN LBRACKET
%token RBRACKET LBRACE RBRACE COMMA COLON DOT SEMI ARRAY QUEUE STACK LINKEDNODE TREENODE
%token EOF

%token <bool> BOOL_LITERAL
%token <string> ID 
%token <int> INT_LITERAL 
%token <float> FLOAT_LITERAL /* not implemented */
%token <string> STRING_LITERAL /* not implemented correctly */

/* need more tokens FOR THE LITERALS */

%start program
%type <Ast.program> program 

%%

program:
  stmt_list EOF { $1 }

stmt_list:
  | { [] }
  | stmt stmt_list { $1 :: $2 }

stmt:
  | expr SEMI { EXPR $1 }

expr:
  /* literal */
  | BOOL_LITERAL { BoolLit $1 }
  | INT_LITERAL { IntLit $1 }
  | FLOAT_LITERAL { FloatLit $1 }

  /* variable */
  | ID { Id $1 }

  /* function */
  | ID LPAREN args_list RPAREN { FuncCall($1, $3) }

args_list_optional:
  | { [] }
  | args_list { $1 }

args_list:
  | expr { [$1] }
  | args_list COMMA expr { $1 :: $3 }


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
| INT_LITERAL {"INTLIT: " ^ string_of_int $1}
| STRING {"STRING"}
| STRING_LITERAL {"STRINGLIT: " ^ $1}
| FLOAT {"FLOAT"}
| BOOLEAN {"BOOLEAN"}
| BOOL_LITERAL {"BOOL: " ^ string_of_bool $1}
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