%{
    open Ast
%}

/*functional*/
%token FUNC RETURN
%token LINK USE IN STEP AS

/*type*/
%token INT STRING FLOAT BOOL NONE

/*conditional*/
%token IF ELSE ELIF FOR WHILE INFINITE_LOOP BREAK CONTINUE TRY CATCH RAISE 

/*arithmetic */
%token PLUS MINUS MULT DIV MOD ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVEQ MODEQ EQ

/*relational*/ 
%token NEQ GTEQ LTEQ GT LT AND OR NOT QUESTION LPAREN RPAREN LBRACKET

/*delimiters*/
%token RBRACKET LBRACE RBRACE COMMA COLON DOT SEMI ARRAY QUEUE STACK LINKEDNODE TREENODE
%token EOF

%token <bool> BOOL_LITERAL
%token <string> ID 
%token <int> INT_LITERAL 
%token <float> FLOAT_LITERAL /* not implemented */
%token <string> STRING_LITERAL /* not implemented correctly */

%left SEMI
%right ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVEQ MODEQ
%left OR AND
%left EQ NEQ
%left LT
%left PLUS MINUS MULT DIV MOD

/* need more tokens FOR THE LITERALS */

%start program_rule
%type <Ast.program> program_rule 

%%
vdecl_list_rule:
  /*nothing*/                   { []       }
  | vdecl_rule vdecl_list_rule  { $1 :: $2 }

vdecl_rule:
   ID COLON typ_rule SEMI { ($1, $3) }

typ_rule:
  INT       { Int  }
  | BOOL    { Bool }
  | FLOAT   { Float }
  | STRING  { String }
  | NONE    { None }

stmt_list_rule:
    /* nothing */               { []     }
    | stmt_rule stmt_list_rule  { $1::$2 }

stmt_rule:
  expr_rule SEMI                                          { Expr $1         }
  | LBRACE stmt_list_rule RBRACE                          { Block $2        }
  /*need to add loop and function here*/

expr_rule:
  | BLIT                          { BoolLit $1            }
  | LITERAL                       { Literal $1            }
  | ID                            { Id $1                 }
  | expr_rule PLUS expr_rule      { Binop ($1, Add, $3)   }
  | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3)   }
  | expr_rule EQ expr_rule        { Binop ($1, Equal, $3) }
  | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3)   }
  | expr_rule LT expr_rule        { Binop ($1, Less, $3)  }
  | expr_rule AND expr_rule       { Binop ($1, And, $3)   }
  | expr_rule OR expr_rule        { Binop ($1, Or, $3)    }
  | ID ASSIGN expr_rule           { Assign ($1, $3)       }
  | LPAREN expr_rule RPAREN       { $2                    }

program_rule:
  stmt_list EOF { $1 }

stmt_list:
  | { [] }
  | stmt stmt_list { $1 :: $2 }

stmt:
  | expr SEMI { Expr $1 }

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
  | expr COMMA args_list { $1 :: $3 }


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
| PLUS_EQ {"PLUS_EQ"}
| MINUS_EQ {"MINUS_EQ"}
| TIMES_EQ {"TIMES_EQ"}
| DIV_EQ {"DIVEQ"}
| MOD_EQ {"MODEQ"}

/* relational operators */
| EQ {"EQ"}
| N_EQ {"N_EQ"}
| GT_EQ {"GT_EQ"}
| LT_EQ {"LT_EQ"}
| GT {"GT"}
| LT {"LT"}
| AND {"AND"}
| OR {"OR"}
| NOT {"NOT"}
| QUESTION {"QUESTION"}

/* delimiters */
| LPAREN  { "LPAREN" }
| RPAREN  { "RPAREN" }
| LBRACKET { "LBRACKET" }
| RBRACKET { "RBRACKET" }
| LBRACE { "LBRACE" }
| RBRACE { "RBRACE" }
| COMMA { "COMMA" }
| COLON { "COLON" }
| DOT { "DOT" }
| SEMI  { "SEMI" }

/* ADT declarations */
| ARRAY      { "ARRAY" }
| QUEUE      { "QUEUE" }
| STACK      { "STACK" }
| LINKED_NODE { "LINKED_NODE" }
| TREE_NODE   { "TREE_NODE" }
