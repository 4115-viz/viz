%{
open Ast
%}

/* arithmetic */
%token PLUS MINUS TIMES DIVIDE

/* assignment */
%token ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVEQ MODEQ

/* relational */
%token EQ NEQ GTEQ LTEQ GT LT AND OR NOT 

/* keywords */
%token FUNC IF ELSE ELIF FOR WHILE INFINITE_LOOP RETURN BREAK
%token CONTINUE TRY CATCH RAISE LINK USE IN STEP AS 

/* type */
%token T_NONE T_STR T_INT T_BOOL T_FLOAT

/* delimiters */
%token SEMI LPAREN RPAREN LBRACE RBRACE COLON COMMA LBRACKET RBRACKET
%token EOF

/* split id into two, nothing changes outside of parser file */
%token <string> ID_FUNC 
%token <string> ID_VAR /* split so variables could have @ */
%token <string> LIT_STR
%token <int> LIT_INT
%token <float> LIT_FLOAT
%token <bool> LIT_BOOL

/* precedence */
%left SEMI
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE


%start program
%type <Ast.program> program

%%

program:
  stmt_list EOF { $1 }

typ:
  | T_NONE { NoneType }
  | T_STR { StrType }
  | T_INT { IntType }
  | T_BOOL { BoolType }
  | T_FLOAT { FloatType }

stmt_list:
  | { [] }
  | stmt stmt_list { $1 :: $2 }

stmt:
  | expr SEMI { Expr $1 }
  | FUNC ID_FUNC LPAREN params_list_opt RPAREN COLON typ LBRACE stmt_list RBRACE {
      FuncDecl({ 
        typ = $7;
        name = $2;
        params = $4;
        body = $9;
      })
    }

expr:
  /* literal */
  | LIT_STR { StrLit($1) }
  | LIT_INT { IntLit($1) }
  | LIT_BOOL { BoolLit($1) }
  | LIT_FLOAT { FloatLit($1) }

  /* variable */
  | ID_VAR { Id($1) }

  /* function */
  | ID_FUNC LPAREN args_list_opt RPAREN { FuncCall($1, $3) }

  /* assignment */
  | ID_VAR COLON typ ASSIGN expr { Assign($1, $5) }

params_list_opt:
  { [] }
| params_list { $1 }

params_list:
  ID_VAR COLON typ { [($3, $1)] }
| params_list COMMA ID_VAR COLON typ { ($5, $3) :: $1 }

args_list_opt:
  | { [] }
  | args_list { $1 }

args_list:
  | expr { [$1] }
