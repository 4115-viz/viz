%{
open Ast
%}

/* arithmetic */
%token PLUS MINUS TIMES DIVIDE

/* assignment */
%token ASSIGN

/* keywords */
%token FUNC

/* type */
%token T_NONE T_STR T_INT T_BOOL

/* delimiters */
%token SEMI LPAREN RPAREN LBRACE RBRACE COLON COMMA
%token EOF

%token <string> ID
%token <string> LIT_STR
%token <int> LIT_INT
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

stmt_list:
  | { [] }
  | stmt stmt_list { $1 :: $2 }

stmt:
  | expr SEMI { Expr $1 }
  | FUNC ID LPAREN params_list_opt RPAREN COLON typ LBRACE params_list_opt stmt_list RBRACE {
      FuncDecl({ 
        typ = $7; (* function return type *)
        name = $2; (* function name *)
        params = $4; (* function arguments *)
        locals = $9; (* all declared variables *)
        body = $10; (* function statements *)
      })
    }

typ:
  | T_NONE { NoneType }
  | T_STR { StrType }
  | T_INT { IntType }
  | T_BOOL { BoolType }

expr:
  /* literal */
  | LIT_STR { StrLit $1 }
  | LIT_INT { IntLit $1 }
  | LIT_BOOL { BoolLit $1 }

  /* variable */
  | ID  { Id $1 }

  /* function */
  | ID LPAREN args_list_opt RPAREN { FuncCall($1, $3) }

  /* assignment */
  | ID COLON typ ASSIGN expr { Assign($1, $5) }
  | ID ASSIGN expr { Assign($1, $3) }

params_list_opt:
  { [] }
| params_list { $1 }

params_list:
  /* variable declaration, with default values */

  ID COLON typ SEMI { [($3, $1)] }
| params_list COMMA ID COLON typ { ($5, $3) :: $1 }

args_list_opt:
  | { [] }
  | args_list { $1 }

args_list:
  | expr { [$1] }
