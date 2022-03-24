%{
open Ast
%}

/* keywords */
%token FUNC

/* type */
%token T_NONE T_STR

/* delimiters */
%token SEMI LPAREN RPAREN LBRACE RBRACE COLON
%token EOF

%token <string> ID
%token <string> LIT_STR

%start program
%type <Ast.program> program

%%

program:
  stmt_list EOF { $1 }

typ:
  | T_NONE { NoneType }
  | T_STR { StrType }

stmt_list:
  | { [] }
  | stmt stmt_list { $1 :: $2 }

stmt:
  | expr SEMI { Expr $1 }
  | FUNC ID LPAREN RPAREN COLON typ LBRACE stmt_list RBRACE {
      FuncDecl({ 
        typ = $6;
        name = $2;
        body = $8
      })
    }

expr:
  /* literal */
  | LIT_STR { StrLit $1 }

  /* variable */
  | ID { Id $1 }

  /* function */
  | ID LPAREN args_list_opt RPAREN { FuncCall($1, $3) }

args_list_opt:
  | { [] }
  | args_list { $1 }

args_list:
  | expr { [$1] }
