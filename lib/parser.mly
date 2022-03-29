%{
open Ast
%}

/* arithmetic */
%token PLUS MINUS TIMES DIVIDE

/* assignment */
%token ASSIGN

/* keywords */
%token FUNC

%token BLOCK

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
  decls EOF { $1 }

typ:
  | T_NONE { NoneType }
  | T_STR { StrType }
  | T_INT { IntType }
  | T_BOOL { BoolType }

decls:
   /* nothing */ { ([], []) }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  { $1 :: $3 }

vdecl:
  typ ID { ($1, $2) }

stmt_list:
  | { [] }
  | stmt stmt_list { $1 :: $2 }

stmt:
  | expr SEMI { Expr $1 }
  | LBRACE stmt_list RBRACE { Block($2) }

expr:
  /* literal */
  | LIT_STR { StrLit $1 }
  | LIT_INT { IntLit $1 }
  | LIT_BOOL { BoolLit $1 }

  /* variable */
  | ID { Id $1 }

  /* function */
  | ID LPAREN args_list_opt RPAREN { FuncCall($1, $3) }

  /* assignment */
  | ID COLON typ ASSIGN expr { Assign($1, $5) }

fdecl:
  FUNC ID LPAREN params_list_opt RPAREN COLON typ LBRACE stmt_list RBRACE 
  {
    {
      typ=$7;
      name=$2;
      params=$4;
      body=$9
    }
  }

params_list_opt:
  { [] }
| params_list { $1 }

params_list:
  ID COLON typ { [($3, $1)] }
| params_list COMMA ID COLON typ { ($5, $3) :: $1 }

args_list_opt:
  | { [] }
  | args_list { $1 }

args_list:
  | expr { [$1] }
