%{
open Ast
%}

/* arithmetic */
%token PLUS MINUS TIMES DIVIDE MOD

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
%token SEMI LPAREN RPAREN LBRACE RBRACE COLON COMMA LBRACKET RBRACKET DOT
%token EOF

/* split id into two, nothing changes outside of parser file */
/*
%token <string> ID_FUNC 
%token <string> ID_VAR
*/

%token <string> ID
%token <string> LIT_STR
%token <int> LIT_INT
%token <float> LIT_FLOAT
%token <bool> LIT_BOOL


/* precedence following C standard*/
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS


%start program
%type <Ast.program> program

%%


program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

 

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

/* @x: string */
vdecl:
  typ ID {($1, $2)}

typ:
  | T_NONE { NoneType }
  | T_STR { StrType }
  | T_INT { IntType }
  | T_BOOL { BoolType }
  | T_FLOAT { FloatType }


/* function declaration */
fdecl:
  /* func with args */
  | vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE 
  {
    { 
      rtyp = fst $1;
      fname = snd $1;
      formals = $3;
      locals = $6;
      body = $7;
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }


stmt:
  | expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | RETURN expr SEMI                        { Return $2      }

expr:
  /* literal */
  | LIT_STR { StrLit($1) }
  | LIT_INT { IntLit($1) }
  | LIT_BOOL { BoolLit($1) }
  | LIT_FLOAT { FloatLit($1) }

  /* variable */
  | ID { Id($1) }

  /* arithmetic */
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }

  /* logical binary ops */
  | expr  EQ    expr { Binop($1, Eq, $3)   }
  | expr  NEQ   expr { Binop($1, Neq,   $3)   }
  | expr  LT    expr { Binop($1, Less,  $3)   }

  /* logical ops */
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }

  /* assignment */
  | ID ASSIGN expr { Assign($1, $3) }

  /* remove clarifying parens */
  | LPAREN expr RPAREN { $2 } /* (expr) -> expr. get rid of parens */

  /* function call */
  | ID LPAREN args_opt RPAREN { FuncCall($1, $3) }

  

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }