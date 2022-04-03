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
%token <string> ID_FUNC 
%token <string> ID_VAR /* split so variables could have @ */
%token <string> LIT_STR
%token <int> LIT_INT
%token <float> LIT_FLOAT
%token <bool> LIT_BOOL

/* precedence following C standard*/
%left COMMA
%left SEMI 
%right ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVEQ MODEQ
%left OR
%left AND
%left EQ NEQ
%left LT GT LTEQ GTEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT
/* array subscripting, function call, member access (if needed) */
%left LBRACKET RBRACKET LPAREN RPAREN DOT

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

/* somewhere here in expr, we need to handle parentheses */
expr:
  /* literal */
  | LIT_STR { StrLit($1) }
  | LIT_INT { IntLit($1) }
  | LIT_BOOL { BoolLit($1) }
  | LIT_FLOAT { FloatLit($1) }

  /* variable */
  | ID_VAR { Id($1) }

  /* arithmetic */
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr MOD    expr { Binop($1, Div,   $3)   }
  
  /* logical binary ops */
  | expr  EQ    expr { Binop($1, Eq, $3)   }
  | expr  NEQ   expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr GT     expr { Binop($1, Great, $3) }
  | expr LTEQ    expr { Binop($1, Leq,   $3)   }
  | expr GTEQ    expr { Binop($1, Geq,   $3)   }
  
  /* logical ops */
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | NOT expr { Unop(Not, $2) }

  /* Note: I can see us doing other unary ops like ++ -- and more
     would just need to add it here */

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
