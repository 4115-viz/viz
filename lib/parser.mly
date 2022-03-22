/*type*/
%token T_INT T_NONE

/*delimiters*/
%token SEMI LPAREN RPAREN LBRACE RBRACE COLON
%token EOF

%token <string> ID
%token <string> STRING_LITERAL

%start program
%type <Ast.tokenseq> program

%%

program:
  tokens EOF { $1}

tokens:
   /* nothing */ { [] }
 | one_token tokens { $1 :: $2 }

one_token:
  | SEMI  {  "SEMI" }
  | T_INT { "Type: INT" }
  | T_NONE { "Type: None" }
  | COLON { "COLON" }
  | LPAREN { "LPAREN" }
  | RPAREN { "RPAREN" }
  | LBRACE { "LBRACE" }
  | RBRACE { "RBRACE" }
  | ID { "ID: " ^ $1 }
  | STRING_LITERAL {"String: " ^ $1}
