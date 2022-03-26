{
open Parser
}

let digit  = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
(* -------- whitespaces -------- *)
| [' ' '\t' '\r' '\n'] { token lexbuf}

(* -------- keywords -------- *)
| "func" { FUNC }

(* -------- types -------- *)
| "none" { T_NONE }
| "int" { T_INT }
| "string" { T_STR }
| "bool" { T_BOOL }

(* -------- arithmetic operators -------- *)
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }

(* -------- assignment operators -------- *)
| '=' { ASSIGN }

(* -------- relational operators -------- *)

(* -------- delimiters -------- *)
| "("  { LPAREN }
| ")"  { RPAREN }
| "{" { LBRACE }
| "}" { RBRACE }
| ":" { COLON }
| ";" { SEMI }
| "," { COMMA }

(* -------- literals -------- *)
| letter (digit | letter | '_')* as lxm { ID(lxm) }
| digit+ as lxm {LIT_INT(int_of_string lxm)}
| '"' ([^ '"']* as lxm) '"' { LIT_STR(lxm) }
| "true" { LIT_BOOL(true) }
| "false" { LIT_BOOL(false) }
| eof { EOF }
| _ as char { raise (Failure("unexpected character: " ^ Char.escaped char)) }