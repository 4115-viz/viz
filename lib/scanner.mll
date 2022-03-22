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

(* -------- arithmetic operators -------- *)

(* -------- assignment operators -------- *)

(* -------- relational operators -------- *)

(* -------- delimiters -------- *)
| "("  { LPAREN }
| ")"  { RPAREN }
| "{" { LBRACE }
| "}" { RBRACE }
| ":" { COLON }
| ";"  { SEMI }

(* -------- literals -------- *)
| letter (digit | letter | '_')* as lxm { ID(lxm) }
| '"' ([^ '"']* as lxm) '"' { LIT_STR(lxm) }
| eof { EOF }
| _ as char { raise (Failure("unexpected character: " ^ Char.escaped char)) }