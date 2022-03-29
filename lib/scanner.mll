{
open Parser
exception Viz_scan_error of string

(* borrowed print test format from ctex *)
(* this code will be called with dune -ts flag, for ensuring we scan correct program *)
let print_token t =
    match t with
      (* identifier *)
      | ID_VAR(s)  -> Printf.printf "ID_VAR(%s)\n" s
      | ID_FUNC(s)  -> Printf.printf "ID_FUNC(%s)\n" s
      | LIT_BOOL(b) -> Printf.printf "LIT_BOOL(%B)\n" b
      | LIT_STR(s) -> Printf.printf "LIT_STR(%s)\n" s
      | LIT_INT(i) -> Printf.printf "LIT_INT(%d)\n" i
      | LIT_FLOAT(fl) -> Printf.printf "LIT_FLOAT(%f)\n" fl
      | FUNC   -> Printf.printf "FUNC\n"
      | COLON  -> Printf.printf "COLON\n"
      | T_NONE -> Printf.printf "T_NONE\n"
      | T_BOOL -> Printf.printf "T_BOOL\n"
      | T_STR  -> Printf.printf "T_STR\n"
      | T_INT  -> Printf.printf "T_INT\n"
      | LBRACE -> Printf.printf "LBRACE\n"
      | RBRACE -> Printf.printf "RBRACE\n"
      | LPAREN -> Printf.printf "LPAREN\n"
      | RPAREN -> Printf.printf "RPAREN\n"
      | SEMI   -> Printf.printf "SEMI\n"
      | PLUS   -> Printf.printf "PLUS\n"
      | MINUS  -> Printf.printf "MINUS\n"
      | DIVIDE -> Printf.printf "DIVIDE\n"
      | TIMES  -> Printf.printf "TIMES\n"
      | ASSIGN -> Printf.printf "ASSIGN\n"
      | COMMA  -> Printf.printf "COMMA\n"
      | _ -> Printf.printf "Unimplemented\n"

}

(* everything below actually creates tokens *)
let digit  = ['0'-'9']
let non_zero_digits = ['1'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
(* -------- whitespaces -------- *)
| [' ' '\t' '\r' '\n'] { token lexbuf}
| "/*" {multi_comment lexbuf}
| "//" {single_comment lexbuf}

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
| "true" { LIT_BOOL(true) }
| "false" { LIT_BOOL(false) }
| '"' ([^ '"']* as lxm) '"' { LIT_STR(lxm) }
| '0'+ | "-" ? non_zero_digits digit*  as lxm {LIT_INT(int_of_string lxm)}
| '0'* "." '0'+ | "-" ? (non_zero_digits+ "." digit+ | "." digit+)  as lxm {LIT_FLOAT(float_of_string lxm)}

(* --------- IDs ------------ *)
| letter (digit | letter | '_')* as lxm { ID_FUNC(lxm) } (* function names dont need @ *)
| "@" letter (digit | letter | '_')* as lxm { ID_VAR(String.sub lxm 1 ((String.length lxm) - 1)) } (* variable names need @ *)

| eof { EOF }
| _ as char {raise (Viz_scan_error ("unexpected character: " ^ Char.escaped char))}

and single_comment = parse
 | '\n' {token lexbuf} (* this is how we will end a single line comment *)
 | _    {single_comment lexbuf} (* want to ignore the rest of the noise *) 
 
 and multi_comment = parse
 | "*/" {token lexbuf} (* end of multi line comment, head back to token *)
 | _    {multi_comment lexbuf} (* want to ignore the rest of the noise *)
