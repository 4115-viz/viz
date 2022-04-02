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
        | T_FLOAT  -> Printf.printf "T_FLOAT\n"
        | LBRACE -> Printf.printf "LBRACE\n"
        | RBRACE -> Printf.printf "RBRACE\n"
        | LPAREN -> Printf.printf "LPAREN\n"
        | RPAREN -> Printf.printf "RPAREN\n"
        | LBRACKET -> Printf.printf "LBRACKET\n"
        | RBRACKET -> Printf.printf "RBRACKET\n"
        | DOT  -> Printf.printf "DOT\n"
        | SEMI   -> Printf.printf "SEMI\n"
        | PLUS   -> Printf.printf "PLUS\n"
        | MINUS  -> Printf.printf "MINUS\n"
        | DIVIDE -> Printf.printf "DIVIDE\n"
        | MOD -> Printf.printf "MOD\n"
        | TIMES  -> Printf.printf "TIMES\n"
        | ASSIGN -> Printf.printf "ASSIGN\n"
        | COMMA  -> Printf.printf "COMMA\n"
        | IF -> Printf.printf "IF\n"
        | ELSE -> Printf.printf "ELSE\n"
        | ELIF -> Printf.printf "ELIF\n"
        | FOR -> Printf.printf "FOR\n"
        | WHILE -> Printf.printf "WHILE\n"
        | INFINITE_LOOP -> Printf.printf "INFINITE_LOOP\n"
        | RETURN -> Printf.printf "RETURN\n"
        | BREAK -> Printf.printf "BREAK\n"
        | CONTINUE -> Printf.printf "CONTINUE\n"
        | TRY -> Printf.printf "TRY\n"
        | CATCH -> Printf.printf "CATCH\n"
        | RAISE -> Printf.printf "RAISE\n" (* i guess we will also need exception right? *)
        | LINK -> Printf.printf "LINK\n"
        | USE -> Printf.printf "USE\n"
        | IN -> Printf.printf "IN\n"
        | STEP -> Printf.printf "STEP\n"
        | AS -> Printf.printf "AS\n"
        | EQ -> Printf.printf "EQ\n"
        | NEQ -> Printf.printf "NEQ\n"
        | GTEQ -> Printf.printf "GTEQ\n"
        | LTEQ -> Printf.printf "LTEQ\n"
        | GT -> Printf.printf "GT\n"
        | LT -> Printf.printf "LT\n"
        | AND -> Printf.printf "AND\n"
        | OR -> Printf.printf "OR\n"
        | NOT -> Printf.printf "NOT\n"
        | PLUSEQ -> Printf.printf "PLUSEQ\n"
        | MINUSEQ -> Printf.printf "MINUSEQ\n"
        | TIMESEQ -> Printf.printf "TIMESEQ\n"
        | DIVEQ -> Printf.printf "DIVEQ\n"
        | MODEQ -> Printf.printf "MODEQ\n"       
      | _ -> Printf.printf "Unimplemented\n"

}

(* 
    actual scanner part
    everything below actually creates tokens 
*)
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
| "if"   { IF }
| "else" { ELSE }
| "elif" { ELIF }
| "for" { FOR }
| "while" { WHILE }
| "infinite_loop" { INFINITE_LOOP }
| "return" {RETURN}
| "break" {BREAK}
| "continue" {CONTINUE}
| "try" {TRY}
| "catch" {CATCH}
| "raise" {RAISE} (* i guess we will also need exception right? *)
| "link" {LINK}
| "use" {USE}
| "in" {IN}
| "step" {STEP}
| "as" {AS}

(* -------- types -------- *)
| "none" { T_NONE }
| "int" { T_INT }
| "string" { T_STR }
| "bool" { T_BOOL }
| "float" { T_FLOAT }

(* -------- arithmetic operators -------- *)
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '%' { MOD }

(* -------- assignment operators -------- *)
| '=' { ASSIGN }
| "+=" {PLUSEQ}
| "-=" {MINUSEQ}
| "*=" {TIMESEQ}
| "/=" {DIVEQ}
| "%=" {MODEQ}

(* -------- relational operators -------- *)
| "==" {EQ}
| "!=" {NEQ}
| ">=" {GTEQ}
| "<=" {LTEQ}
| ">" {GT}
| "<" {LT}
| "and" {AND}
| "or" {OR}
| "not" {NOT}

(* -------- literals -------- *)
| "true" { LIT_BOOL(true) }
| "false" { LIT_BOOL(false) }
| '"' ([^ '"']* as lxm) '"' { LIT_STR(lxm) }
| '0'+ | "-" ? non_zero_digits digit*  as lxm {LIT_INT(int_of_string lxm)}
| '0'* "." '0'+ | "-" ? (non_zero_digits+ "." digit+ | "." digit+)  as lxm {LIT_FLOAT(float_of_string lxm)}

(* -------- delimiters -------- *)
| "("  { LPAREN }
| ")"  { RPAREN }
| "["  { LBRACKET }
| "]"  { RBRACKET }
| "{" { LBRACE }
| "}" { RBRACE }
| ":" { COLON }
| ";" { SEMI }
| "," { COMMA }
| "." { DOT }

(* --------- IDs ------------ *)
| letter (digit | letter | '_')* as lxm { ID_FUNC(lxm) } (* function names dont need @ *)
| "@" letter (digit | letter | '_')* as lxm { ID_VAR(String.sub lxm 1 ((String.length lxm) - 1)) } (* variable names need @ *)

(* -------- Other ----------- *)
| eof { EOF }
| _ as char {raise (Viz_scan_error ("unexpected character: " ^ Char.escaped char))}

and single_comment = parse
 | '\n' {token lexbuf} (* this is how we will end a single line comment *)
 | _    {single_comment lexbuf} (* want to ignore the rest of the noise *) 
 
 and multi_comment = parse
 | "*/" {token lexbuf} (* end of multi line comment, head back to token *)
 | "/*" {raise (Viz_scan_error ("cannot nest multi-line comments"))} (* no nested comments *)
 | _    {multi_comment lexbuf} (* want to ignore the rest of the noise *)
