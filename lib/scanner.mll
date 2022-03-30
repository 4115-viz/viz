{
open Parser
exception Viz_scan_error of string


(* borrowed print test format from ctex *)
let print_token t =
    match t with
      (* identifier *)
      | ID(s) -> Printf.printf "ID(%s)\n" s
      | FUNC -> Printf.printf "FUNC\n"
      | _ -> Printf.printf "Unimplemented\n"

(* nanoc scanner print code for testing *)
(* type tokenseq = string list *)
let string_of_program_tokens l =
  "\n\nScanned program: \n" ^ (List.fold_left (fun s e -> s ^ "\n" ^ e) "" l)
}


let digit  = ['0'-'9']
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
| letter (digit | letter | '_')* as lxm { ID(lxm) }
(*| "@" letter (digit | letter | '_')* as lxm { ID(String.sub lxm 1 ((String.length lxm) - 1)) }*)
| "0" | "-" ? ['1'-'9']+ ['0'-'9']*  as lxm {LIT_INT(int_of_string lxm)}
| '"' ([^ '"']* as lxm) '"' { LIT_STR(lxm) }
| "true" { LIT_BOOL(true) }
| "false" { LIT_BOOL(false) }
| eof { EOF }
| _ as char {raise (Viz_scan_error ("unexpected character: " ^ Char.escaped char))}

and single_comment = parse
 | '\n' {token lexbuf} (* this is how we will end a single line comment *)
 | _    {single_comment lexbuf} (* want to ignore the rest of the noise *) 
 
 and multi_comment = parse
 | "*/" {token lexbuf} (* end of multi line comment, head back to token *)
 | _    {multi_comment lexbuf} (* want to ignore the rest of the noise *)

(*)
(* get the characters from stdin *)
{
    (*)
    ( fun to_test -> 
        match to_test with 
        | true ->
            (
                let buf = Lexing.from_channel stdin in 
                let f = token buf in
                print_endline (print_token f)
            )
        | false -> ()
    ) *)
    (* borrowed print test format from ctex *)
    let print_token t =
        match t with
        (* identifier *)
        | ID(s) -> Printf.sprintf "ID(%s)" s
        | _ -> Printf.sprintf "Unimplemented"
} *)