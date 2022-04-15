open Lib
open Ast

let parse_program (program : string) : program = 
let lexbuf = Lexing.from_string program in
Parser.program Scanner.token lexbuf

(* Parse a single function declaration *)
let parse_function (func_str : string) : func_def =
  List.hd (snd (parse_program func_str))
  
let parse_bind (bind : string) : bind =
  List.hd (fst (parse_program bind))  