(* open Lib

let program_to_ast program_str = 
let lexbuf = Lexing.from_string program_str in
Parser.program Scanner.token lexbuf

let%test _ = 
program_to_ast "int main(){}"
=  *)
