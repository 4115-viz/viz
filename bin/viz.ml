open Lib

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let tokenseq = Parser.program Scanner.token lexbuf in
  print_endline (Ast.string_of_program tokenseq)