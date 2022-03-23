(* Viz Compiler *)
open Lib

type action = Ast | Sast | Compile

let () = 
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
  ] in  
  let usage_msg = "usage: vc [-a|-s] <file.viz>" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  match !action with
    Ast -> print_endline (Ast.string_of_program ast)
  | _ -> 
    let sast = Semant.check_program ast in
    match !action with
      Ast -> ()
    | _ -> print_string (Sast.string_of_sprogram sast)