(* Viz Compiler *)
open Lib

type action = Ast | Sast | Compile | ScanTest

let () = 
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-ts", Arg.Unit (set_action ScanTest), "Print the Scanned Tokens");
  ] in  
  let usage_msg = "usage: vc [-a|-s|-ts] <file.viz>" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  (* match !action with
  | ScanTest -> let lexbuf = Lexing.from_channel !channel in
                (* lexm.sh via CTeX group project 4115*)
                let token_list =
                  let rec next l =
                      match Scanner.token lexbuf with
                          EOF -> l
                      | x -> next (x :: l)
                  in List.rev (next []) in
                List.iter Scanner.print_token token_list
  | Ast ->
    let lexbuf = Lexing.from_channel !channel in
    let ast = Parser.program Scanner.token lexbuf in
    print_endline (Ast.string_of_program ast)
  | _ -> 
    let lexbuf = Lexing.from_channel !channel in
    let ast = Parser.program Scanner.token lexbuf in
    let sast = Semant.check ast in
    match !action with
      Ast -> ()
    | _ -> print_string (Sast.string_of_sprogram sast) *)

    let lexbuf = Lexing.from_channel !channel in

  let ast = Parser.program Scanner.token lexbuf in
  match !action with
    Ast -> print_string (Ast.string_of_program ast)
  | _ -> let sast = Semant.check ast in
    match !action with
      Ast     -> ()
    | _    -> print_string (Sast.string_of_sprogram sast)
    (* | LLVM_IR -> print_string (Llvm.string_of_llmodule (Irgen.translate sast)) *)