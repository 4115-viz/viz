(* Viz Compiler *)
open Lib

type action = Ast | Sast | Compile | ScanTest | LLVM_IR

let () = 
	let action = ref Compile in
	let set_action a () = action := a in
	let speclist = [
		("-a", Arg.Unit (set_action Ast), "Print the AST");
		("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
		("-ts", Arg.Unit (set_action ScanTest), "Print the Scanned Tokens");
	] in  
	let usage_msg = "usage: vc [-a|-s|-ts] <file.viz>" in
	let channel = ref stdin in
	Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

	let lexbuf = Lexing.from_channel !channel in
	let ast = Parser.program Scanner.token lexbuf in
	match !action with
	| ScanTest -> print_endline (Token_fmt.string_of_lexbuf lexbuf)
	| Ast -> print_endline (Ast_fmt.string_of_program ast)
	| _ -> let sast = Semant.check ast in
		match !action with
			Ast -> ()
		| ScanTest -> ()
		| Sast -> print_endline  (Sast_fmt.string_of_sprogram sast)
		| LLVM_IR -> print_string (Llvm.string_of_llmodule (Irgen.translate sast))
		| Compile -> let m = Irgen.translate sast in
			Llvm_analysis.assert_valid_module m;
			print_string (Llvm.string_of_llmodule m)
			