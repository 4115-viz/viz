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

	match !action with
	| ScanTest -> 
		let lexbuf = Lexing.from_channel !channel in
		print_endline (Token_fmt.string_of_lexbuf lexbuf)
	| Ast ->
		let lexbuf = Lexing.from_channel !channel in
		let ast = Parser.program Scanner.token lexbuf in
		print_endline (Ast_fmt.string_of_program ast)
	| Sast -> 
		let lexbuf = Lexing.from_channel !channel in
		let ast = Parser.program Scanner.token lexbuf in
		let sast = Semant.check ast in
		print_endline  (Sast_fmt.string_of_sprogram sast)
	| Compile -> 
		let lexbuf = Lexing.from_channel !channel in
		let ast = Parser.program Scanner.token lexbuf in
		 (* _ in front of name allows us to suppress the fact we are
				not using the _sast variable currently.
		 *)
		let _sast = Semant.check ast in
		()
		