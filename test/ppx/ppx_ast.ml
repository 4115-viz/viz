open Lib
open Ast

let parse_program program_str = 
let lexbuf = Lexing.from_string program_str in
Parser.program Scanner.token lexbuf

(* Parse a single function declaration *)
let parse_function (func_str : string) : func_def =
List.hd (snd (parse_program func_str))

let%test "func return int" = 
parse_function "func main(): int {}"
= {rtyp = IntType; fname = "main"; formals = []; locals = []; body = []}

let%test "func return none" = 
parse_function "func main(): none {}"
= {rtyp = NoneType; fname = "main"; formals = []; locals = []; body = []}

let%test "func return string" = 
parse_function "func main(): string {}"
= {rtyp = StrType; fname = "main"; formals = []; locals = []; body = []}

let%test "func return float" = 
parse_function "func main(): float {}"
= {rtyp = FloatType; fname = "main"; formals = []; locals = []; body = []}
