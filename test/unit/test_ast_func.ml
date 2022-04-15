open Ast_test_utils
open Lib
open Lib.Ast
open Alcotest

let func_def_testable = 
  let func_def_pp ppf fd = Fmt.pf ppf "%s" (Ast_fmt.string_of_fdecl fd) in
  let func_def_eq a b = (a = b) in
  testable func_def_pp func_def_eq

let same_func (fa : func_def) (fb : func_def) = 
  (check func_def_testable) "same func" fa fb

let test_func_return_none () =
  same_func
  (parse_function "func main(): none {}")
  {rtyp = NoneType; fname = "main"; formals = []; locals = []; body = []}

let test_func_return_int () = 
  same_func
  (parse_function "func main(): int {}")
  {rtyp = IntType; fname = "main"; formals = []; locals = []; body = []}

let test_func_return_string () = 
  same_func
  (parse_function "func main(): string {}")
  {rtyp = StrType; fname = "main"; formals = []; locals = []; body = []}

let test_func_return_float () = 
  same_func
  (parse_function "func main(): float {}")
  {rtyp = FloatType; fname = "main"; formals = []; locals = []; body = []}

let test_func_return_bool () = 
  same_func
  (parse_function "func main(): bool {}")
  {rtyp = BoolType; fname = "main"; formals = []; locals = []; body = []}

let () = 
  run "Should able to parse function declarations" [
    "function-return-type", [
      test_case "None Type" `Quick test_func_return_none;
      test_case "Int Type" `Quick test_func_return_int;
      test_case "String Type" `Quick test_func_return_string;
      test_case "Float Type" `Quick test_func_return_float;
      test_case "Boolean Type" `Quick test_func_return_bool;
    ]
  ]