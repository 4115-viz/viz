open Ast_test_utils
open Lib
open Lib.Ast
open Alcotest
let bind_testable =
  let pp ppf bind = Fmt.pf ppf "%s" (Ast_fmt.string_of_vdecl bind) in
  let eq a b = (a = b) in
  testable pp eq

let test_bind_int () =
  (check bind_testable) "same bind"
  (parse_bind "@@x: int;")
  (IntType, "x")

let () = 
  run "Should able to parse bind" [
    "single-bind", [
      test_case "Bind with IntType" `Quick test_bind_int;
    ];
  ]