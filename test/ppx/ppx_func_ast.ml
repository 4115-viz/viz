open Ppx_ast_utils
open Lib.Ast

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

let%test "multiple functions" =
parse_program "func main1(): none {} func main2(): none {}"
= ([],[
  {rtyp = NoneType; fname = "main1"; formals = []; locals = []; body = []};
  {rtyp = NoneType; fname = "main2"; formals = []; locals = []; body = []}])

let%test "func with formals" = 
parse_function "func main(@@x: int, @@y: string): none {}"
= {rtyp = FloatType; fname = "main"; formals = [(IntType, "x"); (StrType, "y")]; locals = []; body = []}