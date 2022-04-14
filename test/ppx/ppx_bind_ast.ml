open Ppx_ast_utils  

let %test "int bind" =
parse_bind "@@x: int;"
= (IntType, "x")

let %test "string bind" =
parse_bind "@@y: string;"
= (StrType, "y")

let %test "float bind" =
parse_bind "@@z: float;"
= (FloatType, "z")

let %test "bool bind" =
parse_bind "@@k: bool;"
= (BoolType, "k")
  