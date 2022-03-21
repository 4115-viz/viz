type typ =
  | NoneType
  | IntegerType
  | FloatType
  | BoolType
  | StringType

type bind = typ * string

type expr =
  | BoolLit of bool
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | Id of string
  | Assign of string * expr
  | FuncCall of string * expr list

type func_decl = {
  func_name: string;
  func_type: typ;
  params: bind list;
  body: stmt list
}

and stmt =
  | Expr of expr
  | FuncDecl of func_decl

type program = stmt list

let string_of_expr = function
  | FuncCall(name, _) -> name ^ " = ";
  | _ -> ""

let string_of_stmt = function
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | _ -> "other"

let string_of_program program =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_stmt program) ^
  "\n"