type bop = Add | Sub | Equal | Neq | Less | And | Or

type typ = Int | Bool | Float | String | None

type bind = typ * string

type expr =
  | BoolLit of bool
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | Id of string
  | Assign of string * expr
  | Binop of expr * bop * expr
  | FuncCall of string * expr list

type func_decl = {
  func_name: string;
  func_type: typ;
  params: bind list;
  body: stmt list
}

and stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | Elif of expr * stmt * stmt
  | Else of expr * stmt
  | While of expr * stmt
  | INFINITE_LOOP of expr * stmt
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
