type builtin_type = 
  | NoneType
  | StrType
  | IntType
  | BoolType
type expr =
  | Assign of string * expr
  | IntLit of int
  | StrLit of string
  | BoolLit of bool
  | Id of string
  | FuncCall of string * expr list

type bind = builtin_type * string

and stmt =
  | Expr of expr
  | FuncDecl of func_decl

and func_decl = {
  typ: builtin_type;
  name: string;
  params: bind list;
  locals: bind list;
  body: stmt list;
}

(* ----- Entry ----- *)
type program = stmt list

(* ----- Print Function ----- *)
let fmt_typ = function
  | NoneType -> "Type(None)"
  | StrType -> "Type(Str)"
  | IntType -> "Type(Int)"
  | BoolType -> "Type(Bool)"

let fmt_string x = String.concat "" ["\""; x; "\""]

let rec fmt_fcall name args = 
  "FuncCall(" ^
     "name: " ^ fmt_string name ^
     ", args: " ^ fmt_expr_list args ^
  ")"

and fmt_expr = function
  | StrLit(x) -> "StrLit(" ^ fmt_string x ^ ")"
  | IntLit(x) -> "IntLit(" ^ fmt_string (string_of_int x) ^ ")"
  | BoolLit(true) -> "BoolLit(true)"
  | BoolLit(false) -> "BoolLit(false)"
  | Assign(v, e) -> v ^ " = " ^ fmt_expr e
  | Id(x) -> "Id(" ^ x ^ ")"
  | FuncCall(name, args) -> fmt_fcall name args 

and fmt_expr_list l = String.concat "\n" (List.map fmt_expr l)

let rec fmt_fdecl fd =
  "Function(" ^ 
      "name: " ^ fmt_string fd.name ^
      ", type: " ^ fmt_typ fd.typ ^
    ")" ^ " {\n" ^ "  " ^
      fmt_stmt_list fd.body
    ^
    "\n}\n"

and fmt_stmt = function
  | Expr e -> fmt_expr e
  | FuncDecl fd -> fmt_fdecl fd

and fmt_stmt_list l = String.concat "\n" (List.map fmt_stmt l)

let string_of_program p = fmt_stmt_list p