type op = Add | Sub

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
  | AssignAndInit of builtin_type * string * expr
  | Binop of expr * op * expr

type bind = builtin_type * string * expr

type stmt =
  | Block of stmt list
  | Expr of expr
  | Return of expr

type func_decl = {
    typ: builtin_type;
    name: string;
    params: bind list;
    (* locals: bind list; *)
    body: stmt list;
}

(* ----- Entry ----- *)
type program = bind list * func_decl list

(* ----- Print Function ----- *)
let fmt_typ = function
  | NoneType -> "Type(None)"
  | StrType -> "Type(Str)"
  | IntType -> "Type(Int)"
  | BoolType -> "Type(Bool)"

let fmt_string x = String.concat "" ["\""; x; "\""]

let string_of_typ = function
    IntType -> "int"
  | BoolType -> "bool"
  | NoneType -> "none"
  | StrType -> "string"

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
  | AssignAndInit(t, v, e) -> "AssignAndInit(" ^ v ^ ": " ^ string_of_typ t ^ fmt_expr e ^ ")"
  | Id(x) -> "Id(" ^ x ^ ")"
  | FuncCall(name, args) -> fmt_fcall name args 
  | Binop (_, _, _) -> "in fmt_expr"

and fmt_expr_list l = String.concat "\n" (List.map fmt_expr l)

(* let rec fmt_fdecl fd =
  "Function(" ^ 
      "name: " ^ fmt_string fd.name ^
      ", type: " ^ fmt_typ fd.typ ^
    ")" ^ " {\n" ^ "  " ^
      fmt_stmt_list fd.body
    ^
    "\n}\n" *)

and fmt_stmt = function
  Block(stmts) ->
      "{\n" ^ String.concat "" (List.map fmt_stmt stmts) ^ "}\n"
  | Expr e -> fmt_expr e
  (* | FuncDecl fd -> fmt_fdecl fd *)
  | Return(expr) -> "return " ^ fmt_expr expr ^ ";\n"

and fmt_stmt_list l = String.concat "\n" (List.map fmt_stmt l)

let rec string_of_expr = function
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | AssignAndInit(t, v, e) -> v ^ ": " ^ string_of_typ t ^ fmt_expr e
  | IntLit(l) -> string_of_int l
  | StrLit(s) -> s
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | FuncCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Binop (_, _, _) -> "this is in string_of_expr"

  (* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"

let string_of_vdecl (t, id, e) = id ^ ": " ^ string_of_typ t ^ " = " ^ fmt_expr e ^ ";\n"

let string_of_fdecl fdecl =
  (* "func " ^ fdecl.name ^ "(" ^ String.concat ", " (List.map snd fdecl.params) ^ *)
  "func " ^ fdecl.name ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.params) ^
  "): " ^ string_of_typ fdecl.typ ^ " " ^ "\n{\n" ^
  (* String.concat "" (List.map string_of_vdecl fdecl.locals) ^ *)
  String.concat "" (List.map fmt_stmt fdecl.body) ^
  "\n}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)