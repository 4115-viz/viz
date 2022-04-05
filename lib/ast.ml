type bop = Add | Sub | Mult | Div | Eq | Neq | Less 
        | Great | Leq | Geq | And | Or | Mod

type uop = Not

type builtin_type = 
  | NoneType
  | StrType
  | IntType
  | BoolType
  | FloatType

type expr =
  | Assign of string * expr
  | IntLit of int
  | FloatLit of float
  | StrLit of string
  | BoolLit of bool
  | Id of string
  | FuncCall of string * expr list
  | Binop of expr * bop * expr
  | Unop of uop * expr

type bind = builtin_type * string

(* ----- Entry ----- *)
type program = stmt list

and stmt =
  | Expr of expr
  | VarDecl of bind * expr option
  | FuncDecl of func_decl
  | Block of stmt list
  | If of expr * stmt * stmt
  | Return of expr

and func_decl = {
  typ: builtin_type;
  name: string;
  params: bind list;
  body: stmt list;
}

(* ----- Print Function ----- *)
let fmt_typ = function
  | NoneType -> "Type(None)"
  | StrType -> "Type(Str)"
  | IntType -> "Type(Int)"
  | BoolType -> "Type(Bool)"
  | FloatType -> "Type(Float)"

let fmt_op = function
| Add -> "+"
| Sub -> "-"
| Mult -> "*"
| Div -> "/"
| Mod -> "%"
| Eq -> "=="
| Neq -> "!="
| Less -> "<"
| Great -> ">"
| Leq -> "<="
| Geq -> ">="
| And -> "and"
| Or  -> "or"

let fmt_uop = function
| Not -> "not"

let fmt_string x = String.concat "" ["\""; x; "\""]

let rec fmt_fcall name args = 
  "FuncCall(" ^
     "name: " ^ fmt_string name ^
     ", args: " ^ fmt_expr_list args ^
  ")"

and fmt_expr e = 
  "(" ^ 
  (match e with 
  | StrLit(x) -> "StrLit(" ^ fmt_string x ^ ")"
  | IntLit(x) -> "IntLit(" ^ fmt_string (string_of_int x) ^ ")"
  | FloatLit(x) -> "FloatLit(" ^ fmt_string (string_of_float x) ^ ")"
  | BoolLit(true) -> "BoolLit(true)"
  | BoolLit(false) -> "BoolLit(false)"
  | Assign(v, e) -> v ^ " = " ^ fmt_expr e
  | Id(x) -> "Id(" ^ x ^ ")"
  | FuncCall(name, args) -> fmt_fcall name args
  | Binop(l, bo, r) ->
    fmt_expr l ^ " " ^ fmt_op bo ^ " " ^ fmt_expr r
  | Unop(uo, r) ->
    fmt_uop uo ^ " " ^ fmt_expr r
  )
  ^ ")"
and fmt_expr_list l = String.concat "\n" (List.map fmt_expr l)

let fmt_vdecl ((t, n), e) =
  "VarDecl(" ^
    "name: " ^ fmt_string n ^
    ", type: " ^ fmt_typ t ^
    match e with
    | None -> ""
    | Some(e) -> ", value: " ^ fmt_expr e

let rec fmt_fdecl fd =
  "Function(" ^ 
      "name: " ^ fmt_string fd.name ^
      ", type: " ^ fmt_typ fd.typ ^
    ")" ^ " {\n\t" ^
      fmt_stmt_list ~sp:"\n\t" fd.body
    ^
    "\n}\n"

and fmt_stmt = function
  | Expr e -> "  " ^ fmt_expr e
  | VarDecl (b, e) -> fmt_vdecl (b, e)
  | FuncDecl fd -> fmt_fdecl fd
  | Block (stmts) ->
    "{\n" ^ String.concat "" (List.map fmt_stmt stmts) ^ "}\n"
  | Return (expr) -> "return " ^ fmt_expr expr ^ ";\n" 
  | If (e, s1, s2) -> "if (" ^ fmt_expr e ^ ")\n" ^
                        fmt_stmt s1 ^ "else\n" ^ fmt_stmt s2

and fmt_stmt_list ?(sp = "\n") l = String.concat sp (List.map fmt_stmt l)

let string_of_program p = fmt_stmt_list p