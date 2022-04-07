type bop = Add | Sub | Eq | Neq | Less | And | Or
          | Mult | Div | Great | Leq | Geq | Mod

type uop = Not

type typ = 
  | NoneType
  | StrType
  | IntType
  | BoolType
  | FloatType

type expr =
  | StrLit of string
  | IntLit of int
  | FloatLit of float
  | BoolLit of bool

  | Id of string

  | Assign of string * expr
  | FuncCall of string * expr list
  | Binop of expr * bop * expr
  | Unop of uop * expr

type stmt =
  | Expr of expr
  | Block of stmt list
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Return of expr

type bind = typ * string

type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

(* ----- Entry ----- *)
type program = bind list * func_def list

(* ----- Print Function ----- *)
let string_of_op = function
| Add   -> "+"
| Sub   -> "-"
| Mult  -> "*"
| Div   -> "/"
| Mod   -> "%"
| Eq    -> "=="
| Neq   -> "!="
| Less  -> "<"
| Great -> ">"
| Leq   -> "<="
| Geq   -> ">="
| And   -> "and"
| Or    -> "or"

let string_of_typ = function
  | NoneType  -> "Type(None)"
  | StrType   -> "Type(Str)"
  | IntType   -> "Type(Int)"
  | BoolType  -> "Type(Bool)"
  | FloatType -> "Type(Float)"

let string_of_uop = function
| Not -> "not"

let fmt_string x = String.concat "" ["\""; x; "\""]

let rec string_of_expr = function
  | StrLit(x) -> "StrLit(" ^ fmt_string x ^ ")"
  | IntLit(x) -> "IntLit(" ^ string_of_int x ^ ")"
  | FloatLit(x) -> "FloatLit(" ^ string_of_float x ^ ")"
  | BoolLit(true) -> "BoolLit(true)"
  | BoolLit(false) -> "BoolLit(false)"

  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Id(x) -> "Id(" ^ x ^ ")"
  | FuncCall(name, args) ->
    (*name ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"*)
    fmt_fcall name args
  | Binop(l, bo, r) ->
    string_of_expr l ^ " " ^ string_of_op bo ^ " " ^ string_of_expr r
  | Unop(uo, r) ->
    string_of_uop uo ^ " " ^ string_of_expr r

and fmt_fcall name args = 
  "FuncCall(" ^
     "name: " ^ fmt_string name ^
     ", args: " ^ fmt_expr_list args ^
  ")"
  
and fmt_expr_list l = String.concat "\n" (List.map string_of_expr l)


and string_of_stmt = function

  | Expr e -> "  " ^ string_of_expr e
  | Block (stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Return (expr) -> "return " ^ string_of_expr expr ^ ";\n" 
  | If (e, s1, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^
                        string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s



(*let string_of_vdecl ((t, n), e) = *)
let string_of_vdecl (t, n) =
  "VarDecl(" ^
    "name: " ^ fmt_string n ^
    ", type: " ^ string_of_typ t ^"\n"(* ^
    match e with
    | None -> ""
    | Some(e) -> ", value: " ^ string_of_expr e *)

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
