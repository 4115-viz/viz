open Ast

type sexpr = builtin_type * sx
and sx =
  | SAssign of string * sexpr
  | SStrLit of string
  | SIntLit of int
  | SFloatLit of float
  | SBoolLit of bool
  | SId of string
  | SFuncCall of string * sexpr list
  | SBinop of sexpr * op * sexpr
  
type sstmt =
  | SExpr of sexpr
  | SFuncDecl of sfunc_decl
and sfunc_decl = {
  styp: builtin_type;
  sname: string;
  sparams: bind list;
  sbody: sstmt list;
}

type sprogram = sstmt list

(* ----- Print Function ----- *)
let fmt_typ = function
  | NoneType -> "Type(None)"
  | StrType -> "Type(Str)"
  | IntType -> "Type(Int)"
  | FloatType -> "Type(Float)"
  | BoolType -> "Type(Bool)"


let fmt_string x = String.concat "" ["\""; x; "\""]

let fmt_op = function
| Add -> "+"
| Sub -> "-"
| Mult -> "*"
| Div -> "/"
| Eq -> "=="
| Neq -> "!="
| Less -> "<"
| Great -> ">"
| Leq -> "<="
| Geq -> ">="

let rec fmt_sfcall name args = 
  "FuncCall(" ^
     "name: " ^ fmt_string name ^
     ", args: " ^ fmt_sexpr_list args ^
  ")"

and fmt_sexpr_list l = String.concat "\n" (List.map fmt_sexpr l)

and fmt_sexpr (t, se) =
  "(" ^ fmt_typ t ^ " : " ^ 
  (match se with
    | SStrLit(x) -> "StrLit(" ^ fmt_string x ^ ")"
    | SIntLit(x) -> "IntLit(" ^ fmt_string (string_of_int x) ^ ")"
    | SFloatLit(x) -> "FloatLit(" ^ fmt_string (string_of_float x) ^ ")"
    | SBoolLit(true) -> "BoolLit(true)"
    | SBoolLit(false) -> "BoolLit(false)"
    | SAssign(v, e) -> v ^ " = " ^ fmt_sexpr e
    | SId(x) -> "Id(" ^ x ^ ")"
    | SFuncCall(name, args) -> fmt_sfcall name args
    | SBinop(l, o, r) -> 
      fmt_sexpr l ^ " " ^ fmt_op o ^ " " ^ fmt_sexpr r 
  )
  ^ ")"

let rec fmt_sfdecl sfd = 
  "Function(" ^ 
    "name: " ^ fmt_string sfd.sname ^
    ", type: " ^ fmt_typ sfd.styp ^
  ")" ^ " {\n" ^
    fmt_sstmt_list sfd.sbody
  ^
  "\n}\n"

and fmt_sstmt = function
  | SExpr se -> "  " ^ fmt_sexpr se
  | SFuncDecl sfd -> fmt_sfdecl sfd

and fmt_sstmt_list l = String.concat "\n" (List.map fmt_sstmt l)

let string_of_sprogram sp = fmt_sstmt_list sp