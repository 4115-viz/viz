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
  | SBinop of sexpr * bop * sexpr
  | SUnop of uop * sexpr

  type sstmt =
  | SExpr of sexpr
  | SVarDecl of bind * sexpr option
  | SFuncDecl of sfunc_decl
  | SBlock of sstmt list
  | SIf of sexpr * sstmt * sstmt
  | SReturn of sexpr

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
    | SBinop(l, bo, r) -> 
      fmt_sexpr l ^ " " ^ fmt_op bo ^ " " ^ fmt_sexpr r 
    | SUnop(uo, r) ->
        fmt_uop uo ^ " " ^ fmt_sexpr r
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
  | SVarDecl (_, _) -> "SVarDecl TODO"
  | SFuncDecl sfd -> fmt_sfdecl sfd
  | SBlock (stmts) ->
    "{\n" ^ String.concat "" (List.map fmt_sstmt stmts) ^ "}\n"
  | SReturn (expr) -> "return " ^ fmt_sexpr expr ^ ";\n" 
  | SIf (e, s1, s2) -> "if (" ^ fmt_sexpr e ^ ")\n" ^
                        fmt_sstmt s1 ^ "else\n" ^ fmt_sstmt s2
and fmt_sstmt_list l = String.concat "\n" (List.map fmt_sstmt l)

let string_of_sprogram sp = fmt_sstmt_list sp