open Ast

type sexpr = builtin_type * sx
and sx =
  | SAssign of string * sexpr
  | SStrLit of string
  | SIntLit of int
  | SBoolLit of bool
  | SId of string
  | SFuncCall of string * sexpr list
  
type sstmt =
  | SBlock of sstmt list
  | SExpr of sexpr
  (* | SFuncDecl of sfunc_decl *)
  | SReturn of sexpr

type sfunc_decl = {
  styp: builtin_type;
  sname: string;
  sparams: bind list;
  (* slocals: bind list; *)
  sbody: sstmt list;
}

type sprogram = bind list * sfunc_decl list

(* ----- Print Function ----- *)
let fmt_typ = function
  | NoneType -> "Type(None)"
  | StrType -> "Type(Str)"
  | IntType -> "Type(Int)"
  | BoolType -> "Type(Bool)"


let fmt_string x = String.concat "" ["\""; x; "\""]

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
    | SBoolLit(true) -> "BoolLit(true)"
    | SBoolLit(false) -> "BoolLit(false)"
    | SAssign(v, e) -> v ^ " = " ^ fmt_sexpr e
    | SId(x) -> "Id(" ^ x ^ ")"
    | SFuncCall(name, args) -> fmt_sfcall name args 
  )

(* let rec fmt_sfdecl sfd = 
  "Function(" ^ 
    "name: " ^ fmt_string sfd.sname ^
    ", type: " ^ fmt_typ sfd.styp ^
  ")" ^ " {\n" ^ "  " ^
    fmt_sstmt_list sfd.sbody
  ^
  "\n}\n" *)

and fmt_sstmt = function
  SBlock(sstmts) ->
      "{\n" ^ String.concat "" (List.map fmt_sstmt sstmts) ^ "}\n"
  | SExpr se -> fmt_sexpr se
  (* | SFuncDecl sfd -> fmt_sfdecl sfd *)
  | SReturn(sexpr) -> "return " ^ fmt_sexpr sexpr ^ ";\n"

and fmt_sstmt_list l = String.concat "\n" (List.map fmt_sstmt l)

(* let string_of_sprogram sp = fmt_sstmt_list sp *)

let string_of_svdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_sfdecl sfdecl =
  "func " ^ sfdecl.sname ^ "(" ^ String.concat ", " (List.map snd sfdecl.sparams) ^
  "): " ^ string_of_typ sfdecl.styp ^ " " ^ "\n{\n" ^
  (* String.concat "" (List.map string_of_vdecl sfdecl.slocals) ^ *)
  String.concat "" (List.map fmt_sstmt sfdecl.sbody) ^
  "\n}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_svdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)