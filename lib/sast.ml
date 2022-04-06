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
  | SBlock of sstmt list
  | SIf of sexpr * sstmt * sstmt
  (* | SWhile of sexpr * sstmt *)
  | SReturn of sexpr
  (*| SVarDecl of bind * sexpr option*)
  (*| SFuncDecl of sfunc_decl*) (* don't think this applies anymore *)

and sfunc_def = {
  styp: builtin_type;
  sname: string;
  sparams: bind list;
  sbody: sstmt list;
  slocals: bind list;
}

type sprogram = bind list * sfunc_def list

(* ----- Print Function ----- *)
let rec string_of_sexpr (t, se) =
  "(" ^ string_of_typ t ^ " : " ^ 
  (match se with
    | SStrLit(x) -> "StrLit(" ^ fmt_string x ^ ")"
    | SIntLit(x) -> "IntLit(" ^ string_of_int x ^ ")"
    | SFloatLit(x) -> "FloatLit(" ^ string_of_float x ^ ")"
    | SBoolLit(true) -> "BoolLit(true)"
    | SBoolLit(false) -> "BoolLit(false)"
    | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
    | SId(x) -> "Id(" ^ x ^ ")"
    | SFuncCall(name, args) -> fmt_sfcall name args
    | SBinop(l, bo, r) -> 
      string_of_sexpr l ^ " " ^ string_of_op bo ^ " " ^ string_of_sexpr r 
    | SUnop(uo, r) ->
        string_of_uop uo ^ " " ^ string_of_sexpr r
  )
  ^ ")"

and string_of_sstmt = function
  | SExpr se -> "  " ^ string_of_sexpr se
  | SBlock (sstmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt sstmts) ^ "}\n"
  | SReturn (sexpr) -> "return " ^ string_of_sexpr sexpr ^ ";\n" 
  | SIf (se, s1, s2) -> "if (" ^ string_of_sexpr se ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  (*| SVarDecl (_, _) -> "SVarDecl TODO"*)
  (*| SFuncDecl sfd -> fmt_sfdecl sfd *) (* don't think this applies anymore *)
  (*| While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s *)

and fmt_sfcall name args = 
  "FuncCall(" ^
     "name: " ^ fmt_string name ^
     ", args: " ^ fmt_sexpr_list args ^
  ")"

and fmt_sexpr_list l = String.concat "\n" (List.map string_of_sexpr l)


(* let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n" *)
(*let string_of_vdecl ((t, n), e) = *)
let string_of_svdecl (t, n) =
  "VarDecl(" ^
    "name: " ^ fmt_string n ^
    ", type: " ^ string_of_typ t ^"\n"(* ^
    match e with
    | None -> ""
    | Some(e) -> ", value: " ^ string_of_expr e *)

let string_of_sfdecl (fdecl:sfunc_def) =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sname ^ "(" ^ String.concat ", " (List.map snd fdecl.sparams) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_svdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

(*let rec fmt_fdecl fd =
    "Function(" ^ 
        "name: " ^ fmt_string fd.name ^
        ", type: " ^ string_of_typ fd.typ ^
      ")" ^ " {\n\t" ^
        fmt_stmt_list ~sp:"\n\t" fd.body
      ^
      "\n}\n"
  
and fmt_stmt_list ?(sp = "\n") l = String.concat sp (List.map string_of_stmt l)
*)

let string_of_sprogram (vars, funcs) =
  "\n\nSemantically checked program: \n\n" ^
  String.concat "" (List.map string_of_svdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)

(* let string_of_program p = fmt_stmt_list p *)
(*let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map fmt_fdecl funcs)
*)