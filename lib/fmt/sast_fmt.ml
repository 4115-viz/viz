open Sast
open Ast_fmt

let rec fmt_sexpr (t, se) =
  "(" ^ fmt_typ t ^ " : " ^ 
  if t == StrType then "(" ^ 
      fmt_sx se
    ^ ")"
  else
    fmt_sx se 
  ^ ")"

and fmt_sx se =
      (match se with
      | SStrLit(x) -> "StrLit(" ^ fmt_string x ^ ")"
      | SIntLit(x) -> "IntLit(" ^ string_of_int x ^ ")"
      | SFloatLit(x) -> "FloatLit(" ^ string_of_float x ^ ")"
      | SNoneLit  -> "NoneLit(NONE)" (* used for return statement *)
      | SBoolLit(true) -> "BoolLit(true)"
      | SBoolLit(false) -> "BoolLit(false)"
      | SArrayLit(a) -> "ArrayLit[" ^ fmt_sarray a ^ "]"
      | SId(x) -> "Id(" ^ x ^ ")"
      | SAssign(v, e) -> v ^ " = " ^ fmt_sexpr e
      | SFuncCall(name, args) -> fmt_sfcall name args
      | SBinop(l, bo, r) -> 
        fmt_sexpr l ^ " " ^ fmt_op bo ^ " " ^ fmt_sexpr r 
      | SUnop(uo, r) ->
          string_of_uop uo ^ " " ^ fmt_sexpr r
      | SSubscript(e, i) -> (fmt_sexpr e) ^ "[" ^ (fmt_sexpr i) ^ "]"
      (*| STypeCast(st, se) -> "(Casting " ^ fmt_sexpr se ^ "->" ^ fmt_typ st ^ "\n"*)
    )

and fmt_sarray (sa : sexpr list) : string =
String.concat ", " (List.map fmt_sexpr sa)

and fmt_svar_decl ((t, s), e) = fmt_typ t ^ " " ^ s ^ " = " ^
(match e with
| None -> "uninitialized"
| Some(e) -> fmt_sexpr e)
^ ";\n"

and fmt_sstmt = function
  | SExpr se -> "  " ^ fmt_sexpr se ^ ";\n"
  | SBlock (sstmts) ->
    "{\n" ^ String.concat "" (List.map fmt_sstmt sstmts) ^ " }\n"
  | SID_Block (sstmts) ->
    "Independent Block {\n" ^ String.concat "" (List.map fmt_sstmt sstmts) ^ "}\n"
  | SReturn (sexpr) -> "return " ^ fmt_sexpr sexpr ^ ";\n" 
  | SIf (se, s1, s2) -> let if_block = "if (" ^ fmt_sexpr se ^ ")\n" ^
      fmt_sstmt s1 in 
      if s2 = SBlock([]) then if_block else if_block ^ "else\n" ^ fmt_sstmt s2
  | SWhile(se, s) -> "while (" ^ fmt_sexpr se ^ ") " ^ fmt_sstmt s
  | SFor(var_init, predicate, update, block_code) ->
    "For Loop (variable: "   ^ fmt_sexpr var_init ^ ", " ^
               "predicate: " ^ fmt_sexpr predicate ^ ", " ^
               "update: "    ^ fmt_sexpr update ^ ") {\n\t" ^
              fmt_sstmt block_code ^ "}\n"
  | SVarDecl vd -> fmt_svar_decl vd
  | SVarDeclList(var_decls) -> 
    "variable initializer list: \n" ^ 
    String.concat "" (List.map fmt_svar_decl var_decls) ^
    "\n"

and fmt_sfcall name args = 
  "FuncCall(" ^
     "name: " ^ fmt_string name ^
     ", args: " ^ fmt_sexpr_list args ^
  ")"

and fmt_sexpr_list l = String.concat "\n" (List.map fmt_sexpr l)


let fmt_sfdecl (fdecl:sfunc_def) =
  fmt_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map fmt_vdecl fdecl.slocals) ^
  String.concat "" (List.map fmt_sstmt fdecl.sbody) ^
  "}\n"

let fmt_sprogram (funcs) =
  String.concat "\n" (List.map fmt_sfdecl funcs)
