open Sast
open Ast_fmt

let rec string_of_sexpr (t, se) =
  "(" ^ string_of_typ t ^ " : " ^ 
  if t == StrType then "(" ^ 
      string_s se
    ^ ")"
  else
    string_s se 
  ^ ")"

and string_s se =
      (match se with
      | SStrLit(x) -> "StrLit(" ^ fmt_string x ^ ")"
      | SIntLit(x) -> "IntLit(" ^ string_of_int x ^ ")"
      | SFloatLit(x) -> "FloatLit(" ^ string_of_float x ^ ")"
      | SBoolLit(true) -> "BoolLit(true)"
      | SBoolLit(false) -> "BoolLit(false)"

      | SId(x) -> "Id(" ^ x ^ ")"
      
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SFuncCall(name, args) -> fmt_sfcall name args
      | SBinop(l, bo, r) -> 
        string_of_sexpr l ^ " " ^ string_of_op bo ^ " " ^ string_of_sexpr r 
      | SUnop(uo, r) ->
          string_of_uop uo ^ " " ^ string_of_sexpr r
    )

and string_of_sstmt = function
  | SExpr se -> "  " ^ string_of_sexpr se ^ ";\n"
  | SBlock (sstmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt sstmts) ^ "}\n"
  | SReturn (sexpr) -> "return " ^ string_of_sexpr sexpr ^ ";\n" 
  | SIf (se, s1, s2) -> let if_block = "if (" ^ string_of_sexpr se ^ ")\n" ^
      string_of_sstmt s1 in 
      if s2 = SNo_op then if_block else if_block ^ "else\n" ^ string_of_sstmt s2
  | SWhile(se, s) -> "while (" ^ string_of_sexpr se ^ ") " ^ string_of_sstmt s
  | SNo_op -> "No Op"

and fmt_sfcall name args = 
  "FuncCall(" ^
     "name: " ^ fmt_string name ^
     ", args: " ^ fmt_sexpr_list args ^
  ")"

and fmt_sexpr_list l = String.concat "\n" (List.map string_of_sexpr l)


let string_of_sfdecl (fdecl:sfunc_def) =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nSemantically checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
