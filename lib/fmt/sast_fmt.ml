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
      | SNoneLit  -> "NoneLit(NONE)" (* used for return statement *)
      | SBoolLit(true) -> "BoolLit(true)"
      | SBoolLit(false) -> "BoolLit(false)"
      | SId(x) -> "Id(" ^ x ^ ")"
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SFuncCall(name, args) -> fmt_sfcall name args
      | SBinop(l, bo, r) -> 
        string_of_sexpr l ^ " " ^ string_of_op bo ^ " " ^ string_of_sexpr r 
      | SUnop(uo, r) ->
          string_of_uop uo ^ " " ^ string_of_sexpr r
      | STypeCast(st, se) -> "(Casting " ^ string_of_sexpr se ^ "->" ^ string_of_typ st ^ "\n"
    )

and string_of_sstmt = function
  | SExpr se -> "  " ^ string_of_sexpr se ^ ";\n"
  | SBlock (sstmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt sstmts) ^ "}\n"
  | SReturn (sexpr) -> "return " ^ string_of_sexpr sexpr ^ ";\n" 
  | SIf (se, s1, s2) -> let if_block = "if (" ^ string_of_sexpr se ^ ")\n" ^
      string_of_sstmt s1 in 
      if s2 = SBlock([]) then if_block else if_block ^ "else\n" ^ string_of_sstmt s2
  | SWhile(se, s) -> "while (" ^ string_of_sexpr se ^ ") " ^ string_of_sstmt s
  | SFor(var_init, predicate, update, block_code) ->
    "For Loop (variable: "   ^ string_of_sexpr var_init ^ ", " ^
               "predicate: " ^ string_of_sexpr predicate ^ ", " ^
               "update: "    ^ string_of_sexpr update ^ ") {\n\t" ^
              string_of_sstmt block_code ^ "}\n"
  | SVarDecl((t, s), se) -> string_of_typ t ^ " " ^ s ^ " = " ^
    match se with
    | Some(se) -> string_of_sexpr se ^ ";\n"
    | None -> ""

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

let string_of_sprogram (funcs) =
  String.concat "\n" (List.map string_of_sfdecl funcs)
