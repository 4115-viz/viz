open Ast

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


let rec string_of_typ = function
  | NoneType  -> "Type(None)"
  | StrType   -> "Type(Str)"
  | IntType   -> "Type(Int)"
  | BoolType  -> "Type(Bool)"
  | FloatType -> "Type(Float)"
  | ArrayType(t) -> String.concat "" ["Type(Array of"; string_of_typ t ;")"]

let string_of_uop = function
| Not -> "not"
| Neg -> "neg"

let fmt_string x = String.concat "" ["\""; x; "\""]

let rec string_of_expr = function
  | StrLit(x) -> "StrLit(" ^ fmt_string x ^ ")"
  | IntLit(x) -> "IntLit(" ^ string_of_int x ^ ")"
  | FloatLit(x) -> "FloatLit(" ^ string_of_float x ^ ")"
  | NoneLit  -> "NoneLit(NONE)" (* used for return statement *)
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
  | TypeCast(t, e) -> "Casting " ^ string_of_expr e ^ "->" ^ string_of_typ t ^ "\n"
  
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
  | If (e, s1, s2) -> let if_block = "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1
                      in if s2 = Block([]) then if_block else if_block ^ "else\n" ^ string_of_stmt s2
  
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(var_init, predicate, update, block_code) ->
    "For Loop (variable: "   ^ string_of_expr var_init ^ ", " ^
               "predicate: " ^ string_of_expr predicate ^ ", " ^
               "update: "    ^ string_of_expr update ^ ") {\n\t" ^
              string_of_stmt block_code ^ "}\n"
 | VarDecl((t, s), e) -> string_of_typ t ^ " " ^ s ^ " = " ^
    (match e with
    | None -> "uninitialized"
    | Some(e) -> string_of_expr e)
    ^ ";\n"

(*let string_of_vdecl ((t, n), e) = *)
let string_of_vdecl (t, n) =
  "VarDecl(" ^
    "name: " ^ fmt_string n ^
    ", type: " ^ string_of_typ t ^")\n"(* ^
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

let string_of_program (funcs) =
  String.concat "\n" (List.map string_of_fdecl funcs)
