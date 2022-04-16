open Ast

let fmt_op = function
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


let rec fmt_typ = function
  | NoneType  -> "Type(None)"
  | StrType   -> "Type(Str)"
  | IntType   -> "Type(Int)"
  | BoolType  -> "Type(Bool)"
  | FloatType -> "Type(Float)"
  | ArrayType(t) -> String.concat "" ["Type(Array("; fmt_typ t ;"))"]

let string_of_uop = function
| Not -> "not"
| Neg -> "neg"

let fmt_string x = String.concat "" ["\""; x; "\""]

let rec fmt_expr = function
  | StrLit(x) -> "StrLit(" ^ fmt_string x ^ ")"
  | IntLit(x) -> "IntLit(" ^ string_of_int x ^ ")"
  | FloatLit(x) -> "FloatLit(" ^ string_of_float x ^ ")"
  | NoneLit  -> "NoneLit(NONE)" (* used for return statement *)
  | BoolLit(true) -> "BoolLit(true)"
  | BoolLit(false) -> "BoolLit(false)"
  | ArrayLit(a) -> "ArrayLit[" ^ fmt_array a ^ "]"
  | Assign(v, e) -> v ^ " = " ^ fmt_expr e
  | Id(x) -> "Id(" ^ x ^ ")"
  | FuncCall(name, args) ->
    (*name ^ "(" ^ String.concat ", " (List.map fmt_expr args) ^ ")"*)
    fmt_fcall name args
  | Binop(l, bo, r) ->
    fmt_expr l ^ " " ^ fmt_op bo ^ " " ^ fmt_expr r
  | Unop(uo, r) ->
    string_of_uop uo ^ " " ^ fmt_expr r
  (*| TypeCast(t, e) -> "Casting " ^ fmt_expr e ^ "->" ^ fmt_typ t ^ "\n"*)
  
and fmt_fcall name args = 
  "FuncCall(" ^
     "name: " ^ fmt_string name ^
     ", args: " ^ fmt_expr_list args ^
  ")"
  
and fmt_expr_list l = String.concat "\n" (List.map fmt_expr l)

and fmt_array (a : expr list) : string =
 String.concat ", " (List.map fmt_expr a)

and fmt_stmt = function

  | Expr e -> "  " ^ fmt_expr e
  | Block (stmts) ->
    "{\n" ^ String.concat "" (List.map fmt_stmt stmts) ^ "}\n"
  | Return (expr) -> "return " ^ fmt_expr expr ^ ";\n" 
  | If (e, s1, s2) -> let if_block = "if (" ^ fmt_expr e ^ ")\n" ^ fmt_stmt s1
                      in if s2 = Block([]) then if_block else if_block ^ "else\n" ^ fmt_stmt s2
  
  | While(e, s) -> "while (" ^ fmt_expr e ^ ") " ^ fmt_stmt s
  | For(var_init, predicate, update, block_code) ->
    "For Loop (variable: "   ^ fmt_expr var_init ^ ", " ^
               "predicate: " ^ fmt_expr predicate ^ ", " ^
               "update: "    ^ fmt_expr update ^ ") {\n\t" ^
              fmt_stmt block_code ^ "}\n"
 | VarDecl((t, s), e) -> fmt_typ t ^ " " ^ s ^ " = " ^
    (match e with
    | None -> "uninitialized"
    | Some(e) -> fmt_expr e)
    ^ ";\n"

(*let fmt_vdecl ((t, n), e) = *)
let fmt_vdecl (t, n) =
  "VarDecl(" ^
    "name: " ^ fmt_string n ^
    ", type: " ^ fmt_typ t ^")\n"(* ^
    match e with
    | None -> ""
    | Some(e) -> ", value: " ^ fmt_expr e *)

let fmt_fdecl fdecl =
  fmt_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map fmt_vdecl fdecl.locals) ^
  String.concat "" (List.map fmt_stmt fdecl.body) ^
  "}\n"

let fmt_program (funcs) =
  String.concat "\n" (List.map fmt_fdecl funcs)
