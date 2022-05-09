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
  | ListType(t, l) -> 
    let typ = match t with
    | Some(t) -> fmt_typ t
    | None -> "Unknown" in
    let len = match l with
    | Some(i) -> string_of_int i
    | None -> "Unknown" in
    String.concat "" [
      "Type(List<"; 
      typ ;
      ">[" ;
      len ;
      "])"]
  | StructType(id:string) -> "Type(Struct(\"" ^ id ^ "\"))"

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
  | ListLit(a) -> "ListLit[" ^ fmt_list a ^ "]"
  | Assign(v, e) -> String.concat "" [
    "Assign("; 
    fmt_expr(PostfixExpr v);
    " = ";
    fmt_expr e;
    ")\n"]
  | FuncCall(name, args) -> fmt_fcall name args
  | Binop(l, bo, r) ->
    fmt_expr l ^ " " ^ fmt_op bo ^ " " ^ fmt_expr r
  | Unop(uo, r) ->
    string_of_uop uo ^ " " ^ fmt_expr r
  | TypeCast(t, e) -> "Casting " ^ fmt_expr e ^ " -> " ^ fmt_typ t ^ "\n"
  | PostfixExpr x -> (match x with
    | Id id -> "Id(" ^ id ^ ")"
    | Subscript(e, i) -> (fmt_expr (PostfixExpr e)) ^ "[" ^ (fmt_expr i) ^ "]"
    | MemberAccess (e,member) -> (fmt_expr (PostfixExpr e)) ^ "." ^ member
  )
  
and fmt_fcall name args = 
  "FuncCall(" ^
     "name: " ^ fmt_string name ^
     ", args: " ^ fmt_expr_list args ^
  ")\n"
  
and fmt_expr_list l = String.concat "\n" (List.map fmt_expr l)

and fmt_list (a : expr list) : string =
 String.concat ", " (List.map fmt_expr a)

and fmt_var_decl ((t, s), e) = fmt_typ t ^ " " ^ s ^ " = " ^
  (match e with
  | None -> "uninitialized"
  | Some(e) -> fmt_expr e)
^ ";\n"

and fmt_stmt = function
  | Expr e -> fmt_expr e ^ "\n"
  | Block (stmts) ->
    "{\n" ^ String.concat "" (List.map fmt_stmt stmts) ^ "}\n"
  | ID_Block (stmts) ->
    "Independent Block {\n" ^ String.concat "" (List.map fmt_stmt stmts) ^ "}\n"
  | Return (expr) -> "return " ^ fmt_expr expr ^ ";\n" 
  | If (e, s1, s2) -> let if_block = "if (" ^ fmt_expr e ^ ")\n" ^ fmt_stmt s1
                      in if s2 = Block([]) then if_block else if_block ^ "else\n" ^ fmt_stmt s2
  
  | While(e, s) -> "while (" ^ fmt_expr e ^ ") " ^ fmt_stmt s
  | For(var_init, predicate, update, block_code) ->
    "For Loop (variable: "   ^ fmt_expr var_init ^ ", " ^
               "predicate: " ^ fmt_expr predicate ^ ", " ^
               "update: "    ^ fmt_expr update ^ ") {\n\t" ^
              fmt_stmt block_code ^ "}\n"
 | VarDecl vd -> fmt_var_decl vd
 | VarDeclList(var_decls) -> 
  "variable initializer list: \n" ^ 
  String.concat "" (List.map fmt_var_decl var_decls) ^
  "\n"
 
(*let fmt_vdecl ((t, n), e) = *)
let fmt_vdecl (t, n) =
  "VarDecl(" ^
    "name: " ^ fmt_string n ^
    ", type: " ^ fmt_typ t ^")\n"(* ^
    match e with
    | None -> ""
    | Some(e) -> ", value: " ^ fmt_expr e *)

let fmt_fdecl fdecl =
  "Function Declartion: " ^
  fmt_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^
  "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")" ^ "\n{\n" ^ "  " ^
  String.concat "  " (List.map fmt_stmt fdecl.body) ^
  "}\n"

let fmt_sdecl struct_decl = 
  "Struct Declaration(\"" ^ struct_decl.name ^ "\")\n{\n" ^
    "  " ^ String.concat "  " (List.map fmt_vdecl struct_decl.members) ^
  "}\n"

let fmt_program (structs, funcs) =
  String.concat "" (List.map fmt_sdecl structs) ^ "\n" ^
  String.concat "\n" (List.map fmt_fdecl funcs)
