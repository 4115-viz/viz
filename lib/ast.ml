type builtin_type = 
  | NoneType

type expr =
  | StrLit of string
  | Id of string
  | FuncCall of string * expr list

and stmt =
  | Expr of expr
  | FuncDecl of func_decl

and func_decl = {
  typ: builtin_type;
  name: string;
  body: stmt list
}

(* ----- Entry ----- *)
type program = stmt list

(* ----- Print Function ----- *)
let string_of_typ = function
  | NoneType -> "Type(None)"

let rec string_of_fcall name args = 
  "FuncCall(" ^
     "name: " ^ name ^
     ", args: " ^ 
      String.concat "\n" (List.map string_of_expr args) ^
  ")"

and string_of_expr = function
  | StrLit(x) -> "StrLit(\"" ^ x ^ "\")"
  | Id(x) -> "Id(" ^ x ^ ")"
  | FuncCall(name, args) -> string_of_fcall name args 

let rec string_of_fdecl fd =
  "Function(" ^ 
      "name: " ^ fd.name ^
      ", type: " ^ string_of_typ fd.typ ^
    ")" ^ " {\n" ^ "  " ^
    String.concat " \n" (List.map string_of_stmt fd.body)
    ^
    "\n}\n"

and string_of_stmt = function
  | Expr e -> string_of_expr e
  | FuncDecl fd -> string_of_fdecl fd

let string_of_program sl = String.concat "\n" (List.map string_of_stmt sl)