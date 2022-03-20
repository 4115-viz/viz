type typ =
  | NoneType
  | IntegerType
  | FloatType
  | BoolType
  | StringType

type bind = typ * string
type expr =
  | BoolLit of bool
  | IntLit of int
  | FloatLit of string
  | Id of string
  | Assign of string * expr
  | FuncCall of string * expr list

type func_decl = {
  func_name: string;
  func_type: typ;
  params: bind list;
  body: stmt list
}

and stmt =
  | Expr of expr
  | FuncDecl of func_decl

type program = stmt list

let string_of_program l =
  "\n\nScanned program: \n" ^ (List.fold_left (fun s e -> s ^ "\n" ^ e) "" l)