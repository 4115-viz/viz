type bop = Add | Sub | Mult | Div | Eq | Neq | Less 
        | Great | Leq | Geq | And | Or | Mod

type uop = Not

type builtin_type = 
  | NoneType
  | StrType
  | IntType
  | BoolType
  | FloatType

  type expr =
  | Assign of string * expr
  | IntLit of int
  | FloatLit of float
  | StrLit of string
  | BoolLit of bool
  | Id of string
  | FuncCall of string * expr list
  | Binop of expr * bop * expr
  | Unop of uop * expr

type bind = builtin_type * string

(* ----- Entry ----- *)
type program = stmt list

and stmt =
  | Expr of expr
  | VarDecl of bind * expr option
  | FuncDecl of func_decl

and func_decl = {
  typ: builtin_type;
  name: string;
  params: bind list;
  body: stmt list;
}