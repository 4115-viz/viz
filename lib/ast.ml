type bop = Add | Sub | Eq | Neq | Less | And | Or
          | Mult | Div | Great | Leq | Geq | Mod

type uop = Not

type typ = 
  | NoneType
  | StrType
  | IntType
  | BoolType
  | FloatType

type expr =
  | StrLit of string
  | IntLit of int
  | FloatLit of float
  | BoolLit of bool

  | Id of string

  | Assign of string * expr
  | FuncCall of string * expr list
  | Binop of expr * bop * expr
  | Unop of uop * expr

type stmt =
  | Expr of expr
  | Block of stmt list
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr * expr * expr * stmt
  | Return of expr
  
type bind = typ * string

type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

(* ----- Entry ----- *)
type program = bind list * func_def list