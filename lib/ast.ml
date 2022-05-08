type bop = Add | Sub | Eq | Neq | Less | And | Or
          | Mult | Div | Great | Leq | Geq | Mod


type uop = Not | Neg

type typ = 
  | NoneType
  | StrType
  | IntType
  | BoolType
  | FloatType
  | ListType of (typ option) * (int option)
  | StructType of string

type bind = typ * string

type expr =
  | StrLit of string
  | IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | NoneLit
  | ListLit of expr list
  | Assign of postfix_expr * expr
  | FuncCall of string * expr list
  | Binop of expr * bop * expr
  | Unop of uop * expr
  | TypeCast of typ * expr
  | PostfixExpr of postfix_expr

and postfix_expr =
  | Id of string
  | MemberAccess of postfix_expr * string
  | Subscript of postfix_expr * expr

type var_decl = bind * expr option

type stmt =
  | Expr of expr
  | Block of stmt list
  | ID_Block of stmt list
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr * expr * expr * stmt
  | Return of expr
  | VarDecl of var_decl
  | VarDeclList of var_decl list
  
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  body: stmt list;
}

type struct_def = {
  name: string;
  members: bind list; 
}

(* ----- Entry ----- *)
type program = struct_def list * func_def list