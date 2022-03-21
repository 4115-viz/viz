type tokenseq = string list

type bop = Add | Sub | Equal | Neq | Less | And | Or

type typ = Int | Bool | Float | String | None

type bind = typ * string

type expr =
  | Literal of int
  | BoolLit of bool
  | Id of string
  | Binop of expr * bop * expr
  | Assign of string * expr

type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | Elif of expr * stmt * stmt
  | Else of expr * stmt
  | While of expr * stmt
  | INFINITE_LOOP of expr * stmt

type program = {
  locals: bind list;
  body: stmt list;
}

let string_of_program l =
  "\n\nScanned program: \n" ^ (List.fold_left (fun s e -> s ^ "\n" ^ e) "" l)