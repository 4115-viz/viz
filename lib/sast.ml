open Ast

type sexpr = builtin_type * sx
and sx =
  | SStrLit of string
  | SIntLit of int
  | SFloatLit of float
  | SBoolLit of bool
  | SNoneLit
  | SArrayLit of sexpr list
  | SId of string
  | SBinop of sexpr * bop * sexpr
  | SAssign of string * sexpr
  | SFuncCall of string * sexpr list
  | SUnop of uop * sexpr

type sstmt =
  | SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SReturn of sexpr
  | SVarDecl of bind * sexpr option
  
type sfunc_def = {
  srtyp: builtin_type;
  sfname: string;
  sformals: bind list;
  sbody: sstmt list;
  slocals: bind list;
}

type sprogram = bind list * sfunc_def list