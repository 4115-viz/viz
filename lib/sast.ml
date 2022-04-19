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
  | SSubscript of sexpr * sexpr

type svar_decl = bind * sexpr option

type sstmt =
  | SBlock of sstmt list
  | SID_Block of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SReturn of sexpr
  | SVarDecl of svar_decl
  | SVarDeclList of svar_decl list
  
type sfunc_def = {
  srtyp: builtin_type;
  sfname: string;
  sformals: bind list;
  sbody: sstmt list;
  slocals: bind list;
}

type sprogram = bind list * sfunc_def list