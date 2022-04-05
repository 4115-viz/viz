open Ast

type sexpr = builtin_type * sx
and sx =
  | SAssign of string * sexpr
  | SStrLit of string
  | SIntLit of int
  | SFloatLit of float
  | SBoolLit of bool
  | SId of string
  | SFuncCall of string * sexpr list
  | SBinop of sexpr * bop * sexpr
  | SUnop of uop * sexpr

  type sstmt =
  | SExpr of sexpr
  | SVarDecl of bind * sexpr option
  | SFuncDecl of sfunc_decl

and sfunc_decl = {
  styp: builtin_type;
  sname: string;
  sparams: bind list;
  sbody: sstmt list;
}

type sprogram = sstmt list