open Ast

type sexpr = builtin_type * sx
and sx =
  | SStrLit of string
  | SId of string
  | SFuncCall of string * sexpr list
  
type sstmt =
  | SExpr of sexpr
  | SFuncCall of sfunc_decl
and sfunc_decl = {
  typ: builtin_type;
  name: string;
  body: sstmt list;
}

type sprogram = sstmt list