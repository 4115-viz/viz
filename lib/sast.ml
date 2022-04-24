open Ast

type sexpr = typ * sx
and sx =
  | SStrLit of string
  | SIntLit of int
  | SFloatLit of float
  | SBoolLit of bool
  | SNoneLit
  | SArrayLit of sexpr list
  | SBinop of sexpr * bop * sexpr
  | SAssign of spostfix_expr * sexpr
  | SFuncCall of string * sexpr list
  | SUnop of uop * sexpr
  | STypeCast of typ * sexpr
  | SPostfixExpr of spostfix_expr

and spostfix_expr = typ * spx
and spx =
  | SId of string
  | SMemberAccess of spostfix_expr * string
  | SSubscript of spostfix_expr * sexpr

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
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  sbody: sstmt list;
}

type sstruct_def = {
  sname: string;
  smembers: bind list; 
}

type sprogram = sstruct_def list * sfunc_def list