(* Semantic checking for the Viz compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

let builtin_funcs = [
  {name = "print"; typ = NoneType; body = []};
]

(* Semantic checking of the AST. Returns an SAST if successful,
  throws an exception if something is wrong.

  Check each global variable, then check each function *)
let rec check_program (program : stmt list) =
  let check_stmt (_: func_decl StringMap.t) stmt =
    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
      StringMap.empty []
    in

    (* Return a variable from our local symbol table *)
    let check_id (symbols: builtin_type StringMap.t ) id =
      try StringMap.find id symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ id))
    in

    (* Return a semantically-checked expression (typ, expr) *)
    let check_expr (symbols: builtin_type StringMap.t) = function
      | StrLit x -> (StrType, SStrLit x)
      | Id x -> (check_id symbols x, SId x)
      | FuncCall (_, _) -> raise(Failure ("TODO - FuncCall"))
    in
    
    let check_function f = 
      {
        styp = f.typ;
        sname = f.name;
        sbody = check_program f.body;
      }
      
    in match stmt with 
      | Expr e -> SExpr (check_expr symbols e)
      | FuncDecl fd -> SFuncDecl (check_function fd)
  in
  let ctxt = 
    let add_func map fd = StringMap.add fd.name fd map in
    List.fold_left add_func StringMap.empty builtin_funcs
  in
  List.map (fun stmt -> check_stmt ctxt stmt) program