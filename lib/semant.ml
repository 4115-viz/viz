(* Semantic checking for the Viz compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)
  
(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)
let check_program (program : stmt list) =


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

  let check_stmt (symbols: builtin_type StringMap.t) = function
    | Expr e -> SExpr (check_expr symbols e)
    | _ -> raise(Failure ("TODO - check_stmt"))
  in

  let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
    StringMap.empty (func.formals @ func.locals)
  in

  List.map (fun stmt -> check_stmt symbols stmt) program