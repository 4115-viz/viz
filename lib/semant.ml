(* Semantic checking for the Viz compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

let builtin_funcs = [
  {name = "print"; typ = NoneType; params = [(StrType, "x")]; body = []};
]

(* Return a function from our symbol table *)
let find_func ctxt s = 
  try StringMap.find s ctxt
  with Not_found -> raise (Failure ("unrecognized function " ^ s))

(* Semantic checking of the AST. Returns an SAST if successful,
  throws an exception if something is wrong.

  Check each global variable, then check each function *)
let rec check_program (program : stmt list) =
  let check_stmt (ctxt: func_decl StringMap.t) stmt =
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
    let rec check_expr (symbols: builtin_type StringMap.t) expr =
      let check_assign ltyp rtyp err =
        if ltyp = rtyp then ltyp else raise (Failure err)
      in

      let check_call (name, args) =
        let fd = find_func ctxt name in
        let params_length = List.length fd.params in
        if List.length args != params_length then
          raise (Failure ("expecting " ^ string_of_int params_length ^ 
                          " arguments in " ^ name))
        else let check_arg (ft, _) arg =
          let (et, e') = check_expr symbols arg in
          let err = "illegal argument found " ^ fmt_typ et ^
          " expected " ^ fmt_typ ft ^ " in " ^ fmt_expr arg 
        in (check_assign ft et err, e') in

        let sargs = List.map2 check_arg fd.params args
        in (fd.typ, SFuncCall(name, sargs))
      in match expr with
        | StrLit x -> (StrType, SStrLit x)
        | Id x -> (check_id symbols x, SId x)
        | FuncCall (name, args) -> check_call (name, args)
    in
    
    let check_function f = 
      {
        styp = f.typ;
        sname = f.name;
        sparams = f.params;
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