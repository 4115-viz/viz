(* Semantic checking for the Viz compiler

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
        | IntLit x -> (IntType, SIntLit x)
        | StrLit x -> (StrType, SStrLit x)
        | Assign(v, e) as ex ->
          let lt = check_id symbols v
          and (rt, e') = check_expr symbols e in
          let err = "illegal assignment " ^ fmt_typ lt ^ " = " ^
              fmt_typ rt ^ " in " ^ fmt_expr ex
          in
          (check_assign lt rt err, SAssign(v, (rt, e')))
        | BoolLit x -> (BoolType, SBoolLit x)
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
  List.map (fun stmt -> check_stmt ctxt stmt) program *)

  (* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (builtin_type * string) list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Make sure no globals duplicate *)
  check_binds "global" globals;

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    StringMap.add "print" {
      typ = IntType;
      name = "print";
      params = [(IntType, "x")];
      (* locals = [];  *)
      body = [] } StringMap.empty
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.name ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.name
    and make_err er = raise (Failure er)
    and n = fd.name (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.params;
    (* check_binds "local" func.locals; *)

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty (globals @ func.params) (* @ func.locals *)
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr = function
        IntLit l -> (IntType, SIntLit l)
      | StrLit s -> (StrType, SStrLit s)
      | BoolLit l -> (BoolType, SBoolLit l)
      | Id var -> (type_of_identifier var, SId var)
      | Assign(var, e) as ex ->
        let lt = type_of_identifier var
        and (rt, e') = check_expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        (check_assign lt rt err, SAssign(var, (rt, e')))

      (* | Binop(e1, op, e2) as e ->
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
              Add | Sub when t1 = Int -> Int
            | Equal | Neq -> Bool
            | Less when t1 = Int -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else raise (Failure err) *)
      | FuncCall(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.params in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
               let (et, e') = check_expr e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.params args
          in (fd.typ, SFuncCall(fname, args'))
    in

    (* let check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | BoolType -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in *)

    let rec check_stmt_list = function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt = function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      (* | If(e, st1, st2) -> *)
        (* SIf(check_bool_expr e, check_stmt st1, check_stmt st2) *)
      (* | While(e, st) -> *)
        (* SWhile(check_bool_expr e, check_stmt st) *)
      | Return e ->
        let (t, e') = check_expr e in
        if t = func.typ then SReturn (t, e')
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                     string_of_typ func.typ ^ " in " ^ string_of_expr e))
      (* | FuncDecl func -> SFuncDecl (check_stmt_list func) *)
    in (* body of check_func *)
    { styp = func.typ;
      sname = func.name;
      sparams = func.params;
      (* slocals  = func.locals; *)
      sbody = check_stmt_list func.body
    }
  in
  (globals, List.map check_func functions)