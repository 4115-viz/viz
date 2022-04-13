(* Semantic checking for the Viz compiler *)

open Ast
open Sast
open Ast_fmt

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (typ * string) list) =
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
  (* need to convert print to print_int *)
  (* should also have a print_bool, print_string, print_none, print_float*)
  
  (* create a list of pairs. (func name, func_def) *)
 (*
 let builtin_funcs = [
  {name = "print"; typ = NoneType; params = [(StrType, "x")]; locals = []; body = []};
] and iterate through the list
 *)
  
  let built_in_decls =
    let add_built_in_function map (name, f) = StringMap.add name {
      rtyp = NoneType;
      fname = name;
      formals = f;
      locals = [];
      body = [] } map
      in List.fold_left add_built_in_function StringMap.empty [("print", [StrType, "x"]);
                                                               ("print_int", [IntType, "x"]);
                                                               ("print_float", [FloatType, "x"]);
                                                               ("print_bool", [BoolType, "x"]);
                                                               ("println", [])]
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
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
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (*let string_conversion et e err =
    (* convert the first expression result to string *)
      if et == NoneType then raise (Failure err)
      else if et != StrType then e
      else e
    in*)

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr = function
        IntLit x -> (IntType, SIntLit x)
      | FloatLit x -> (FloatType, SFloatLit x)
      | StrLit x -> (StrType, SStrLit x)
      | BoolLit x -> (BoolType, SBoolLit x)
      | NoneLit   -> (NoneType, SNoneLit)
      | Id var -> (type_of_identifier var, SId var)
      | Assign(var, e) as ex ->
        let lt = type_of_identifier var
        and (rt, e') = check_expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        (check_assign lt rt err, SAssign(var, (rt, e')))

      | Binop(l, bo, r) as ex-> 
        let (ltype, l') = check_expr l in
        let (rtype, r') = check_expr r in
        (* we can only do binop on operands of same type *)
        let compatible_types = (ltype = rtype) in
        (* throw error, or return final_type for supported binops *)
        let final_type = 
          if compatible_types = false then
            raise (Failure ("incompatible types for binary operator " ^
                  string_of_typ ltype ^ " " ^ string_of_op bo ^ " " ^
                  string_of_typ rtype ^ " in " ^ string_of_expr ex))
          
          else           
            (fun my_op -> match my_op with
            | (Add | Sub | Mult | Mod ) when ltype = IntType && rtype = IntType -> IntType
            | (Add | Sub | Mult | Mod ) when ltype = FloatType && rtype = FloatType -> FloatType
            | (Div) when ltype = IntType && rtype = IntType -> 
              (* is this the correct way to check for div by zero? is there a way to evaluat the expr on RHS? *)
              let () = print_endline (string_of_expr r) in 
              if r = IntLit(0) then raise (Failure ("Cannot Divide by Zero in " ^ string_of_expr l 
                                            ^ " " ^ string_of_op my_op ^ " " ^ string_of_expr r))
                    else IntType
            | (Div) when ltype = FloatType && rtype = FloatType -> 
              (* is this the correct way to check for div by zero? is there a way to evaluat the expr on RHS? *)
              if r = FloatLit(0.0) then raise (Failure ("Cannot Divide by Zero in " ^ string_of_expr l 
                                              ^ " " ^ string_of_op my_op ^ " " ^ string_of_expr r))
                    else FloatType
            | (Eq | Neq) -> BoolType
            | (Leq | Geq | Less | Great) when (ltype = IntType && rtype = IntType ||
                                                ltype = FloatType && rtype = FloatType) -> BoolType
            | (And | Or) when (ltype = BoolType && rtype = BoolType) -> BoolType
            | _ -> raise (Failure ("No operator (" ^ string_of_op bo ^ ") " ^ "to handle type (" ^
                          string_of_typ ltype ^ ", " ^ string_of_typ rtype))
            ) bo
        in (final_type, SBinop((ltype, l'), bo, (rtype, r')))
      | Unop(uo, r) as ex ->
        let (rtype, r') = check_expr r in
        let final_type = 
          (* if we add other unary operands, we may need to be more clever 
          with type support. Right now, we are supporting Not, so this is ok.  
          *)
          if rtype != BoolType then
            raise (Failure ("incompatible types for unary operator " ^
            string_of_uop uo ^ " " ^ string_of_typ rtype ^ " in " ^ string_of_expr ex))
          else
            (fun my_uop -> match my_uop with 
            | Not -> BoolType
            ) uo
          in (final_type, SUnop(uo, (rtype, r')))
      | FuncCall(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
                let (et, e') = check_expr e in
                let err = "illegal argument found " ^ string_of_typ et ^
                          " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
                in 
                (*if fd.fname == "print" then (ft, string_conversion et e' err) (*convert to string*)
                else*) (check_assign ft et err, e')
        in
        let args' = List.map2 check_call fd.formals args
        in (fd.rtyp, SFuncCall(fname, args'))
      | TypeCast(ty, expr) -> 
        let (rtype, r') = check_expr expr in (* thing we want to cast *)
        let err = (fun ty1 ty2 -> raise (Failure ("Cannot cast " ^ string_of_typ ty1 ^ " to " ^ string_of_typ ty2  )) )
        in let casted_expr = 
            (match ty with
                  | IntType -> 
                          (* i think the actual casting may be done here, or in LLVM not entirely sure
                              I originlally had each of the subcases cast but I couldnt get the return type correctly
                          *)
                          ( match rtype with 
                          | IntType | FloatType | StrType -> r'
                          | _ -> err rtype ty
                          )
                  | FloatType ->
                          ( match rtype with 
                          | IntType | FloatType | StrType -> r'
                          | _ -> err rtype ty
                          )
                  | StrType ->
                          ( match rtype with 
                          | IntType | FloatType | StrType | BoolType -> r' (* there is in fact of string_of_bool cast in ocaml *)
                          | _ -> err rtype ty
                          )
                  | BoolType ->
                          ( match rtype with 
                          | StrType -> r' (* there is in fact a bool_of_string cast in ocaml *)
                          | _ -> err rtype ty
                          )
                  | NoneType -> raise (Failure ("Cannot cast to NoneType" ))
            )
        in (ty, casted_expr) 
    in

    let check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | BoolType -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in

    (* Here we need to ensure that you are following
      correct fdecl format. see below 
      added this in to check two function types:
        1) func main(): none { ... }
        2) func main(): 
    *)
    (*let check_function_decl = true
          {
        styp = f.typ;
        sname = f.name;
        sparams = f.params;
        sbody = check_program f.body;
      } something like ensure that these fields can be filled
    in *)
    let rec check_stmt_list =function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt = function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      | If(e, st1, st2) ->
        
        (*(* note: we need to check for st2 being a No_op *)
        (* this case is if without else *)
        if check_stmt st2 = SNo_op then
          SIf(check_bool_expr e, check_stmt st1, SBlock([]))
        (* this case is if/else *)
        else *)
          SIf(check_bool_expr e, check_stmt st1, check_stmt st2)
      | While(e, st) ->
        SWhile(check_bool_expr e, check_stmt st)
      | For(e1, e2, e3, st) ->
          SFor(check_expr e1, check_bool_expr e2, check_expr e3, check_stmt st)
      | Return e ->
        let (t, e') = check_expr e in
        if t = func.rtyp then SReturn (t, e')
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                     string_of_typ func.rtyp ^ " in " ^ string_of_expr e))
      (*| No_op -> SNo_op (* for the case where we only want if (..) {...} with no else block *)*)
    in (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = check_stmt_list func.body
    }
  in
  (globals, List.map check_func functions)
