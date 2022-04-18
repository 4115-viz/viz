(* Semantic checking for the Viz compiler *)

open Ast
open Sast
open Ast_fmt

module StringMap = Map.Make(String)

type symbol = {
  typ: builtin_type;
  inited: bool;
}

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each function *)

let check (functions) =

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (builtin_type * string) list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Collect function declarations for built-in functions: no bodies *)
  (* need to convert print to print_int *)
  (* should also have a print_bool, print_string, print_none, print_float*)
  
  (* create a list of pairs. (func name, func_def) *)
 (*
 let builtin_funcs = [
  {name = "print"; builtin_type = NoneType; params = [(StrType, "x")]; locals = []; body = []};
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
    let check_assign (lvaluet:builtin_type) (rvaluet:builtin_type) (err:string): builtin_type =
      if lvaluet = rvaluet then 
        lvaluet
      else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols : symbol StringMap.t = 
      List.fold_left (fun m (ty, name) -> StringMap.add name { typ = ty; inited = true } m)
        StringMap.empty (func.formals @ func.locals)
    in

    (* Query a variable from our local symbol table *)
    let find_symbol (id: string) (symbols : symbol StringMap.t) : symbol =
      try StringMap.find id symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ id))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr (symbols: symbol StringMap.t) (e: expr) : sexpr = 
      match e with
        IntLit x -> (IntType, SIntLit x)
      | FloatLit x -> (FloatType, SFloatLit x)
      | StrLit x -> (StrType, SStrLit x)
      | BoolLit x -> (BoolType, SBoolLit x)
      | NoneLit   -> (NoneType, SNoneLit)
      | ArrayLit (a : expr list) -> (match a with
        | [] -> raise (Failure "TODO: support array of length 0")
        | x :: xs -> let (t, se) = check_expr symbols x in
          let sa = List.map (fun i -> match (check_expr symbols i) with
            | (i_t, _) when i_t != t -> raise (Failure ("Invalid array literal. Expect type " ^ fmt_typ t ^ ", found" ^ fmt_typ i_t))
            | _ -> (t, se)) xs 
          in
          (ArrayType t, SArrayLit sa)
        
      )
      | Id id -> (match find_symbol id symbols with
        | { inited = false; _ } -> raise (Failure("uninitialized local variable '" ^ id ^ "' used."))
        | { typ = t; _ } -> (t, SId id))
      | Assign(var, e) as ex ->
        let s = find_symbol var symbols
        and (rt, e') = check_expr symbols e in
        let err = "illegal assignment " ^ fmt_typ s.typ ^ " = " ^
                  fmt_typ rt ^ " in " ^ fmt_expr ex
        in
        let t = check_assign s.typ rt err in
        (t, SAssign(var, (rt, e')))
      | Binop(l, bo, r) as ex-> 
        let (ltype, l') = check_expr symbols l in
        let (rtype, r') = check_expr symbols r in
        (* we can only do binop on operands of same type *)
        let compatible_types = (ltype = rtype) in
        (* throw error, or return final_type for supported binops *)
        let final_type = 
          if compatible_types = false then
            raise (Failure ("incompatible types for binary operator " ^
                  fmt_typ ltype ^ " " ^ fmt_op bo ^ " " ^
                  fmt_typ rtype ^ " in " ^ fmt_expr ex))
          
          else           
            (fun my_op -> match my_op with
            | (Add | Sub | Mult | Mod | Div) when ltype = IntType && rtype = IntType -> IntType
            | (Add | Sub | Mult | (*Mod |*) Div) when ltype = FloatType && rtype = FloatType -> FloatType
            (*| (Div) when ltype = IntType && rtype = IntType -> 
              (* is this the correct way to check for div by zero? is there a way to evaluat the expr on RHS? *)
              let () = print_endline (fmt_expr r) in 
              if r = IntLit(0) then raise (Failure ("Cannot Divide by Zero in " ^ fmt_expr l 
                                            ^ " " ^ fmt_op my_op ^ " " ^ fmt_expr r))
                    else IntType
            | (Div) when ltype = FloatType && rtype = FloatType -> 
              (* is this the correct way to check for div by zero? is there a way to evaluat the expr on RHS? *)
              if r = FloatLit(0.0) then raise (Failure ("Cannot Divide by Zero in " ^ fmt_expr l 
                                              ^ " " ^ fmt_op my_op ^ " " ^ fmt_expr r))
                    else FloatType *)
            | (Eq | Neq) -> BoolType
            | (Leq | Geq | Less | Great) when (ltype = IntType && rtype = IntType ||
                                                ltype = FloatType && rtype = FloatType) -> BoolType
            | (And | Or) when (ltype = BoolType && rtype = BoolType) -> BoolType
            | _ -> raise (Failure ("No operator (" ^ fmt_op bo ^ ") " ^ "to handle type (" ^
                          fmt_typ ltype ^ ", " ^ fmt_typ rtype))
            ) bo
        in (final_type, SBinop((ltype, l'), bo, (rtype, r')))
      | Unop(op, e) as ex -> 
        let (t, e') = check_expr symbols e in
        let ty = match op with
          Neg when t = IntType || t = FloatType -> t
        | Not when t = BoolType -> BoolType
        | _ -> raise (Failure ("illegal unary operator " ^ 
                                string_of_uop op ^ fmt_typ t ^
                                " in " ^ fmt_expr ex))
        in (ty, SUnop(op, (t, e')))
      | FuncCall(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ fmt_expr call))
        else 
        let check_call (ft, _) e =
                let (et, e') = check_expr symbols e in
                let err = "illegal argument found " ^ fmt_typ et ^
                          " expected " ^ fmt_typ ft ^ " in " ^ fmt_expr e
                in 
                if fname = "print" then (et, e') (*convert to string*)
                else (check_assign ft et err, e')
        in
        let args' = List.map2 check_call fd.formals args in
        let func_name = (
            if fname = "print" then
              (
                match (fst (List.hd args')) with
                | IntType -> "print_int"
                | FloatType -> "print_float"
                | StrType -> "print"
                | BoolType -> "print_bool"
                | NoneType -> raise (Failure ("None type cannot be printed"))
                | ArrayType _ -> raise(Failure ("TODO: print array hasn't been implemented"))
              )
            else fname
        ) 
        in (fd.rtyp, SFuncCall(func_name, args'))
      | Subscript(e, i) ->
      let (t, _) as e' = match check_expr symbols e with
      | ArrayType t, sx -> t, sx
      | (t, _) -> failwith (String.concat "" ["Subscript operator [] expect array, got: "; fmt_typ t])
      in  
      let i' = match check_expr symbols i with
      | (IntType, _) as sexpr -> sexpr
      | t, _ -> failwith (String.concat "" ["Subscript operator [] expect index to be int, got: "; fmt_typ t])
      in (t, SSubscript(e', i'))

      (*| TypeCast(ty, expr) -> 
        let (rtype, r') = check_expr symbols expr in (* thing we want to cast *)
        let err = (fun ty1 ty2 -> raise (Failure ("Cannot cast " ^ fmt_typ ty1 ^ " to " ^ fmt_typ ty2  )) )
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
                  | NoneType -> raise (Failure "Cannot cast to NoneType")
                  | ArrayType _ -> raise(Failure "TODO: NOT SUPPORT")
            )
        in (ty, casted_expr) *)
    in

    let check_bool_expr symbols e =
      let (t, e') = check_expr symbols e in
      match t with
      | BoolType -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ fmt_expr e))
    in

    (* Here we need to ensure that you are following
      correct fdecl format. see below 
      added this in to check two function types:
        1) func main(): none { ... }
        2) func main(): 
    *)
    (*let check_function_decl = true
          {
        styp = f.builtin_type;
        sname = f.name;
        sparams = f.params;
        sbody = check_program f.body;
      } something like ensure that these fields can be filled
    in *)
    let rec check_stmt_list symbols =function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list symbols (sl @ sl') (* Flatten blocks *)
      | s :: sl -> 
          let (stmt, new_symbols) = check_stmt symbols s 
          in
          stmt :: check_stmt_list new_symbols sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt (symbols : symbol StringMap.t) = function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> (SBlock (check_stmt_list symbols sl), symbols)
      | Expr e -> 
        let se = check_expr symbols e in
        let new_symbols = match e with
          | Assign (id, _) -> StringMap.add id {typ = fst se; inited = true} symbols
          | _ -> symbols
        in
        (SExpr se, new_symbols)
      | If(e, st1, st2) ->
        
        (*(* note: we need to check for st2 being a No_op *)
        (* this case is if without else *)
        if check_stmt st2 = SNo_op then
          SIf(check_bool_expr e, check_stmt st1, SBlock([]))
        (* this case is if/else *)
        else *)
          (SIf(check_bool_expr symbols e, fst (check_stmt symbols st1), fst (check_stmt symbols st2)), symbols)
      | While(e, st) ->
        (SWhile(check_bool_expr symbols e, fst (check_stmt symbols st)), symbols)
      | For(e1, e2, e3, st) ->
          (SFor(check_expr symbols e1, check_bool_expr symbols e2, check_expr symbols e3, fst (check_stmt symbols st)), symbols)
      | Return e ->
        let (t, e') = check_expr symbols e in
        if t = func.rtyp then (SReturn (t, e'), symbols)
        else raise (
            Failure ("return gives " ^ fmt_typ t ^ " expected " ^
                     fmt_typ func.rtyp ^ " in " ^ fmt_expr e))
      | VarDeclList _ -> 
        raise ( Failure("Under Construction"))

      | VarDecl ((t, id) as b, e) ->
        match t with
        | NoneType -> raise (Failure ("Variable type cannot be none: '" ^ id ^ "'"))
        | _ ->
        try match (StringMap.find id symbols) with
          _ -> raise (Failure ("Invalid redeclaration of variable '" ^ id ^ "'"))
        with Not_found ->
          match e with
            | None -> 
              let new_symbols = StringMap.add id {typ = t; inited = false} symbols in
              (SVarDecl (b, None), new_symbols)
            | Some(e) ->
              let (e_t, _) = check_expr symbols e in
              if e_t <> t then
                raise (Failure (String.concat "" [
                  "Type mismatch for variable: '"; id; "'. ";
                  "Expect '"; (fmt_typ t); "'"; 
                  ", Got: '"; (fmt_typ e_t); "'"]))
              else let new_symbols = StringMap.add id {typ = t; inited = true} symbols in
              (SVarDecl (b, Some(check_expr symbols e)), new_symbols)
    
    in (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = check_stmt_list symbols func.body
    }
  in
  List.map check_func functions
