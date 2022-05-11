(* Semantic checking for the Viz compiler *)

open Ast
open Sast
open Ast_fmt

module StringMap = Map.Make(String)
module StringHash = Hashtbl.Make(struct
  type t = string
  let equal x y = x = y
  let hash = Hashtbl.hash
end)

(* Used to query a struct using it's name from a StringMap *)
type struct_symbol = {
  members: typ StringMap.t
}

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each function *)

let check ((structs: struct_def list), (functions: func_def list)) =

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* built-in declarations *)
  let built_in_decls =
    let add_built_in_function map (ret_type, name, f) = StringMap.add name {
      rtyp = ret_type;
      fname = name;
      formals = f;
      body = [] } map
      in List.fold_left add_built_in_function StringMap.empty [
                                                               (NoneType, "print", [StrType, "x"]);
                                                               (NoneType, "print_int", [IntType, "x"]);
                                                               (NoneType, "print_float", [FloatType, "x"]);
                                                               (NoneType, "print_bool", [BoolType, "x"]);
                                                               (NoneType, "println", []);
                                                               (NoneType,"print_list", [(ListType (Some(IntType), None)), "x"]);
                                                               (IntType, "str_len", [StrType, "x"]);
                                                               (StrType, "to_upper", [StrType, "x"]);
                                                               (StrType, "to_lower", [StrType, "x"]);
                                                               (IntType, "list_len", [StrType, "x"]);
                                                               (IntType, "list_len_int", [(ListType (Some(IntType), None)), "x"]);
                                                               (IntType, "pop", [(ListType (Some(IntType), None)), "x"]);
                                                               (StrType, "push", [(ListType (Some(IntType), None)), "x"; IntType,"x"]);
                                                               ]
  in

  (* Add Struct to the symbol table *)
  let add_struct map (s:struct_def) =
    let name = s.name in
    let add_member map (member:bind) =
      match member with
      | (_, m_id) when StringMap.mem m_id map -> 
        failwith ("Duplicate struct member: " ^ m_id)
      | (m_typ, m_id) -> StringMap.add m_id m_typ map 
    in
    let (symbol:struct_symbol) = {
      members = List.fold_left add_member StringMap.empty s.members;
    } 
  in
    match s with (* No duplicate structs or redefinitions of built-ins *)
    | _ when StringMap.mem name map -> failwith ("duplicate struct " ^ name)
    | _ ->  StringMap.add name symbol map
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let name = fd.fname in
    match fd with (* No duplicate functions or redefinitions of built-ins *)
    | _ when StringMap.mem name built_in_decls -> 
      failwith ("function " ^ name ^ " may not be defined, it is reserved")
    | _ when StringMap.mem name map -> 
      failwith ("duplicate function " ^ name)
    | _ -> StringMap.add name fd map
  in

  (* Collect all struct names into one symbol table *)
  let struct_decls = List.fold_left add_struct StringMap.empty structs
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a struct from our symbol table *)
  let find_struct name: struct_symbol =
    try StringMap.find name struct_decls
    with Not_found -> raise (Failure ("unrecognized struct: " ^ name))
  in

  (* Given a struct symbol, and the member name, return the type of that member*)
  let find_struct_member (symbol: struct_symbol) (member_id:string): typ =
    try StringMap.find member_id symbol.members
    with Not_found -> raise (Failure ("unrecognized member: " ^ member_id))
  in


  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_struct (s:struct_def) =
    (* Make sure no members are void or duplicate *)
    check_binds "struct members" s.members;
    (* body of check_struct *)
    { 
      sname = s.name;
      smembers  = s.members;
    }
  in 

  let check_func func =
    (* Make sure no formals are void or duplicates *)
    check_binds "function formals" func.formals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign (lvaluet:typ) (rvaluet:typ): typ = match lvaluet with
    (* If the lhs and rhs are both list and have the same type, using the rhs list length *)
      | ListType(l_list_typ, _) -> (match rvaluet with
        | ListType(r_list_typ, _) when l_list_typ = r_list_typ -> rvaluet
        | _ -> failwith ("illegal assignment " ^ fmt_typ lvaluet ^ " = " ^ fmt_typ rvaluet)
      )
      | _ when lvaluet = rvaluet -> lvaluet
      | _ -> failwith ("illegal assignment " ^ fmt_typ lvaluet ^ " = " ^ fmt_typ rvaluet)
    in

    (* Build local symbol table of variables for this function *)
    let symbols : typ StringMap.t = 
      List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty (func.formals)
    in

    (* Query a variable from our local symbol table *)
    let type_of_id (id: string) (symbols : typ StringMap.t) : typ =
      try StringMap.find id symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ id))
    in

    let uninited_symbols = StringHash.create 10 in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr (symbols: typ StringMap.t) (e: expr) : (sexpr * typ StringMap.t) = 
      match e with
        IntLit x -> ((IntType, SIntLit x), symbols)
      | FloatLit x -> ((FloatType, SFloatLit x), symbols)
      | StrLit x -> ((StrType, SStrLit x), symbols)
      | BoolLit x -> ((BoolType, SBoolLit x), symbols)
      | NoneLit   -> ((NoneType, SNoneLit), symbols)
      | ListLit (a : expr list) -> (match a with
        | [] -> ((ListType (None, Some(0)), SListLit []), symbols)
        | (x :: _) as xxs -> let ((t, _), symbols) = check_expr symbols x in
          let sa = List.map (fun i -> match (check_expr symbols i) with
            | ((i_t, _), _) when i_t != t -> raise (Failure ("Invalid list literal. Expect type " ^ fmt_typ t ^ ", found" ^ fmt_typ i_t))
            | ((t', se'), _) -> (t', se')) xxs
          in
          let len = List.length xxs in
          ((ListType (Some(t), Some(len)), SListLit sa), symbols)
      )
      | Assign(pe, e) -> 
        let (l_t, _) as l_spe = match pe with
          | Id id -> (type_of_id id symbols, SId(id))
          | _ -> check_postfix_expr symbols pe
        in 
        let ((r_t, _) as r_se, symbols) = check_expr symbols e in
        let t = check_assign l_t r_t in
        (* If the left side is a variable, removed the symbol from uninited symbol table *)
        let new_symbols = (match pe with
        | Id id -> 
          (if StringHash.mem uninited_symbols id then StringHash.remove uninited_symbols id;
          (* update the id's type in the symbol table, in the case that list has been reassigned *)
          StringMap.add id t symbols)
        | _ -> symbols) in
        ((t, SAssign(l_spe, r_se)), new_symbols)

      | Binop(l, bo, r) as ex-> 
        let ((ltype, l'), symbols) = check_expr symbols l in
        let ((rtype, r'), symbols) = check_expr symbols r in
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
            | (Eq | Neq) -> BoolType
            | Add when ltype = StrType && rtype = StrType  -> StrType
            | (Leq | Geq | Less | Great) when (ltype = IntType && rtype = IntType ||
                                               ltype = FloatType && rtype = FloatType ||
                                               ltype = StrType && rtype = StrType) -> BoolType
            | (And | Or) when (ltype = BoolType && rtype = BoolType) -> BoolType
            | _ -> raise (Failure ("No operator (" ^ fmt_op bo ^ ") " ^ "to handle type (" ^
                          fmt_typ ltype ^ ", " ^ fmt_typ rtype))
            ) bo
        in ((final_type, SBinop((ltype, l'), bo, (rtype, r'))), symbols)
      | Unop(op, e) as ex -> 
        let ((t, e'), symbols) = check_expr symbols e in
        let ty = match op with
          Neg when t = IntType || t = FloatType -> t
        | Not when t = BoolType -> BoolType
        | _ -> raise (Failure ("illegal unary operator " ^ 
                                string_of_uop op ^ fmt_typ t ^
                                " in " ^ fmt_expr ex))
        in ((ty, SUnop(op, (t, e'))), symbols)
      | FuncCall(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ fmt_expr call))
        else 
        let check_call (ft, _) e =
          let ((et, e'), _) = check_expr symbols e in
          if fname = "print" then (et, e') (*convert to string*)
          else if fname = "list_len" then (et, e')
          else(check_assign ft et, e')
        in
        let args' = List.map2 check_call fd.formals args in
        let func_name = match fname with
          | "print" ->               
            (match (fst (List.hd args')) with
            | IntType -> "print_int"
            | FloatType -> "print_float"
            | StrType -> "print"
            | BoolType -> "print_bool"
            | NoneType -> failwith "Does not support print None type"
            | ListType _ -> "print_list"
            | StructType _ -> failwith "Does not support print custom Struct type"
          )
        | "str_len" -> 
          (
            match (fst (List.hd args')) with
            | StrType -> "str_len"
            | _ -> raise (Failure ("cannot get string length of non string"))
          )
        | "to_upper" | "to_lower" ->
          (
            match (fst (List.hd args')) with
            | StrType -> fname
            | _ -> raise (Failure ("invalid type for to_upper/to_lower"))
          )
        | "push" -> (
            match (fst (List.hd args')) with
            | ListType (Some(IntType), _) -> "push"
            | _ -> raise (Failure ("cannot push to parameter"))
          )
        | "pop" ->
          (
            match (fst (List.hd args')) with
            | ListType (Some(IntType), _) -> "pop"
            | _ -> raise (Failure ("cannot pop from parameter"))
          )
        | "list_len" -> 
          (
            match (fst (List.hd args')) with
            | ListType (Some(IntType), _) -> "list_len_int"
            | ListType (Some(StrType), _) -> raise (Failure ("need to support str arr len"))
            | ListType (Some(FloatType), _) -> raise (Failure ("need to support float arr len"))
            | ListType (Some(BoolType), _) -> raise (Failure ("need to support bool arr len"))
            | _ -> raise (Failure ("cannot get array length of non list"))
          )
        | _ -> fname
        in 
        ((fd.rtyp, SFuncCall(func_name, args')), symbols)
      | TypeCast(ty, expr) ->
        let ((ty_exp, var), symbols) = check_expr symbols expr in
         (
          let type_cast_err e1 e2 = 
            raise (Failure("Cast type not supported from " ^ 
                          fmt_typ e1 ^ " to " ^ 
                          fmt_typ e2)) in

           match ty with
              | IntType -> (
                if ty_exp = FloatType || ty_exp = IntType || ty_exp = BoolType || ty_exp = StrType
                    then ((ty , STypeCast(ty, (ty_exp, var))), symbols)
                else type_cast_err ty_exp ty
              )
              | FloatType -> (
                if ty_exp = IntType || ty_exp = FloatType || ty_exp = StrType 
                    then ((ty ,STypeCast(ty, (ty_exp, var))), symbols)
                else type_cast_err ty_exp ty
              )
              | StrType ->
                if ty_exp = IntType || ty_exp = FloatType || ty_exp = BoolType || ty_exp = StrType 
                then ((ty ,STypeCast(ty, (ty_exp, var))), symbols)
                else type_cast_err ty_exp ty
              | _ -> type_cast_err ty_exp ty
         )
      | PostfixExpr pe -> (match pe with
        | Id id -> if StringHash.mem uninited_symbols id 
          then failwith ("uninitialized local variable '" ^ id ^ "' used.")
        | _ -> ());
        let spe = check_postfix_expr symbols pe in
        ((fst spe, SPostfixExpr spe), symbols)

    and check_postfix_expr (symbols: typ StringMap.t) (pe: postfix_expr) : spostfix_expr =
      match pe with
      | Id id -> (type_of_id id symbols, SId id)
      | Subscript(list_pe, idx_expr) ->
        let (list_typ, _) as list_spe = check_postfix_expr symbols list_pe in
        let (idx_sexpr, _) = check_expr symbols idx_expr in
        let (idx:int) = match (idx_sexpr) with
          | IntType, SIntLit x -> x
          | IntType, SPostfixExpr (_ , SId _) -> 0
          | t, _ -> failwith (String.concat "" ["Subscript operator [] expect index to be int, got: "; fmt_typ t])
        in
        let (list_ele_typ, (len:int)) = match (list_typ) with
          | ListType (Some(t), Some(l)) -> (t, l) 
          | ListType (None, _) -> failwith "Runtime error: List type unknown."
          | ListType (_, None) -> failwith "Runtime error: List length unknown."
          | t -> failwith (String.concat "" ["Subscript operator [] expect list, got: "; fmt_typ t])
        in
        if idx >= len then failwith "Index out of range."
        else (list_ele_typ, SSubscript(list_spe, idx_sexpr))
      | MemberAccess (pe, member_id) ->
        (* Check the type of the given postfix expression is a struct *)
        let spe = check_postfix_expr symbols pe in
        let struct_name:string = match spe with
          | (StructType name, _) -> name
          | _ -> failwith "The type of the given postfix expression must be Struct."
        in
        (* Check the given struct existed *)
        let s_symbol = find_struct struct_name in
        (* Check the memebr_id exists in the given struct *)
        let member_type = find_struct_member s_symbol member_id in
        (member_type, SMemberAccess(spe, member_id))
    in
    
    let check_bool_expr symbols e =
      let ((t, e'), _) = check_expr symbols e in
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
        styp = f.typ;
        sname = f.name;
        sparams = f.params;
        sbody = check_program f.body;
      } something like ensure that these fields can be filled
    in *)
    let rec check_stmt_list symbols = function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list symbols (sl @ sl') (* Flatten blocks *)
      | s :: sl -> 
          let (stmt, new_symbols) = check_stmt symbols s 
          in
          stmt :: check_stmt_list (match s with
                                  | ID_Block _ -> symbols
                                  | _ -> new_symbols) sl
    
    and check_list_var_decl symbols ((list_ele_typ, id), list_e):(svar_decl * typ StringMap.t) = 
      let ((list_t, list_sx) as list_sexpr, symbols) = check_expr symbols list_e in
      (* match rhs expression type *)
      let (bind, sexpr) = (match list_t with
        (* rhs is empty list, we need to deduce the empty list's type *)
        | ListType (None, _) -> 
          let deduced_expr_t = ListType(Some(list_ele_typ), Some(0)) in
          ((deduced_expr_t, id), Some((deduced_expr_t, list_sx)))
        (* rhs has the same type as lhs *)
        | ListType (Some(expr_ele_t), _) when expr_ele_t == list_ele_typ -> 
          ((list_t, id), Some(list_sexpr))
        | _ -> failwith "Type mismatch, expected list.")
      in
      let new_symbols = StringMap.add id list_t symbols in
      ((bind, sexpr), new_symbols)

    and check_var_decl symbols (((t, id) as b, e):var_decl):(svar_decl * typ StringMap.t) =
      match e with
      | None -> (* rhs is empty *)
        let new_symbols = StringMap.add id t symbols in
        (* Struct don't need to be initialized *)
        (match t with
        | StructType _ -> ()
        | _ -> StringHash.add uninited_symbols id true);
        ((b, None), new_symbols)
      | Some(e) -> match t with (* match variable's type *)
        | ListType (Some(list_ele_typ), None) -> (* Special case: lhs is list type *)
          check_list_var_decl symbols ((list_ele_typ, id), e)
        | _ -> let ((expr_t, _) as e_sexpr, symbols) = check_expr symbols e in
          if t <> expr_t then
            failwith (String.concat "" [
              "Type mismatch for variable: '"; id; "'. ";
              "Expect '"; (fmt_typ t); "'"; 
              ", Got: '"; (fmt_typ expr_t); "'"])
          else let new_symbols = StringMap.add id t symbols in
          ((b, Some(e_sexpr)), new_symbols) 

    and check_var_decl_list symbols (dl: var_decl list): (svar_decl list * typ StringMap.t) = match dl with
      | [] -> ([], symbols)
      | x :: xs -> 
        let (svar_decl, new_symbols) = check_var_decl symbols x in
        let (svar_decl_list, new_symbols) = check_var_decl_list new_symbols xs in
        (svar_decl :: svar_decl_list, new_symbols)
          
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt (symbols : typ StringMap.t) = function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> (SBlock (check_stmt_list symbols sl), symbols)
      | ID_Block sl -> (SID_Block (check_stmt_list symbols sl), symbols)
      | Expr e -> 
        let sexpr, symbols = check_expr symbols e in
        (SExpr sexpr, symbols)
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
          let se1, symbols = check_expr symbols e1 in
          let se3, symbols = check_expr symbols e3 in
          (SFor(se1, check_bool_expr symbols e2, se3, fst (check_stmt symbols st)), symbols)
      | Return e ->
        let (t, e'), symbols = check_expr symbols e in
        if t = func.rtyp then (SReturn (t, e'), symbols)
        else raise (
            Failure ("return gives " ^ fmt_typ t ^ " expected " ^
                     fmt_typ func.rtyp ^ " in " ^ fmt_expr e))
      | VarDecl (((t, id), _) as vd) -> (match t with
        | NoneType -> raise (Failure ("Variable type cannot be none: '" ^ id ^ "'"))
        | _ -> let (svd, sym) = check_var_decl symbols vd in (SVarDecl(svd), sym))

      | VarDeclList var_decls_list -> 
        let (s_var_decl_list, new_symbols) = check_var_decl_list symbols var_decls_list
        in
        (SVarDeclList s_var_decl_list, new_symbols)

    in (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      sbody = check_stmt_list symbols func.body
    }
  in
  (List.map check_struct structs, List.map check_func functions)