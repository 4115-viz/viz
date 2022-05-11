(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (structs, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Viz" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i1_t       = L.i1_type     context
  and void_t     = L.void_type    context
  and float_t    = L.double_type context 
  and str_t      = L.pointer_type (L.i8_type context)
  and arr_t      = L.pointer_type (L.i32_type context)

  in
  (* Return the LLVM type for a Viz type *)
  let rec ltype_of_typ struct_decls = function
      A.IntType   -> i32_t
    | A.BoolType  -> i1_t
    | A.NoneType -> void_t
    | A.StrType -> str_t
    | A.FloatType -> float_t
    | A.ListType(t, _) -> (match t with
      | Some(t) -> L.pointer_type (ltype_of_typ struct_decls t)
      | None -> failwith "Runtime error: unable to deduce the list's type")
    | A.StructType(name) -> L.pointer_type (fst (StringMap.find name struct_decls))
  in

  (* for casting error messages *)
  let string_of_type = function
    A.IntType   -> "IntType"
    | A.BoolType  -> "BoolType"
    | A.NoneType -> "NoneType"
    | A.StrType -> "StrType"
    | A.FloatType -> "FloatType"
    | A.ListType(_,_) -> "ListType"
    | A.StructType(_) -> "StructType"
  in

  (* viz Builtins *)

  (* Declaring print function *)
  let print_t = L.var_arg_function_type i32_t [| str_t |] in
  let print_func = L.declare_function "printf" print_t the_module in

  let print_list_t = L.var_arg_function_type i32_t [| arr_t |] in
  let print_list_func = L.declare_function "print_list" print_list_t the_module in

  (* cast int to double C lib function *)
  let int_to_double_t = L.var_arg_function_type float_t [| i32_t |] in
  let int_to_double_func = L.declare_function "int_to_double" int_to_double_t the_module in

  (* cast string to double C lib function *)
  let str_to_double_t = L.var_arg_function_type float_t [| str_t |] in
  let str_to_double_func = L.declare_function "str_to_double" str_to_double_t the_module in

  (* cast double to int using C lib function *)
  let double_to_int_t = L.var_arg_function_type i32_t [| float_t |] in
  let double_to_int_func = L.declare_function "double_to_int" double_to_int_t the_module in

  (* cast bool to int using C lib function *)
  let bool_to_int_t = L.var_arg_function_type i32_t [| i1_t |] in
  let bool_to_int_func = L.declare_function "bool_to_int" bool_to_int_t the_module in

  (* cast str to int using C lib function *)
  let str_to_int_t = L.var_arg_function_type i32_t [| str_t |] in
  let str_to_int_func = L.declare_function "str_to_int" str_to_int_t the_module in

  (* cast bool to string C lib function *)
  let bool_to_str_t = L.var_arg_function_type str_t [| i1_t |] in
  let bool_to_str_func = L.declare_function "bool_to_str" bool_to_str_t the_module in

  (* cast int to string C lib function *)
  let int_to_str_t = L.var_arg_function_type str_t [| i32_t |] in
  let int_to_str_func = L.declare_function "int_to_str" int_to_str_t the_module in

  (* cast bool to string C lib function *)
  let double_to_str_t = L.var_arg_function_type str_t [| float_t |] in
  let double_to_str_func = L.declare_function "double_to_str" double_to_str_t the_module in

  (* string concat library function *)
  let string_concat_t = L.var_arg_function_type str_t [| str_t ; str_t |] in
  let string_concat_func = L.declare_function "string_concat" string_concat_t the_module in

  (* string concat library function *)
  let str_len_func_t = L.var_arg_function_type i32_t [| str_t |] in
  let str_len_func = L.declare_function "str_len" str_len_func_t the_module in

   (* list length library function *)
  let pop_func_t = L.var_arg_function_type i32_t [| arr_t |] in
  let pop_func = L.declare_function "pop" pop_func_t the_module in

  let push_func_t = L.var_arg_function_type i32_t [| arr_t ; i32_t |] in
  let push_func = L.declare_function "push" push_func_t the_module in

  (* int list length library function *)
  let list_len_int_func_t = L.var_arg_function_type i32_t [| arr_t |] in
  let list_len_int_func = L.declare_function "list_len_int" list_len_int_func_t the_module in

  (* to_upper string library function *)
  let to_upper_func_t = L.var_arg_function_type str_t [| str_t |] in
  let to_upper_func = L.declare_function "to_upper" to_upper_func_t the_module in

  (* to_upper string library function *)
  let to_lower_func_t = L.var_arg_function_type str_t [| str_t |] in
  let to_lower_func = L.declare_function "to_lower" to_lower_func_t the_module in

  (* string comparison functions *)
  (* string_equals *)
  let string_equals_func_t = L.var_arg_function_type i32_t [| str_t ; str_t |] in
  let string_equals_func = L.declare_function "string_equals" string_equals_func_t the_module in
  
  (* strin not equal *)
  let string_not_equals_func_t = L.var_arg_function_type i32_t [| str_t ; str_t|] in
  let string_not_equals_func = L.declare_function "string_not_equals" string_not_equals_func_t the_module in

  (* string lt *)
  let string_lt_func_t = L.var_arg_function_type i32_t [| str_t ; str_t |] in
  let string_lt_func = L.declare_function "string_lt" string_lt_func_t the_module in

  (* string gt *)
  let string_gt_func_t = L.var_arg_function_type i32_t [| str_t ; str_t |] in
  let string_gt_func = L.declare_function "string_gt" string_gt_func_t the_module in

  (* string lte *)
  let string_lte_func_t = L.var_arg_function_type i32_t [| str_t ; str_t |] in
  let string_lte_func = L.declare_function "string_lte" string_lte_func_t the_module in

  (* string gte *)
  let string_gte_func_t = L.var_arg_function_type i32_t [| str_t ; str_t |] in
  let string_gte_func = L.declare_function "string_gte" string_gte_func_t the_module in
  
  (* Format strings for printing *) 
  let int_format_str builder = L.build_global_stringptr "%d\n" "fmt" builder 
  and str_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder
  and float_format_str builder = L.build_global_stringptr "%f\n" "fmt" builder 
  in

  let struct_decls : (L.lltype * sstruct_def) StringMap.t =
    let add_struct m struct_decl =
      let name = struct_decl.sname in
      let members = Array.of_list 
        (List.map (fun (t, _) -> ltype_of_typ m t) struct_decl.smembers)
      in
      let struct_type = L.named_struct_type context name in
      L.struct_set_body struct_type members false;
      StringMap.add name (struct_type, struct_decl) m
    in
    List.fold_left add_struct StringMap.empty structs
  in

  let find_struct_decl name = try snd (StringMap.find name struct_decls)
  with Not_found -> raise (Failure ("undeclared struct " ^ name))
  in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ struct_decls t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ struct_decls fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars : (L.llvalue StringMap.t) =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ struct_decls t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m
      in
      List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function))
    in

    (* Return the value for a variable or formal argument.
       Check local names *)
    let lookup local_vars n = try StringMap.find n local_vars
      with Not_found -> raise (Failure ("undeclared identifier " ^ n))
    in

    (* Construct code for an expression; return its value *)
    let rec build_expr local_vars builder ((t, e) : sexpr) = match e with
        SStrLit s -> L.build_global_stringptr s "str" builder
      | SIntLit i  -> L.const_int i32_t i
      | SFloatLit f -> L.const_float float_t f
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SNoneLit -> L.const_null void_t
      | SListLit sl -> 
        let all_elem = List.map (fun e ->
          build_expr local_vars builder e) sl in
        let num_elems = List.length sl in
        let lllist_t = match num_elems with
          | 0 -> (match t with
            | ListType(Some(list_ele_t), _) -> ltype_of_typ struct_decls list_ele_t
            | _ -> failwith "Runtime error: unexpected error during the semantical check of empty ListLit"
          )
          | _ -> L.type_of (List.hd all_elem)
        in
        let ptr = L.build_array_malloc lllist_t (L.const_int i32_t num_elems) "" builder 
        in
        ignore (List.fold_left (fun i elem ->
          let idx = L.const_int i32_t i in
          let list_gep = L.build_in_bounds_gep ptr [|idx|] "" builder in
          let _ = (L.build_store elem list_gep builder) 
          in i+1)
        0 all_elem); ptr
      | SAssign (l_spe, r_e) -> 
        let r_val = (build_expr local_vars) builder r_e in
        (match l_spe with
        (* If left is a variable, simply store the r_val into the address of that variable *)
        | (_, SId id) -> 
          ignore(L.build_store r_val (lookup local_vars id) builder); r_val
        | (_, SMemberAccess((t, spx), member_id)) ->
            (match spx with
            (* The expression before dot is a @variable *)
            | SId id -> 
              let struct_name = match t with
                | StructType(s) -> s
                | _ -> failwith (String.concat "" [id ^ " is not a struct"])
              in
              let struct_addr = lookup local_vars id in
              let llname = String.concat "" [id; "_"; member_id] in
              let struct_decl = find_struct_decl struct_name in
              let struct_addr_load = L.build_load struct_addr ("struct_" ^ id) builder in
              let get_member_idx (sd: sstruct_def) member = 
                let rec find idx = function
                  | [] -> failwith ("struct member " ^ member_id ^ " undefined.")
                  | (_, name) :: _ when name = member -> idx
                  | (_) :: tl -> find (idx + 1) tl
                in find 0 sd.smembers
              in
              let idx = get_member_idx struct_decl member_id in
              let member_addr = L.build_struct_gep struct_addr_load idx llname builder in
              ignore(L.build_store r_val member_addr builder); r_val
            (* The expression before is another postfix expression: recursively evaluate its value *)
            | _ -> ignore(L.build_store r_val (lookup local_vars member_id) builder); r_val)
        | (typ, SSubscript(spe, idx_sexpr)) ->
            let list_v = build_expr local_vars builder (typ, SPostfixExpr spe) in
            let idx_v = build_expr local_vars builder idx_sexpr in
            let ptr =
              L.build_gep list_v [|idx_v|] "Subscript Assign" builder
            in
            ignore(L.build_store r_val ptr builder); r_val
        )
      | SBinop ((op_ret_type, _ ) as e1, op, e2) ->
        (
        let e1' = (build_expr local_vars) builder e1
        and e2' = (build_expr local_vars) builder e2 in
        match op_ret_type with 
          | A.IntType -> (* const_fneg for arithmetic negation, const_neg for negation of int *)
                  (match op with
                    A.Add     -> L.build_add
                  | A.Sub     -> L.build_sub
                  | A.Mult    -> L.build_mul
                  | A.Div     -> 
                    (ignore(L.string_of_llvalue (int_format_str builder));
                    if L.is_null e2' then raise (Failure "Divison by zero")
                    else L.build_sdiv)
                  | A.Mod     -> L.build_srem
                  | A.Eq      -> L.build_icmp L.Icmp.Eq
                  | A.Neq     -> L.build_icmp L.Icmp.Ne
                  | A.Less    -> L.build_icmp L.Icmp.Slt
                  | A.Great   -> L.build_icmp L.Icmp.Sgt
                  | A.Leq     -> L.build_icmp L.Icmp.Sle
                  | A.Geq     -> L.build_icmp L.Icmp.Sge
                  | _ -> raise ((Failure "TODO: Unimplemented Binary Op for Ints"))
                  ) e1' e2' "tmp" builder
          | A.FloatType -> 
            (match op with
                    A.Add     -> L.build_fadd
                  | A.Sub     -> L.build_fsub
                  | A.Mult    -> L.build_fmul
                  | A.Div     -> 
                    (ignore(L.string_of_llvalue (float_format_str builder));
                    if L.is_null e2' then raise (Failure "Divison by zero")
                    else L.build_fdiv)
                  | A.Eq      -> L.build_fcmp L.Fcmp.Oeq
                  | A.Neq     -> L.build_fcmp L.Fcmp.One
                  | A.Less    -> L.build_fcmp L.Fcmp.Olt
                  | A.Great   -> L.build_fcmp L.Fcmp.Ogt
                  | A.Leq     -> L.build_fcmp L.Fcmp.Ole
                  | A.Geq     -> L.build_fcmp L.Fcmp.Oge
                  | _ -> failwith "TODO: Unimplemented Binary Op for Floats"
                  ) e1' e2' "tmp" builder
          | A.BoolType ->
            (match op with
            | A.Eq      -> L.build_icmp L.Icmp.Eq
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | A.And     -> L.build_and
            | A.Or      -> L.build_or
            | _ -> failwith "Unimplemented Binary Op for Bools"
            ) e1' e2' "tmp" builder
          | A.StrType -> 
            ( match op with
            | A.Add -> 
              L.build_call string_concat_func [| e1' ; e2'|]
              "string_concat" builder
            | A.Eq -> 
              L.build_call string_equals_func [| e1' ; e2'|]
              "string_equals" builder
            | A.Neq -> 
              L.build_call string_not_equals_func [| e1' ; e2'|]
              "string_not_equals" builder
            | A.Leq -> 
              L.build_call string_lte_func [| e1' ; e2'|]
              "string_lte" builder
            | A.Geq ->
              L.build_call string_gte_func [| e1' ; e2'|]
              "string_gte" builder
            | A.Less -> 
              L.build_call string_lt_func [| e1' ; e2'|]
              "string_lt" builder
            | A.Great ->
              L.build_call string_gt_func [| e1' ; e2'|]
              "string_gt" builder
            | _ -> raise ((Failure "Unimplemented Binary Op for StrType"))
            )
          | A.NoneType -> failwith "TODO: Unimplemented Binary Op for NoneType"
          | A.ListType _ -> failwith "TODO: Unimplemented Binary Op for ListType"
          | A.StructType _ -> failwith "does not support binary operation for struct"
        )
      | SUnop (op, ((ret_ty, _ ) as e)) ->
        (
          let e' = (build_expr local_vars) builder e in
          match ret_ty with 
            | A.BoolType  -> 
              (match op with 
              | A.Not -> L.build_not e' "tmp" builder
              | _ -> failwith "Unimplemented Unary Op for BoolType"
              )
            | A.IntType   ->
              (match op with 
              | A.Neg -> L.build_neg e' "tmp" builder
              | _ -> failwith "Unimplemented Unary Op for IntType"
              )
            | A.FloatType ->
              (match op with 
              | A.Neg -> L.build_fneg e' "tmp" builder
              | _ -> failwith "Unimplemented Unary Op for FloatType"
              )
            | A.StrType   -> failwith "Unimplemented Unary Op for StrType"
            | A.NoneType  -> failwith "Unimplemented Unary Op for NoneType"
            | A.ListType _  -> failwith "Unimplemented Unary Op for NoneType"
            | A.StructType _ -> failwith "Does not import unary operation for StructType"
        )
      | SFuncCall("println", [])   -> 
        L.build_call print_func [| str_format_str builder; ((build_expr local_vars) builder (A.StrType, SStrLit("")))|]
        "printf" builder
      | SFuncCall("print", [e])   -> 
        L.build_call print_func [| str_format_str builder; ((build_expr local_vars) builder e)|]
        "printf" builder
      | SFuncCall("print_int", [e])   -> 
          L.build_call print_func [| int_format_str builder; ((build_expr local_vars) builder e)|]
          "printf" builder
      | SFuncCall("print_float", [e]) -> 
          L.build_call print_func [| float_format_str builder; ((build_expr local_vars) builder e)|]
          "printf" builder
      | SFuncCall("print_bool", [e])  -> 
          L.build_call print_func [| int_format_str builder; ((build_expr local_vars) builder e)|]
          "printf" builder
      | SFuncCall("print_list", [e])  -> 
          L.build_call print_list_func [| ((build_expr local_vars) builder e) |]
          "print_list" builder
      | SFuncCall("str_len", [e])  -> 
            L.build_call str_len_func [| ((build_expr local_vars) builder e)|]
            "str_len" builder
      | SFuncCall("pop", [e])  -> 
            L.build_call pop_func [| ((build_expr local_vars) builder e)|]
            "pop" builder
      | SFuncCall("push", [e;f])  -> 
            L.build_call push_func [| ((build_expr local_vars) builder e); ((build_expr local_vars) builder f)|]
            "push" builder      
      | SFuncCall("list_len_int", [e])  -> 
            L.build_call list_len_int_func [| ((build_expr local_vars) builder e)|]
            "list_len_int" builder
      | SFuncCall("to_upper", [e])  -> 
            L.build_call to_upper_func [| ((build_expr local_vars) builder e)|]
            "to_upper" builder
      | SFuncCall("to_lower", [e])  -> 
            L.build_call to_lower_func [| ((build_expr local_vars) builder e)|]
            "to_lower" builder
      | SFuncCall (f, args) ->
        let (fdef, _) = StringMap.find f function_decls in
        let llargs = List.rev (List.map ((build_expr local_vars) builder) (List.rev args)) in
        (*let result = f ^ "_result" in *)
        
        (* had to borrow this from Edward's MicroC to complete the NoneType return call *)

        let result = (match fdecl.srtyp with 
                        A.NoneType -> ""
                      | _ -> f ^ "_result") in

        L.build_call fdef (Array.of_list llargs) result builder
      | STypeCast(typ, ((ty_exp, _) as e)) -> 
          (
            let type_cast_err e1 e2 = 
                raise (Failure("Cast type not supported from " ^ 
                              string_of_type e1 ^ " to " ^ 
                              string_of_type e2)) 
            in
            (* typ is what we mywould like to cast the expr to *)
            match typ with
            | IntType -> 
                  (match ty_exp with 
                    | IntType   -> ((build_expr local_vars) builder e)
                    | BoolType  ->
                          (                    
                            L.build_call bool_to_int_func [| ((build_expr local_vars) builder e)|]
                            "bool_to_int" builder
                          )
                    | FloatType ->
                          (                    
                            L.build_call double_to_int_func [| ((build_expr local_vars) builder e)|]
                            "double_to_int" builder
                          )
                    | StrType ->
                          (                    
                            L.build_call str_to_int_func [| ((build_expr local_vars) builder e)|]
                            "str_to_int" builder
                          )                          
                    | _ -> type_cast_err ty_exp typ
                  )
            | FloatType -> 
              (match ty_exp with 
                | IntType   -> 
                    (
                    L.build_call int_to_double_func [| ((build_expr local_vars) builder e)|]
                    "int_to_double" builder
                    )
                | FloatType -> ((build_expr local_vars) builder e)
                | StrType ->
                  (                    
                    L.build_call str_to_double_func [| ((build_expr local_vars) builder e)|]
                    "str_to_double" builder
                  )   
                | _ -> type_cast_err ty_exp typ
              )

            | StrType ->
              (
                match fst(e) with
                | IntType ->
                  (                    
                    L.build_call int_to_str_func [| ((build_expr local_vars) builder e)|]
                    "int_to_str" builder
                  )   
                | BoolType -> 
                  (                    
                    L.build_call bool_to_str_func [| ((build_expr local_vars) builder e)|]
                    "bool_to_str" builder
                  )   
                | FloatType ->
                  (                    
                    L.build_call double_to_str_func [| ((build_expr local_vars) builder e)|]
                    "double_to_str" builder
                  )   
                | StrType -> ((build_expr local_vars) builder e)
                | _ -> type_cast_err ty_exp typ
              )
            | _ -> type_cast_err ty_exp typ
        )
      | SPostfixExpr (typ, spx) -> (match spx with
        | SId id -> L.build_load (lookup local_vars id) id builder
        | SSubscript (spe, idx_sexpr) -> 
          let list_v = build_expr local_vars builder (typ, SPostfixExpr spe) in
          let idx_v = build_expr local_vars builder idx_sexpr in
          L.build_load (L.build_gep list_v [| idx_v |] "subscript" builder) "" builder
        | SMemberAccess((t, spx), member_id) ->
          (match spx with
          (* The expression before dot is a @variable *)
          | SId id -> 
            let struct_name = match t with
              | StructType(s) -> s
              | _ -> failwith (String.concat "" [id ^ " is not a struct"])
            in
            let struct_addr = lookup local_vars id in
            let llname = String.concat "" [id; "_"; member_id] in
            let struct_decl = find_struct_decl struct_name in
            let struct_addr_load = L.build_load struct_addr ("struct_" ^ id) builder in
            let get_member_idx (sd: sstruct_def) member = 
              let rec find idx = function
                | [] -> failwith ("struct member " ^ member_id ^ " undefined.")
                | (_, name) :: _ when name = member -> idx
                | (_) :: tl -> find (idx + 1) tl
              in find 0 sd.smembers
            in
            let idx = get_member_idx struct_decl member_id in
            let member_addr = L.build_struct_gep struct_addr_load idx llname builder in
            L.build_load member_addr ("load_" ^ llname) builder
          (* The expression before is another postfix expression: recursively evaluate its value *)
          | _ -> failwith "TODO: nested member access."))
      in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    (*return (local_vars, builder) *)
    let rec build_stmt local_vars builder = function
        SBlock sl -> List.fold_left (fun (local, b) -> build_stmt local b) (local_vars, builder) sl
      | SID_Block sl -> ignore(build_stmt local_vars builder  (SBlock(sl)) ); (local_vars, builder)
      | SExpr e -> ignore((build_expr local_vars) builder e); (local_vars, builder)
      (*| SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder*)
      (* borrowed from Edward's MicroC so that we can return void types *)
      | SReturn e -> ignore(match fdecl.srtyp with
                              (* Special "return nothing" instr *)
                              A.NoneType -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret ((build_expr local_vars) builder e) builder );
                     (local_vars, builder)
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = (build_expr local_vars) builder predicate in
	      let merge_bb = L.append_block context "merge" the_function in
        let build_br_merge = L.build_br merge_bb in (* partial function *)

	      let then_bb = L.append_block context "then" the_function in
	      add_terminal (snd ((build_stmt local_vars) (L.builder_at_end context then_bb) then_stmt)) 
        build_br_merge;

	      let else_bb = L.append_block context "else" the_function in
	      add_terminal (snd ((build_stmt local_vars) (L.builder_at_end context else_bb) else_stmt)) 
        build_br_merge;

	      ignore(L.build_cond_br bool_val then_bb else_bb builder);
	      (local_vars, L.builder_at_end context merge_bb)
  
      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = (build_expr local_vars) while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (snd ((build_stmt local_vars) (L.builder_at_end context body_bb) body)) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        (local_vars, L.builder_at_end context end_bb)
      (*| SFor(var_init, predicate, update, block_code) -> builder (* TODO: SFor *)*)
      (* | SFor(_, _, _, _) -> (local_vars, builder) *)
      | SFor (e1, e2, e3, body) -> build_stmt local_vars builder
	      ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
      | SVarDeclList vl -> List.fold_left (
        fun (local, b) v -> build_stmt local b (SVarDecl v))
        (local_vars, builder) vl
      | SVarDecl ((t, id), e) ->
        let local_var = match t with
          | StructType(name) -> 
            let struct_ptr_t = ltype_of_typ struct_decls t in
            let struct_type = L.element_type struct_ptr_t in
            let struct_addr = L.build_alloca struct_ptr_t name builder in 
            let struct_val = L.build_malloc struct_type name builder in
            ignore(L.build_store struct_val struct_addr builder);
            struct_addr
          | _ -> L.build_alloca (ltype_of_typ struct_decls t) ("var_" ^ id) builder
        in
        let new_local_vars = StringMap.add id local_var local_vars in
        (* Handle the uninitalized case *)
        let _ = (match e with
          | Some se -> 
            let e' = build_expr new_local_vars builder se in
            ignore(L.build_store e' local_var builder);
          (* rhs is uninitialized *)
          | _ -> ()
        ) in
        new_local_vars, builder
    in
    (* Build the code for each statement in the function *)
    let func_builder local_vars = snd ((build_stmt local_vars) builder (SBlock fdecl.sbody)) in

    (* Add a return if the last block falls off the end *)
    (*add_terminal func_builder (L.build_ret (L.const_int i32_t 0))*)
    
    (* borrowed from Edward's MicroC so we can get non int return values to compile*)
    add_terminal (func_builder local_vars) (match fdecl.srtyp with
        A.NoneType -> L.build_ret_void
      | A.FloatType -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ struct_decls t) 0))
  in

  List.iter build_function_body functions;
  the_module