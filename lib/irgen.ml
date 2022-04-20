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
let translate (functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Viz" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  (* and i8_t       = L.i8_type     context *)
  and i1_t       = L.i1_type     context
  and void_t     = L.void_type    context
  and float_t    = L.double_type context 
  and str_t      = L.pointer_type   (L.i8_type context)
  in
  (* Return the LLVM type for a Viz type *)
  let rec ltype_of_typ = function
      A.IntType   -> i32_t
    | A.BoolType  -> i1_t
    | A.NoneType -> void_t
    | A.StrType -> str_t
    | A.FloatType -> float_t
    | A.ArrayType(t, _) -> match t with
      | Some(t) -> L.pointer_type (ltype_of_typ t)
      | None -> failwith "Runtime error: unable to deduce the array's type"
  in

  (* Declaring print function *)
  let print_t = L.var_arg_function_type i32_t [| str_t |] in
  let print_func = L.declare_function "printf" print_t the_module in

  (* Format strings for printing *) 
  let int_format_str builder = L.build_global_stringptr "%d\n" "fmt" builder 
  and str_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder
  and float_format_str builder = L.build_global_stringptr "%f\n" "fmt" builder 
  in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names *)
    let lookup local_vars n = try StringMap.find n local_vars
      with Not_found -> raise (Failure ("undeclared identifier " ^ n))
    in

    (* Construct code for an expression; return its value *)
    let rec build_expr local_vars builder ((_, e) : sexpr) = match e with
        SStrLit s -> L.build_global_stringptr s "str" builder
      | SIntLit i  -> L.const_int i32_t i
      | SFloatLit f -> L.const_float float_t f
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SNoneLit -> L.const_null void_t
      | SArrayLit sa -> 
        (match sa with
        | [] -> raise (Failure "TODO: empty array")
        | sa ->
            let all_elem = List.map (fun e ->
              build_expr local_vars builder e) sa in
            let llarray_t = L.type_of (List.hd all_elem) in
            let num_elems = List.length sa in
            let ptr = L.build_array_malloc llarray_t
                (L.const_int i32_t num_elems) "" builder 
            in
            ignore (List.fold_left (fun i elem ->
                let idx = L.const_int i32_t i in
                let eptr = L.build_gep ptr [|idx|] "" builder in
                let cptr = L.build_pointercast eptr 
                    (L.pointer_type (L.type_of elem)) "" builder in
                let _ = (L.build_store elem cptr builder) 
                in i+1)
                0 all_elem); ptr)
      | SId s       -> L.build_load (lookup local_vars s) s builder
      | SAssign (s, e) -> let e' = (build_expr local_vars) builder e in
        ignore(L.build_store e' (lookup local_vars s) builder); e'
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
                  | _ -> raise ((Failure "TODO: Unimplemented Binary Op for Floats"))
                  ) e1' e2' "tmp" builder
          | A.BoolType ->
            (match op with
            | A.Eq      -> L.build_icmp L.Icmp.Eq
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | A.And     -> L.build_and
            | A.Or      -> L.build_or
            | _ -> raise ((Failure "Unimplemented Binary Op for Bools"))
            ) e1' e2' "tmp" builder
          | A.StrType -> raise ((Failure "TODO: Unimplemented Binary Op for StrType"))
          | A.NoneType -> raise ((Failure "TODO: Unimplemented Binary Op for NoneType"))
          | A.ArrayType _ -> raise ((Failure "TODO: Unimplemented Binary Op for ArrayType"))
        )
      | SUnop (op, ((ret_ty, _ ) as e)) ->
        (
          let e' = (build_expr local_vars) builder e in
          match ret_ty with 
            | A.BoolType  -> 
              (match op with 
              | A.Not -> L.build_not e' "tmp" builder
              | _ -> raise ((Failure "Unimplemented Unary Op for BoolType"))
              )
            | A.IntType   ->
              (match op with 
              | A.Neg -> L.build_neg e' "tmp" builder
              | _ -> raise ((Failure "Unimplemented Unary Op for IntType"))
              )
            | A.FloatType ->
              (match op with 
              | A.Neg -> L.build_fneg e' "tmp" builder
              | _ -> raise ((Failure "Unimplemented Unary Op for FloatType"))
              )
            | A.StrType   -> raise ((Failure "Unimplemented Unary Op for StrType"))
            | A.NoneType  -> raise ((Failure "Unimplemented Unary Op for NoneType"))
            | A.ArrayType _  -> raise ((Failure "Unimplemented Unary Op for NoneType"))
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
      | SFuncCall (f, args) ->
        let (fdef, _) = StringMap.find f function_decls in
        let llargs = List.rev (List.map ((build_expr local_vars) builder) (List.rev args)) in
        (*let result = f ^ "_result" in *)
        
        (* had to borrow this from Edward's MicroC to complete the NoneType return call *)

        let result = (match fdecl.srtyp with 
                        A.NoneType -> ""
                      | _ -> f ^ "_result") in

        L.build_call fdef (Array.of_list llargs) result builder
      | SSubscript (arr_e, idx_e) -> 
        let arr_v = build_expr local_vars builder arr_e in
        let idx_v = build_expr local_vars builder idx_e in
        L.build_load (L.build_gep arr_v [| idx_v |] "subscript" builder) "" builder
      (*| STypeCast(_, e) -> (* TODO: Below is just a placeholder. *)
        L.build_call print_func [| ((build_expr local_vars) builder e) |]
        "print" builder *)
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
        let local_var = L.build_alloca (ltype_of_typ t) id builder in
        let new_local_vars = StringMap.add id local_var local_vars in
        let _ = (match e with
          | Some se -> 
            let e' = build_expr new_local_vars builder se in
            ignore(L.build_store e' local_var builder);
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
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module