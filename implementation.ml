open Back
open Builtin
open Emit
open Error
open Front
open Global
open Instruction
open Lambda
open Printer
open Syntax
open Type
open Typing

let stage = ref 3
let verbose = ref false

let typing_impl_expr loc e =
  push_level();
  let ty = typing_expr [] e in
  pop_level();
  if should_generate e then
    gen_type ty;
  ty

let submit_variant ty_res cs =
  let n = List.length cs in
  let rec go i acc = function
    | [] -> List.rev acc
    | (name,arg)::xs ->
        match arg with
        | None ->
            let constr =
              { qualid=Lident name
              ; info={ cs_res=ty_res
                     ; cs_arg=type_unit
                     ; cs_tag=Constr_tag_regular(n,i)
                     ; cs_kind=Constr_constant
                     }
              }
            in
            add_global_constr constr;
            go (i+1) (constr::acc) xs
        | Some arg ->
            let ty_arg = type_of_type_expression true arg in
            (* TODO kind *)
            let constr =
              { qualid=Lident name
              ; info={ cs_res=ty_res
                     ; cs_arg=ty_arg
                     ; cs_tag=Constr_tag_regular(n,i)
                     ; cs_kind=Constr_regular
                     }
              }
            in
            add_global_constr constr;
            go (i+1) (constr::acc) xs
  in
  let cds = go 0 [] cs in
  pop_level();
  gen_type ty_res;
  List.iter (fun cd -> gen_type cd.info.cs_arg) cds;
  Variant_type cds

let typing_impl_typedef loc decl : (typ * type_components) list =
  let submit (name,args,def) =
    let ty_constr =
      { qualid=Lident name
      ; info={ ty_stamp=new_type_stamp(); ty_abbr=Tnotabbrev }
      } in
    let ty_desc =
      { qualid=Lident name
      ; info={ ty_constr=ty_constr; ty_arity=List.length args;
               ty_desc=Abstract_type }
      } in
    add_global_type ty_desc;
    ty_desc,args,def
  in
  let submit_abbrev ty_constr ty_params body =
    let ty = type_of_type_expression true body in
    pop_level();
    gen_type ty;
    List.iter gen_type ty_params;
    ty_constr.info.ty_abbr <- Tabbrev(ty_params, ty);
    Abbrev_type(ty_params, ty)
  in
  let define (ty_desc,args,def) =
    push_level();
    let ty_args =
      try
        bind_te_vars args
      with Failure "bind_te_vars" ->
        duplicate_param_in_type_decl_err loc
    in
    let ty_res =
      { typ_desc=Tconstr(ty_desc.info.ty_constr, ty_args)
      ; typ_level=notgeneric
      } in
    let ty_comp =
      match def with
      | Ptd_abstract ->
          pop_level();
          Abstract_type
      | Ptd_variant cs ->
          (* pop_level() included *)
          submit_variant ty_res cs
      | Ptd_alias te ->
          (* pop_level() included *)
          submit_abbrev ty_desc.info.ty_constr ty_args te
    in
    ty_desc.info.ty_desc <- ty_comp;
    ty_res, ty_comp
  in
  let decl = List.map submit decl in
  let r = List.map define decl in
  List.iter (fun (ty_desc,_,_) ->
    check_recursive_abbrev ty_desc.info.ty_constr
  ) decl;
  r

let typing_impl_excdef loc (name,arg) : constr_desc global =
  let cd =
    begin match arg with
    | None ->
        let id = Lident name in
        { qualid=id
        ; info={ cs_res=type_exn
               ; cs_arg=type_unit
               ; cs_tag=Constr_tag_extensible(id,new_type_stamp())
               ; cs_kind=Constr_constant
               }
        }
    | Some arg ->
        let id = Lident name in
        let ty_arg = type_of_type_expression true arg in
        { qualid=id
        ; info={ cs_res=type_exn
               ; cs_arg=ty_arg
               ; cs_tag=Constr_tag_extensible(id,new_type_stamp())
               ; cs_kind=Constr_regular
               }
        }
    end
  in
  add_global_constr cd;
  cd

let typing_impl_letdef loc isrec pes =
  push_level();
  let tys = List.map (fun _ -> new_type_var()) pes in
  let env = typing_pat_list [] (List.map fst pes) tys in
  let submit () =
    List.iter (fun (name,ty) ->
      add_global_value
      { qualid=Lident name
      ; info={ v_typ=ty; v_prim=Not_prim }
      }) env in
  if isrec then
    submit();
  List.iter2 (fun (_,e) ty ->
    typing_expect [] e ty
  ) pes tys;
  pop_level();

  let gens = List.map (fun (_,e) -> should_generate e) pes in
  (*List.iter (dump_typ 0) tys;*)
  List.iter2 (fun gen ty ->
    if gen then (
      (*print_endline "+ gen";*)
      gen_type ty
    )
    else (
      (*print_endline "+ restrict";*)
      value_restrict ty
    )
  ) gens tys;
  (*List.iter (dump_typ 0) tys;*)
  if not isrec then
    submit();
  env

let process_lambda oc lambda =
  if !stage = 2 then
    dump_lambda 0 lambda
  else if !stage >= 3 then (
    let init, fcts as code = compile_lambda lambda in
    if !stage = 3 then (
      print_endline "Initialization";
      dump_zinc init;
      print_endline "";
      print_endline "Code for functions";
      dump_zinc fcts;
      print_endline ""
    ) else
      emit_phrase oc code
  )

let compile_impl oc impl =
  let loc = impl.im_loc in
  reset_te_vars();
  match impl.im_desc with
  | Pimpl_expr e ->
      let ty = typing_impl_expr loc e in
      if !verbose then
        print_impl_expr ty;
      if !stage >= 2 then
         process_lambda oc @@ translate_expr e
  | Pimpl_typedef decl ->
      let ty_decl = typing_impl_typedef loc decl in
      if !verbose then
        print_impl_typedef ty_decl
  | Pimpl_letdef(isrec,binds) ->
      let env = typing_impl_letdef loc isrec binds in
      if !verbose then
        print_impl_letdef env;
      if !stage >= 2 then
        process_lambda oc @@ translate_letdef impl.im_loc isrec binds
  | Pimpl_excdef decl ->
      let cd = typing_impl_excdef loc decl in
      if !verbose then
        print_impl_excdef cd

let compile_implementation objfile impls =
  let oc = if !stage >= 4 then open_out_bin objfile else stdout in
  if !stage >= 4 then
    start_emit_phrase oc;
  List.iter (compile_impl oc) impls;
  if !stage >= 4 then
    end_emit_phrase oc;
  close_out oc
