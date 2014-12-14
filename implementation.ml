open Syntax
open Typing
open Printer

let verbose = ref false

let typing_impl_expr loc e =
  push_level();
  let ty = typing_expr [] e in
  pop_level();
  if not (should_value_restrict e) then
    generalize e

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
  if is_rec then
    submit();
  List.iter2 (fun (_,e) ty ->
    typing_expect [] e ty
  ) pes tys;
  pop_level();

  let gens = List.map (fun (_,e) -> should_value_restrict e) pes in
  List.iter2 (fun gen ty ->
    if gen then
      generalize ty
    else
      value_restrict ty
  ) gens tys;
  if not is_rec then
    submit();
  env

let compile_impl impl =
  let loc = impl.im_loc in
  reset_te_vars();
  match impl.im_desc with
  | Pimpl_expr e ->
      let ty = typing_impl_expr loc e in
      print_endline "Expr";
      if !verbose then
        print_impl_expr ty;
  | Pimpl_typedef decl ->
      let ty_decl = typing_impl_typedef loc decl in
      if !verbose then
        print_impl_typedef ty_decl
  | Pimpl_letdef(isrec,pes) ->
      let env = typing_impl_letdef loc isrec pes in
      if !verbose then
        print_impl_letdef env

let compile_implementation impls =
  List.iter compile_impl impls
