open Builtins
open Error
open Syntax
open Global

let assoc_lookup kvs k = List.assoc k kvs
let assoc_mem kvs k = List.mem k kvs
let assoc_add kvs k v = (k,v)::kvs

exception Unify
exception Recursive_abbrev

let cur_level = ref 0
let push_level () = incr cur_level
let pop_level () = decr cur_level

let same_type_constr c1 c2 =
  c1.info.ty_stamp = c2.info_ty_stamp

let rec same_base_type ty1 ty2 =
  match (type_repr ty1).typ_desc, (type_repr ty2).typ_desc with
  | Tconstr({info={ty_abbr=Tabbrev(body,args)}}, params), _ ->
      same_base_type (expand_abbrev body args params) ty2
  | _, Tconstr({info={ty_abbr=Tabbrev(body,args)}}, params) ->
      same_base_type ty1 (expand_abbrev body args params)
  | Tconstr(c1,args1), Tconstr(c2,args2) ->
      same_type_constr c1 c2 &&
      List.for_all2 same_base_type args1 args2
  | _, _ ->
      false

let should_value_restrict expr =
  let rec go expr =
    match expr.e_desc with
    | Pexpr_array es -> List.for_all go es
    | Pexpr_constant _ -> true
    | Pexpr_constr(c,arg) ->
        begin match arg with
        | None -> true
        | Some arg -> go arg
        end
    | Pexpr_function _ -> true
    | Pexpr_ident _ -> true
    | Pexpr_if(cond,ifso,ifnot) -> go cond && go ifso && go ifnot
    | Pexpr_let(isrec,binds,body) -> false (* TODO *)
    | Pexpr_match _ -> false (* TODO *)
    | Pexpr_sequence(e1,e2) -> go e1 && go e2
    | Pexpr_tuple es -> List.for_all go es
    | _ -> false
  in
  go expr

(* new *)

let new_type_var () =
  { typ_desc=Tvar(ref Tnolink); typ_level= !cur_level }

let new_global_type_var () =
  { typ_desc=Tvar(ref Tnolink); typ_level=0 }

let new_type_var_list level arity =
  if arity <= 0 then
    []
  else
    { typ_desc=Tvar(ref Tnolink); typ_level=level } ::
    new_type_var_list level (arity-1)

let type_var_list arity =
  if arity <= 0 then
    []
  else
    { typ_desc=Tvar(ref Tnolink); typ_level= !cur_level } ::
    type_var_list (arity-1)

(* type of *)

let type_of_constant = function
  | Const_bool -> type_bool
  | Const_char -> type_char
  | Const_int -> type_int
  | Const_float -> type_float
  | Const_string -> type_string

let te_vars = ref []
let reset_te_vars = te_vars := []

let type_of_type_expression te =
  let rec go te =
    match te.te_desc with
    | Ptype_arrow(te1,te2) ->
        type_arrow (go te1) (go te2)
    | Ptype_constr(c,params) ->
        let c =
          try
            find_type_desc c
          with Desc_not_found ->
            unbound_type_constr_error te.te_loc c in
        if List.length params <> c.info.ty_arity then
          type_arity_error te.te_loc c params;
        { typ_desc=Tconstr(c.info.ty_constr, List.map go params)
        ; typ_level=notgeneric }
    | Ptype_tuple tes ->
        type_product (List.map go tes)
    | Ptype_var v ->
        begin try
          assoc_lookup !te_vars v
        with Not_found ->
          let ty = new_global_type_var() in
          te_vars := (v,ty):: !te_vars;
          ty
        end
  in

(* generalize *)

let rec gen_type ty =
  let ty = type_repr ty in
  let level = ty.typ_level in
  match ty.typ_desc with
  | Tarrow(ty1,ty2) ->
      let l1 = gen_type ty1
      and l2 = gen_type ty2 in
      ty.typ_level <- min l1 l2
  | Tconstr(_,tys) ->
      ty.typ_level <- gen_type_list tys
  | Tproduct tys ->
      ty.typ_level <- gen_type_list tys
  | Tvar _ ->
      if level > !cur_level then
        ty.typ_level <- generic

and gen_type_list = function
  | [] -> notgeneric
  | ty::tys ->
      let l1 = gen_type ty
      and l2 = gen_type_list tys in
      min l1 l2

let value_restrict ty =
  let rec go ty =
    let ty = type_repr ty in
    begin match ty with
    | Tarrow(ty1,ty2) ->
        go ty1;
        go ty2;
        ty.typ_level <- min ty1.typ_level ty2.typ_level
    | Tconstr(_,tys)
    | Tproduct tys ->
        ty.typ_level <- List.map go tys
    | Tvar _ ->
        if ty.typ_level > !cur_level then
          ty.typ_level <- !cur_level
    end;
    ty.typ_level
  in
  go ty |> ignore

(* instance *)

let rec type_repr ty =
  match ty.typ_desc with
  | Tvar link ->
      begin match !link with
      | Tnolink ->
          ty
      | Tlink ty' ->
          let ty = type_repr ty' in
          link := Tlink ty;
          ty
      end
  | _ -> ty

let rec copy_type ty =
  match ty.desc with
  | Tarrow(ty1,ty2) ->
      if ty.typ_level = generic then
        { typ_desc=Tarrow(copy_type ty1, copy_type ty2);
          typ_level=notgeneric }
      else
        ty
  | Tconstr(c, tys) ->
      if ty.typ_level = generic then
        { typ_desc=Tconstr(c, List.map copy_type tys);
          typ_level=notgeneric }
      else
        ty
  | Tproduct tys ->
      if ty.typ_level = generic then
        { typ_desc=Tproduct(List.map copy_type tys);
          typ_level=notgeneric }
      else
        ty
  | Tvar link ->
      begin match !link with
      | Tnolink ->
          if ty.typ_level = generic then (
            let v = new_type_var() in
            link := Tlink v;
            v
          ) else
            ty
      | Tlink ty ->
          if ty.typ_level = generic then
            copy_type ty
          else
            ty
      end

let rec cleanup_type ty =
  match ty.desc with
  | Tarrow(ty1,ty2) ->
      if ty.typ_level = generic then (
        cleanup_type ty1;
        cleanup_type ty2
      )
  | Tconstr(_,tys) ->
      if ty.typ_level = generic then
        List.iter cleanup_type tys
  | Tproduct tys ->
      if ty.typ_level = generic then
        List.iter cleanup_type tys
  | Tvar link ->
      begin match !link with
      | Tnolink ->
          ()
      | Tlink ty ->
          if ty.typ_level = generic then
            link := Tnolink
          else
            cleanup_type ty
      end

let type_instance ty =
  let ty' = copy_type ty in
  cleanup_type ty;
  ty'

let type_pair_instance ty1 ty2 =
  let ty1' = copy_type ty1
  and ty2' = copy_type ty2 in
  cleanup_type ty1';
  cleanup_type ty2';
  ty1', ty2'

let expand_abbrev body args params =
  let args' = List.map copy_type args in
  let body' = copy_type body in
  List.iter cleanup_type args;
  cleanup_type body;
  List.iter2 (fun a p ->
    match a.typ_desc with
    | Tvar link ->
        begin match !link with
        | Tnolink -> link := Tlink p
        | _ -> assert false
        end
    | _ -> assert false
  ) args params;
  body'

let check_recursive_abbrev c =
  match c.info.ty_abbr with
  | Tnotabbrev -> ()
  | Tabbrev(body,_) ->
      let rec go seen ty =
        match (type_repr ty).typ_desc with
        | Tvar -> ()
        | Tarrow(ty1,ty2) ->
            go seen ty1;
            go seen ty2
        | Tconstr(c, tys) ->
            if List.mem c seen then
              raise Recursive_abbrev;
            List.iter (go seen) tys;
            match c.info.ty_abbr with
            | Tnotabbrev -> ()
            | Tabbrev(body,args) ->
                go (c::seen) body
      in
      go [c] body

let check_occur l v ty =
  let rec go ty =
    match ty.typ_desc with
    | Tarrow(ty1,ty2) ->
        go ty1 || go ty2
    | Tconstr(_,tys)
    | Tproduct tys ->
        List.exists go tys
    | Tvar _ ->
        if l < ty.typ_level then
          ty.typ_level <- l;
        v == ty

(* unify *)

let rec unify ty1 ty2 =
  if ty1 == ty2 then
    ()
  else
    let ty1 = type_repr ty1
    and ty2 = type_repr ty2 in
    match ty1.typ_desc, ty2.typ_desc with
    | Tvar link1, Tvar link2 ->
        if ty1.typ_level < ty2.typ_level then (
          ty2.typ_level <- ty1.typ_level;
          link2 := Tlink ty1
        ) else (
          ty1.typ_level <- ty2.typ_level;
          link1 := Tlink ty2
        )
    | Tvar link, _ when not (occur_check ty1.typ_level ty1 ty2) ->
        link <- Tlink ty1
    | _, Tvar link when not (occur_check ty2.typ_level ty2 ty1) ->
        link <- Tlink ty2
    | Tarrow(t1x,t1y), Tarrow(t2x,t2y) ->
        unify t1x t2x;
        unify t1y t2y
    | Tconstr(c1,tys1), Tconstr(c2,tys2)
      when c1.info.ty_stamp = c2.info.ty_stamp ->
        unify_list tys1 tys2
    | Tconstr({info={ty_abbr=Tabbrev(body,args)}}, params), _ ->
        unify (expand_abbrev body args params args) ty2
    | _, Tconstr({info={ty_abbr=Tabbrev(body,args)}}, params) ->
        unify ty1 (expand_abbrev body args params args)
    | Tproduct t1s, Tproduct t2s ->
        unify_list t1s t2s
    | _ ->
        raise Unify

and unify_list t1s t2s =
  match t1s, t2s with
  | [], [] -> ()
  | t1::t1s, t2::t2s ->
      unify t1 t2;
      unify_list t1s t2s
  | _ -> raise Unify

let unify_expr expr expect_ty actual_ty =
  try
    unify expect_ty actual_ty
  with Unify ->
    expr_wrong_type_error expr expect_ty actual_ty

let unify_pat pat expect_ty actual_ty =
  try
    unify expect_ty actual_ty
  with Unify ->
    pat_wrong_type_error pat expect_ty actual_ty

(* filter *)

let rec filter_arrow ty =
  let ty = type_repr ty in
  let level = ty.typ_level in
  match ty.typ_desc with
  | Tarrow(ty1,ty2) ->
      ty1, ty2
  | Tconstr({info={ty_abbr=Tabbrev(body,args)}}, params) ->
      filter_arrow (expand_abbrev body args params)
  | Tvar link ->
      let ty1 = { typ_desc=Tvar(ref Tnolink); typ_level=level }
      and ty2 = { typ_desc=Tvar(ref Tnolink); typ_level=level } in
      link := Tlink { typ_desc=Tarrow(ty1, ty2); typ_level=level };
      ty1, ty2

let rec filter_product arity ty =
  match type_repr ty with
  | { typ_desc=Tvar link; typ_level=level } ->
      let tys = type_var_list level arity in
      link := Tlink tys in
      tys
  | { typ_desc=Tproduct tys } ->
      if List.length tys = arity then
        tys
      else
        raise Unify
  | { typ_desc=Tconstr({info={ty_abbr=Tabbrev(args,body)}}, params) } ->
      expand_abbrev

let rec typing_expr env expr =
  match expr.e_desc with
  | Pexpr_apply(e,es) ->
      _
  | Pexpr_array es ->
      _
  | Pexpr_constant c ->
      type_of_constant c
  | Pexpr_constr(c,arg) ->
      begin match arg with
      | None ->
        begin match c.info.cs_kind with
        | Constr_constant ->
            type_instance c.info.cs_res
        | _ ->
            let ty1, ty2 = type_pair_instance c.info.cs_arg c.info_cs.res in
            type_arrow ty1 ty2
        end
      | Some arg ->
        begin match c.info.cs_kind with
        | Constr_constant ->
            constant_constr_error expr.e_loc c
        | _ ->
            let ty1, ty2 = type_pair_instance c.info.cs_arg c.info_cs.res in
            typing_expect env arg ty1;
            ty2
        end
      end
  | Pexpr_function mat ->
      begin match mat with
      | [] -> failwith "empty matching"
      | (ps,e)::mat ->
          let arity = List.length ps in
          let ty_args = List.map (fun _ -> new_type_var()) ps in
          let ty_res = new_type_var() in
          List.iter (fun (ps,e) ->
            if List.length ps <> arity then
              ill_shaped_match_error expr;
            typing_expect (typing_pat_list ps ty_args @ env) e ty_res
          ) mat;
          List.fold_right type_arrow ty_args ty_res
      end;
  | Pexpr_ident id ->
      begin match id with
      | Lident id ->
          type_instance (try
              assoc_lookup env id
            with Not_found ->
              assoc_lookup all_types id)
      | Ldot(qual,id) ->
          type_unit
      end
  | Pexpr_if(cond,ifso,ifnot) ->
      typing_expect env type_bool cond;
      begin match ifnot with
      | None ->
          typing_expect env type_unit ifso;
          type_unit
      | Some ifnot ->
          let ty = typing_expr env ifso in
          typing_expect env t ifnot;
          ty
      end
  | Pexpr_let(isrec,binds,body) ->
  | Pexpr_sequence(e1,e2) ->
      typing_stmt env e1;
      typing_expr env e2
  | Pexpr_tuple es ->
      type_product (List.map (type_expr env)) es

and typing_expect env expr expect_ty =
  match expr.e_desc with
  | Pexpr_let(isrec,binds,body) ->
      typing_expect (typing_let env isrec binds) body expect_ty
  | Pexpr_sequence(e1,e2) ->
      typing_stmt env e1;
      typing_expr env e2 expect_ty
  | Pexpr_if(cond,ifso,ifnot) ->
      typing_expect env cond type_bool;
      typing_expect env ifso expect_ty;
      typing_expect env ifnot expect_ty
  | Pexpr_tuple es ->
      begin try
        List.iter2 (typing_expr env) es
        (filter_product (List.length es) expect_ty)
      with Unify ->
        unify_expr exp expect_ty (typing_expr env expr)
      end
  | _ ->
      unify_expr exp expect_ty (typing_expr env expr)

and typing_stmt env expr =
  let ty = typing_expr env expr in
  match (type_repr ty).typ_desc with
  | Tvar _ -> ()
  | Tarrow _ -> partial_apply_warning expr.e_loc
  | _ ->
      if not (same_base_type ty type_unit) then
        not_unit_type_warning expr ty

and typing_let env isrec pes =
  incr cur_level;
  let tys = List.map (fun _ -> new_type_var()) pes in
  let env' = typing_pat_list [] (List.map fst pes) tys @ env in
  List.iter (fun (p,e) ty ->
    typing_expect (if isrec then env' else env) e ty
  ) pes tys;
  desc cur_level;
  let gens = List.map (fun (p,e) -> should_value_restrict e) pes in
  List.iter2 (fun gen ty ->
    if gen then
      generalize ty
    else
      value_restrict ty
  ) gens tys;
  env'

and typing_pat penv pat ty =
  pat.p_typ <- ty;
  match pat.p_desc with
  | Ppat_alias(pat,a) ->
      if assoc_mem penv a then
        nonlinear_pattern_error pat a
      else
        typing_pat (assoc_add penv v ty) ty pat
  | Ppat_any ->
      penv
  | Ppat_array ->
      penv
  | Ppat_constant c ->
      unify_pat pat ty (type_of_constant c);
      penv
  | Ppat_constr(c,arg) ->
      begin match arg with
      | None ->
          begin match c.info.cs_kind with
          | Constr_constant ->
              unify_pat pat ty (type_instance c.info.cs_res);
              penv
          | _ ->
              nonconstant_const_error expr
          end
      | Some arg ->
          begin match c.info.cs_kind with
          | Constr_constant ->
              constant_constr_error expr.e_loc c
          | _ ->
              let ty1, ty2 = type_pair_instance c.info.cs_arg c.info_cs.res in
              unify_pat pat ty ty2;
              typing_pat penv arg ty1
          end
      end
  | Ppat_constraint(pat, te) ->
      let ty' = type_of_type_expression te in
      let penv = tpat penv pat ty' in
      unify_pat pat ty ty';
      penv
  | Ppat_or(p1,p2) ->
      let penv = typing_pat penv p1 ty in
      typing_pat penv p2 ty
  | Ppat_tuple ps ->
      typing_pat_list penv ps ty
  | Ppat_var v ->
      if assoc_mem penv v then
        nonlinear_pattern_error pat v
      else
        assoc_add penv v ty

and typing_pat_list penv ps tys =
  match ps, tys with
  | [], [] ->
      penv
  | p::ps, ty::tys ->
      typing_pat_list (typing_pat penv p ty) ps tys
  | _, _ ->
      failwith "arity unmatch"
