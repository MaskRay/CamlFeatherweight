open Builtin
open Syntax
open Global

let assoc_mem k kvs = try List.assoc k kvs; true with Not_found -> false

exception Unify
exception Recursive_abbrev

let cur_level = ref 0
let push_level () = incr cur_level
let pop_level () = decr cur_level

(* union-find *)

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

let should_value_restrict expr =
  let rec go expr =
    match expr.e_desc with
    | Pexpr_array [] -> true
    | Pexpr_constant _ -> true
    | Pexpr_constr(c,arg) ->
        begin match arg with
        | None -> true
        | Some arg -> go arg
        end
    | Pexpr_function _ -> true
    | Pexpr_ident _ -> true
    | Pexpr_if(cond,ifso,ifnot) ->
        begin match ifnot with
        | None -> go ifso
        | Some ifnot -> go ifso && go ifnot
        end
    | Pexpr_let(isrec,binds,body) -> false (* TODO *)
    | Pexpr_sequence(e1,e2) -> go e1 && go e2
    | _ -> false
  in
  go expr

(* new *)

let new_type_var () =
  { typ_desc=Tvar(ref Tnolink); typ_level= !cur_level }

let new_global_type_var () =
  { typ_desc=Tvar(ref Tnolink); typ_level=0 }

let rec new_type_var_list arity =
  if arity <= 0 then
    []
  else
    { typ_desc=Tvar(ref Tnolink); typ_level= !cur_level } ::
    new_type_var_list (arity-1)

let rec type_var_list level arity =
  if arity <= 0 then
    []
  else
    { typ_desc=Tvar(ref Tnolink); typ_level=level } ::
    type_var_list level (arity-1)

(* generalize *)

let gen_type ty =
  let rec go ty =
    let ty = type_repr ty in
    let level = ty.typ_level in
    begin match ty.typ_desc with
    | Tarrow(ty1,ty2) ->
        let l1 = go ty1
        and l2 = go ty2 in
        ty.typ_level <- min l1 l2
    | Tconstr(_,tys) ->
        ty.typ_level <- gos tys
    | Tproduct tys ->
        ty.typ_level <- gos tys
    | Tvar _ ->
        if level > !cur_level then
          ty.typ_level <- generic
    end;
    ty.typ_level
  and gos = function
    | [] -> notgeneric
    | ty::tys ->
        let l1 = go ty and l2 = gos tys in
        min l1 l2
  in
  go ty |> ignore

let value_restrict ty =
  let rec go ty =
    let ty = type_repr ty in
    begin match ty.typ_desc with
    | Tarrow(ty1,ty2) ->
        go ty1;
        go ty2;
        ty.typ_level <- min ty1.typ_level ty2.typ_level
    | Tconstr(_,tys)
    | Tproduct tys ->
        ty.typ_level <- List.fold_left min max_int (List.map go tys)
    | Tvar _ ->
        if ty.typ_level > !cur_level then
          ty.typ_level <- !cur_level
    end;
    ty.typ_level
  in
  go ty |> ignore

(* instance *)

let rec copy_type ty =
  match ty.typ_desc with
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
  match ty.typ_desc with
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

(* same *)

let same_type_constr c1 c2 =
  c1.info.ty_stamp = c2.info.ty_stamp

let rec same_base_type ty1 ty2 =
  match (type_repr ty1).typ_desc, (type_repr ty2).typ_desc with
  | Tconstr({info={ty_abbr=Tabbrev(args,body)}}, params), _ ->
      same_base_type (expand_abbrev body args params) ty2
  | _, Tconstr({info={ty_abbr=Tabbrev(args,body)}}, params) ->
      same_base_type ty1 (expand_abbrev body args params)
  | Tconstr(c1,args1), Tconstr(c2,args2) ->
      same_type_constr c1 c2 &&
      List.for_all2 same_base_type args1 args2
  | _, _ ->
      false

let check_recursive_abbrev c =
  match c.info.ty_abbr with
  | Tnotabbrev -> ()
  | Tabbrev(_,body) ->
      let rec go seen ty =
        match (type_repr ty).typ_desc with
        | Tvar _ -> ()
        | Tarrow(ty1,ty2) ->
            go seen ty1;
            go seen ty2
        | Tconstr(c, tys) ->
            if List.mem c seen then
              raise Recursive_abbrev;
            List.iter (go seen) tys;
            match c.info.ty_abbr with
            | Tnotabbrev -> ()
            | Tabbrev(args,body) ->
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
  in
  go ty

(* filter *)

let rec filter_arrow ty =
  let ty = type_repr ty in
  let level = ty.typ_level in
  match ty.typ_desc with
  | Tarrow(ty1,ty2) ->
      ty1, ty2
  | Tconstr({info={ty_abbr=Tabbrev(args,body)}}, params) ->
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
      link := Tlink { typ_desc=Tproduct tys; typ_level=level }; (* TODO not *)
      tys
  | { typ_desc=Tproduct tys } ->
      if List.length tys = arity then
        tys
      else
        raise Unify
  | { typ_desc=Tconstr({info={ty_abbr=Tabbrev(args,body)}}, params) } ->
      filter_product arity (expand_abbrev body args params)
