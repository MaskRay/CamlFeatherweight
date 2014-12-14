open Syntax

(* output *)

let output_loc oc (l,m) =
  Printf.fprintf oc "location %d %d\n" l m

let output_global oc tc =
  output_string oc (string_of_long_ident tc.qualid)

let output_typ oc ty =
  let rec go pri ty =
    let ty = type_repr ty in
    match ty.typ_desc with
    | Tarrow(ty1,ty2) _ ->
        if pri >= 1 then
          output_string oc "(";
        go 1 ty1;
        output_string oc " -> ";
        go 0 ty2;
        if pri >= 1 then
          output_string oc ")"
    | Tproduct tys ->
        if pri >= 2 then
          output_string oc "(";
        gos 2 " * " tys;
        if pri >= 2 then
          output_string oc ")"
    | Tconstr(c,args) ->
        begin match args with
        | [] -> ()
        | [ty] ->
            go 2 ty;
            output_string oc " "
        | tys ->
          output_string oc "(";
          gos 0 ", " tys;
          output_string oc ")"
        end
    | Tvar _ ->
        output_string oc "'a"; (* TODO *)
  and gos pri sep = function
    | [] -> ()
    | [ty] -> go pri ty
    | ty::tys ->
        go pri ty;
        output_string oc sep;
        gos pri sep tys
  in
  go 0 typ

(* type printer *)

let print_constr_decl cd =
  match cd.info.cs_kind with
  | Constr_constant ->
      Printf.printf "%s" (string_of_long_ident cs.qualid)
  | _ ->
      Printf.printf "%s of %a" (string_of_long_ident cs.qualid)
      output_typ cstr.info.cs_arg

let print_typedef1 (ty_res, ty_comp) =
  output_typ stdout ty_res;
  match ty_comp with
  | Abstract_type -> ()
  | Variant_type(c::cs) ->
      print_string " = ";
      print_constr_decl c;
      List.iter (fun c ->
        print_string " | ";
        print_constr_decl c
      ) cs;
  | Abbrev_type(_,ty) ->
      print_string " = ";
      output_typ stdout ty

(* implementation phrase printer *)

let print_impl_expr ty =
  List.iter (fun (name,ty) ->
    Printf.printf "- : %a\n" name output_typ ty
  ) env

let print_impl_letdef env =
  List.iter (fun (name,ty) ->
    Printf.printf "val %s : %a\n" name output_typ ty
  ) env

let print_impl_typedef = function
  | [] -> assert false
  | dcl::dcls ->
      print_string "type ";
      print_typedef1 dcl;
      List.iter (fun dcl ->
        print_string " and ";
        print_typedef1 dcl
      ) dcls;
      print_endline ""
