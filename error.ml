open Syntax
open Type

type lexical_error =
  | Illegal_character of char
  | Unterminated_string
  | Unterminated_comment
  | Bad_char_constant

exception Toplevel
exception Lexical_error of lexical_error * location

let fatal_error s = failwith s

(* output *)

let type_var_names = ref []
let type_var_ctr = ref 0
let reset_type_var_names () =
  type_var_ctr := 0;
  type_var_names := []

let name_of_type_var sch var =
  try
    List.assq var !type_var_names
  with Not_found ->
    let i = !type_var_ctr in
    let name =
      if i < 26 then
        String.make 1 (char_of_int (i+97))
      else
        String.make 1 (char_of_int (i mod 26+97)) ^ string_of_int (i/26) in
    let name = if (not sch) || var.typ_level = generic then name else "_"^name in
    incr type_var_ctr;
    type_var_names := (var,name) :: !type_var_names;
    name

let output_loc oc (l,m) =
  Printf.fprintf oc "location %d %d\n" l m

let output_global oc tc =
  output_string oc (string_of_long_ident tc.qualid)

let output_long_ident oc id =
  output_string oc (string_of_long_ident id)

let output_constr = output_global
let output_type_constr = output_global

let output_typ oc sch ty =
  let rec go pri ty =
    let ty = type_repr ty in
    begin match ty.typ_desc with
    | Tarrow(ty1,ty2) ->
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
          output_string oc ") "
        end;
        output_global oc c
    | Tvar _ ->
        output_string oc "'";
        output_string oc (name_of_type_var sch ty)
    end
  and gos pri sep = function
    | [] -> ()
    | [ty] -> go pri ty
    | ty::tys ->
        go pri ty;
        output_string oc sep;
        gos pri sep tys
  in
  go 0 ty

let output_type oc ty =
  output_typ oc false ty

let output_new_type oc ty =
  reset_type_var_names();
  output_typ oc false ty

let output_schema oc ty =
  reset_type_var_names();
  output_typ oc true ty

let nonlinear_pattern_err pat name =
  Printf.eprintf "%aThe variable %s is bound several times in this pattern.\n"
    output_loc pat.p_loc
    name;
  raise Toplevel

let constant_constr_err loc c =
  Printf.eprintf "%aThe constant constructor %a cannot accept an argument.\n"
    output_loc loc
    output_constr c;
  raise Toplevel

let duplicate_param_in_type_decl_err loc =
  Printf.eprintf "%aRepeated type parameter in type declaration.\n"
    output_loc loc;
  raise Toplevel

let nonconstant_constr_err loc c =
  Printf.eprintf "%aThe constructor %a requires an argument.\n"
    output_loc loc
    output_constr c;
  raise Toplevel

let ill_shaped_match_err e =
  Printf.eprintf "%aThis curried matching contains cases of different lengths.\n"
    output_loc e.e_loc;
  raise Toplevel

let partial_apply_warn loc = prerr_endline "partial"

let not_unit_type_warn e actual_ty =
  Printf.eprintf "%aWarning: this expression has type %a,\n\
           but is used with type unit.\n"
    output_loc e.e_loc
    output_new_type actual_ty;
  flush stderr

let expr_wrong_type_err e expect_ty actual_ty =
  Printf.eprintf "%aThis expression has type %a,\n\
           but is used with type %a.\n"
    output_loc e.e_loc
    output_new_type actual_ty
    output_type expect_ty;
  raise Toplevel

let pat_wrong_type_err pat expect_ty actual_ty =
  Printf.eprintf "%aThis pattern matches values of type %a,\n\
           but should match values of type %a.\n"
    output_loc pat.p_loc
    output_new_type actual_ty
    output_type expect_ty;
  raise Toplevel

let type_arity_err loc c params =
  Printf.eprintf "%aThe type constructor %a expects %d argument(s),\n\
           but is here given %d argument(s).\n"
    output_loc loc
    output_type_constr c
    c.info.ty_arity (List.length params);
  raise Toplevel

let application_of_non_function_err e ty =
  begin try
    filter_arrow ty |> ignore;
    Printf.eprintf "%aThis function is applied to too many arguments.\n"
      output_loc e.e_loc
  with Unify ->
    Printf.eprintf "%aThis expression is not a function, it cannot be applied.\n"
      output_loc e.e_loc
  end;
  raise Toplevel

let unbound_value_err loc id =
  Printf.eprintf "%aThe value identifier %a is unbound.\n" 
    output_loc loc
    output_long_ident id;
  raise Toplevel

let unbound_constr_err loc id =
  Printf.eprintf "%aThe constructor %a is unbound.\n"
    output_loc loc
    output_long_ident id;
  raise Toplevel

let unbound_type_constr_err loc id =
  Printf.eprintf "%aThe type constructor %a is unbound.\n"
    output_loc loc
    output_long_ident id;
  raise Toplevel

let unbound_type_var_err v te =
  Printf.eprintf "%aThe type variable %s is unbound.\n"
    output_loc te.te_loc v;
  raise Toplevel
