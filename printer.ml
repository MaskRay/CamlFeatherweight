open Syntax
open Typing
open Error

(* type printer *)

let print_constr_decl cd =
  match cd.info.cs_kind with
  | Constr_constant ->
      Printf.printf "%s" (string_of_long_ident cd.qualid)
  | _ ->
      Printf.printf "%s of %a" (string_of_long_ident cd.qualid)
      output_type cd.info.cs_arg

let print_typedef1 (ty_res, ty_comp) =
  output_type stdout ty_res;
  match ty_comp with
  | Abstract_type -> ()
  | Variant_type [] -> assert false
  | Variant_type(c::cs) ->
      print_string " = ";
      print_constr_decl c;
      List.iter (fun c ->
        print_string " | ";
        print_constr_decl c
      ) cs;
  | Abbrev_type(_,ty) ->
      print_string " = ";
      output_type stdout ty

(* implementation phrase printer *)

let print_impl_expr ty =
  Printf.printf "- : %a\n" output_new_type ty

let print_impl_letdef env =
  List.iter (fun (name,ty) ->
    Printf.printf "val %s : %a\n" name output_schema ty
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
