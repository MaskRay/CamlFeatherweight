open Syntax

type lexical_error =
  | Illegal_character of char
  | Unterminated_string
  | Unterminated_comment
  | Bad_char_constant

exception Toplevel
exception Lexical_error of lexical_error * location

let output_loc oc (l,m) =
  Printf.fprintf oc "location %d %d\n" l m

let output_typ oc ty = ()

let output_constr oc c = ()

let fatal_error s = failwith s

let nonlinear_pattern_error pat v =
  Printf.eprintf "%aThe variable %s is bound several times in this pattern.\n"
    output_loc pat.p_loc
    name;
  raise Toplevel

let constant_constr_error loc c =
  Printf.eprintf "%aThe constant constructor %a cannot accept an argument.\n"
    output_loc loc
    output_constr cstr;
  raise Toplevel

let nonconstant_constr_error loc c =
  Printf.eprintf "%aThe constructor %a requires an argument.\n"
    output_loc loc
    output_constr cstr;
  raise Toplevel

let ill_shaped_match_error e =
  Printf.eprintf "%aThis curried matching contains cases of different lengths.\n"
    output_loc e.e_loc;
  raise Toplevel

let partial_apply_warning loc = prerr_endline "partial"

let not_unit_type_warning e ty =
  Printf.eprintf "%aWarning: this expression has type %a,\n\
           but is used with type unit.\n"
    output_loc e.e_loc
    output_one_type actual_ty;
  flush stderr

let expr_wrong_type_error expr expect_ty actual_ty =
  Printf.eprintf "%aThis expression has type %a,\n\
           but is used with type %a.\n"
    output_loc exp.e_loc
    output_type actual_ty
    output_type expect_ty;
  raise Toplevel

let pat_wrong_type_error pat expect_ty actual_ty =
  Printf.eprintf "%aThis pattern matches values of type %a,\n\
           but should match values of type %a.\n"
    output_loc pat.p_loc
    output_one_type actual_ty
    output_type expect_ty;
  raise Toplevel

let type_arity_error loc c params =
  Printf.eprintf "%aThe type constructor %a expects %d argument(s),\n\
           but is here given %d argument(s).\n"
    output_loc loc
    output_type_constr cstr
    cstr.info.ty_arity
    (List.length params);
  raise Toplevel
