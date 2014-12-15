type location = int * int

type long_ident =
  | Lident of string
  | Ldot of long_ident * string

type constant =
  | Const_char of char
  | Const_int of int
  | Const_float of float
  | Const_string of string

(* primitive *)

type primitive =
  | Paddint
  | Psubint
  | Pmulint
  | Pdivint
  | Pmodint
  | Pfloat of float_primitive

and float_primitive =
  | Paddfloat
  | Psubfloat
  | Pmulfloat
  | Pdivfloat
  | Pmodfloat

(* global *)

type 'a global = { qualid: long_ident; info: 'a }

(* type *)

let generic = -1 and notgeneric = 0

type typ = { typ_desc: typ_desc; mutable typ_level: int }
and typ_desc =
  | Tarrow of typ * typ
  | Tconstr of type_constr global * typ list
  | Tproduct of typ list
  | Tvar of typ_link ref
and typ_link =
  | Tnolink
  | Tlink of typ
and type_constr = { ty_stamp: int; mutable ty_abbr: type_abbrev }
and type_abbrev =
  | Tnotabbrev
  | Tabbrev of typ list * typ

(* type constructur descriptions
 * *)

(* e.g. unit, int, list, option *)
type type_desc =
  { ty_constr: type_constr global; ty_arity: int; mutable ty_desc: type_components }
and type_components =
  | Abstract_type
  | Variant_type of constr_desc global list
  | Abbrev_type of typ list * typ

(* e.g. false, None *)
and constr_desc =
  { cs_arg: typ; cs_res: typ; cs_tag: int * int; cs_kind: constr_kind }

and constr_kind =
  | Constr_constant
  | Constr_regular
  | Constr_superfluous of int

type expression = { e_desc: expression_desc; e_loc: location; mutable e_typ: typ }
and expression_desc =
  | Pexpr_apply of expression * expression list
  | Pexpr_array of expression list
  | Pexpr_constant of constant
  | Pexpr_constraint of expression * type_expression
  | Pexpr_constr of long_ident * expression option
  | Pexpr_function of (pattern list * expression) list
  | Pexpr_ident of long_ident
  | Pexpr_if of expression * expression * expression option
  | Pexpr_let of bool * (pattern * expression) list * expression
  | Pexpr_sequence of expression * expression
  | Pexpr_tuple of expression list

and type_expression = { te_desc: type_expression_desc; te_loc: location }
and type_expression_desc =
  | Ptype_var of string
  | Ptype_arrow of type_expression * type_expression
  | Ptype_tuple of type_expression list
  | Ptype_constr of long_ident * type_expression list

and pattern = { p_desc: pattern_desc; p_loc: location; mutable p_typ: typ }
and pattern_desc =
  | Ppat_alias of pattern * string
  | Ppat_any
  | Ppat_array of pattern list
  | Ppat_constant of constant
  | Ppat_constraint of pattern * type_expression
  | Ppat_constr of long_ident * pattern option
  | Ppat_or of pattern * pattern
  | Ppat_tuple of pattern list
  | Ppat_var of string

(* RHS of type xx = ... *)
type type_decl =
  | Ptd_abstract
  | Ptd_variant of (string * type_expression option) list
  | Ptd_alias of type_expression

let rec expr_is_pure expr =
  match expr.e_desc with
  | Pexpr_array(es) -> List.for_all expr_is_pure es
  | Pexpr_constr _
  | Pexpr_constant _
  | Pexpr_function _
  | Pexpr_ident _ -> true
  | _ -> false

type impl_phrase = { im_desc: impl_desc; im_loc: location }
and impl_desc =
  | Pimpl_expr of expression
  | Pimpl_typedef of (string * string list * type_decl) list
  | Pimpl_letdef of bool * (pattern * expression) list

(* global value *)

type value_desc = { v_typ: typ; v_prim: prim_desc }
and prim_desc =
  | Not_prim
  | Prim of int * primitive

(* instances *)

let no_type = {typ_desc=Tproduct []; typ_level=0}

(* type stamp *)

let init_stamp = ref 0

let new_type_stamp () =
  let r = !init_stamp in
  incr init_stamp;
  r

(* dump *)

let rec string_of_long_ident = function
  | Lident id -> id
  | Ldot (l,id) -> string_of_long_ident l ^ "." ^ id

let dump_constant = function
  | Const_char c ->
      Printf.printf "Const_char %s\n" (Char.escaped c)
  | Const_int i ->
      Printf.printf "Const_int %d\n" i
  | Const_float f ->
      Printf.printf "Const_float %f\n" f
  | Const_string s ->
      Printf.printf "Const_string %s\n" (String.escaped s)

let rec dump_pattern d pat =
  let rec go d p =
    Printf.printf "%*s" (2*d) "";
    match p.p_desc with
    | Ppat_alias(p,a) ->
        Printf.printf "Alias %s\n" a;
        go (d+1) p
    | Ppat_any ->
        print_endline "Any"
    | Ppat_array ps ->
        Printf.printf "Array %d\n" (List.length ps);
        List.iter (go (d+1)) ps
    | Ppat_constant c->
        dump_constant c
    | Ppat_constraint(p,t) ->
        print_endline "Constraint";
        go (d+1) p;
        dump_type_expression (d+1) t
    | Ppat_constr(id,p) ->
        Printf.printf "Constr %s\n" (string_of_long_ident id);
        begin match p with
        | None -> ()
        | Some p -> go (d+1) p
        end
    | Ppat_or(p1,p2) ->
        print_endline "Or";
        go (d+1) p1;
        go (d+1) p2
    | Ppat_tuple ps ->
        print_endline "Tuple";
        List.iter (go (d+1)) ps
    | Ppat_var v ->
        Printf.printf "Var %s\n" v
  in
  go d pat

and dump_expression d expr =
  let rec go d e =
    Printf.printf "%*s" (2*d) "";
    match e.e_desc with
    | Pexpr_apply(e, es) ->
        print_endline "Apply";
        go (d+1) e;
        List.iter (go (d+1)) es
    | Pexpr_array es ->
        print_endline "Array";
        List.iter (go (d+1)) es
    | Pexpr_constant c ->
        dump_constant c
    | Pexpr_constraint(e,t) ->
        print_endline "Constraint";
        go (d+1) e;
        dump_type_expression (d+1) t
    | Pexpr_constr(id, e) ->
        Printf.printf "Constr %s\n" (string_of_long_ident id);
        begin match e with
        | None -> ()
        | Some e -> go (d+1) e
        end
    | Pexpr_function alts ->
        print_endline "Function";
        List.iter (fun (ps,e) ->
          Printf.printf "%*sCase\n" (2*d+2) "";
          List.iter (dump_pattern (d+2)) ps;
          go (d+2) e
        ) alts
    | Pexpr_ident id ->
        Printf.printf "Ident %s\n" (string_of_long_ident id)
    | Pexpr_if(cond,ifso,ifnot) ->
        print_endline "If";
        go (d+1) cond;
        go (d+1) ifso;
        begin match ifnot with
        | None -> ()
        | Some e -> go (d+1) e
        end
    | Pexpr_let(isrec,binds,body) ->
        print_endline (if isrec then "Letrec" else "Let");
        List.iter (fun (p,e) ->
          Printf.printf "%*sBinding\n" (2*d+2) "";
          dump_pattern (d+2) p;
          go (d+2) e
        ) binds;
        go (d+1) body
    | Pexpr_sequence(e1,e2) ->
        print_endline "Sequence";
        go (d+1) e1;
        go (d+1) e2
    | Pexpr_tuple(es) ->
        print_endline "Tuple";
        List.iter (go (d+1)) es
  in
  go d expr

and dump_type_expression d te =
  let rec go d te =
    Printf.printf "%*s" (2*d) "";
    match te.te_desc with
    | Ptype_var v ->
        Printf.printf "Var %s\n" v;
    | Ptype_arrow(te1,te2) ->
        print_endline "Arrow";
        go (d+1) te1;
        go (d+1) te2
    | Ptype_tuple(tes) ->
        print_endline "Tuple";
        List.iter (go (d+1)) tes
    | Ptype_constr(id,tes) ->
        Printf.printf "Constr %s\n" (string_of_long_ident id);
        List.iter (go (d+1)) tes
  in
  go d te

let dump_type_decl d td =
  let rec go d td =
    Printf.printf "%*s" (2*d) "";
    match td with
    | Ptd_abstract ->
        print_endline "Abstract"
    | Ptd_variant ts ->
        print_endline "Variant";
        List.iter (fun (name,te) ->
          Printf.printf "%*sConstructor %s\n" (2*d+2) "" name;
          match te with
          | None -> ()
          | Some te -> dump_type_expression (d+2) te
        ) ts
    | Ptd_alias te ->
        print_endline "Alias";
        dump_type_expression (d+1) te
  in
  go d td

let dump_impl_phrase d impl =
  Printf.printf "%*s" (2*d) "";
  match impl.im_desc with
  | Pimpl_expr e ->
      print_endline "Expr";
      dump_expression 1 e
  | Pimpl_typedef ts ->
      List.iter (fun (name,args,decl) ->
        Printf.printf "Type %s%s\n" (if args <> [] then String.concat " " args ^ " " else "") name;
        dump_type_decl (d+1) decl
      ) ts
  | Pimpl_letdef(isrec,pes) ->
      print_endline (if isrec then "Letdef rec" else "Letdef");
      List.iter (fun (p,e) ->
        dump_pattern (d+1) p;
        dump_expression (d+1) e
      ) pes

let dump_typ d ty =
  let rec go d ty =
    Printf.printf "%*s" (2*d) "";
    let l = ty.typ_level in
    match ty.typ_desc with
    | Tarrow(ty1,ty2) ->
        Printf.printf "Tarrow %d\n" l;
        go (d+1) ty1;
        go (d+1) ty2
    | Tconstr(tc,tys) ->
        Printf.printf "Tconstr %d\n" l;
        Printf.printf "%*s%s\n" (2*d+2) "" (string_of_long_ident tc.qualid);
        List.iter (go (d+1)) tys
    | Tproduct tys ->
        Printf.printf "Tproduct %d\n" l;
        List.iter (go (d+1)) tys
    | Tvar link ->
        match !link with
        | Tnolink ->
            Printf.printf "Tnolink %d\n" l
        | Tlink ty ->
            Printf.printf "Tlink %d\n" l;
            go (d+1) ty
  in
  go d ty
