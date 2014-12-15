open Error
open Global
open Lambda
open Syntax

let search_env env name =
  let rec go i = function
    | [] -> assert false
    | vs::vss ->
        try i, List.assoc name vs
        with Not_found -> go (i+1) vss
  in
  go 0 env

let transl_access env name =
  let i, path = search_env env name in
  List.fold_right (fun j l ->
    Lprim(Pfield j, [l])
  ) path (Lvar i)

let transl_update env name value =
  match search_env env name with
  | i, n::path ->
      Lprim(Psetfield n,
      [List.fold_right (fun j l -> Lprim(Pfield j, [l])) path (Lvar i);
      value])
  | _ -> assert false

let paths_of_pat path p =
  let rec go acc path p =
    match p.p_desc with
    | Ppat_alias(p,a) ->
        go ((a,path)::acc) path p
    | Ppat_constraint(p,_) ->
        go acc path p
    | Ppat_constr(_,p) ->
        begin match p with
        | None -> acc
        | Some p -> go acc path p
        end
    | Ppat_tuple ps ->
        let rec go2 acc i = function
          | [] -> acc
          | p::ps -> go2 (go acc (i::path) p) (i+1) ps
        in
        go2 acc 0 ps
    | Ppat_var v ->
        (v,path)::acc
    | _ -> acc
  in
  go [] path p

let make_env env ps =
  List.fold_left (fun env p -> paths_of_pat [] p :: env) env ps

type res = Partial | Total | Dubious

type row = pattern list * lambda
type matching = row list * lambda list

let add_match (rows,paths) row =
  row::rows, paths

(* (vars with leftmost column removed, others) *)
let split_matching (rows,paths) =
  let rec go = function
    | ({p_desc=Ppat_any}::ps,act)::rest
    | ({p_desc=Ppat_var _}::ps,act)::rest ->
        let vars, others = go rest in
        add_match vars (ps,act), others
    | ({p_desc=Ppat_alias(p,_)}::ps,act)::rest
    | ({p_desc=Ppat_constraint(p,_)}::ps,act)::rest ->
        go ((p::ps,act)::rest)
    | rows ->
        ([],List.tl paths), (rows,paths)
  in
  go rows

let make_const_match paths row : matching =
  [row], List.tl paths

let make_constr_match cd paths row : matching =
  match paths with
  | [] -> assert false
  | hd::tl ->
      match cd.info.cs_kind with
      | Constr_constant ->
          [row], tl
      | _ ->
          [row], Lprim(Pfield 0, [hd])::tl

let add_to_division make_match divs key (row : row) =
  try
    let ms = List.assoc key divs in
    ms := add_match !ms row;
    divs
  with Not_found ->
    (key, ref (make_match row)) :: divs

(* (constants divided, tags divided, rest) *)
let divide_matching (rows,paths) =
  let rec go = function
    | [] ->
        [], [], ([], paths)
    | ([],_)::_ ->
        assert false
    | ({p_desc=Ppat_alias(p,_)}::ps,act)::rest
    | ({p_desc=Ppat_constraint(p,_)}::ps,act)::rest ->
        go ((p::ps,act)::rest)
    | ({p_desc=Ppat_or(p1,p2)}::ps,act)::rest ->
        go ((p1::ps,act)::(p2::ps,act)::rest)
    | ({p_desc=Ppat_constant c}::ps,act)::rest ->
        let constants, constrs, others = go rest in
        add_to_division (make_const_match paths) constants c (ps,act),
        constrs,
        others
    | ({p_desc=Ppat_constr(id,arg)}::ps,act)::rest ->
        let cd = find_constr_desc (string_of_long_ident id) in
        let constants, constrs, others = go rest in
        let ps =
          begin match arg with
          | None -> ps
          | Some arg ->
              match cd.info.cs_kind with
              | Constr_constant -> ps
              | _ -> arg::ps
          end in
        constants,
        constrs,
        (*add_to_division (make_constr_match cd paths) constrs cd.info.cs_tag (ps,act),*)
        others
    | rows ->
        [], [], (rows,paths)
  in
  go rows

let upper_left_pattern =
  let rec go p =
    match p.p_desc with
    | Ppat_alias(p,_)
    | Ppat_constraint(p,_)
    | Ppat_or(p,_) -> go p
    | _ -> p
  in
  go

let get_span_of_constr cd = fst cd.info.cs_tag

let rec conquer_matching =
  let rec conquer_divided_matching = function
    | [] ->
        [], Total
    | (key,ms)::rest ->
        let lambda1, total1 = conquer_matching !ms
        and list2, total2 = conquer_divided_matching rest in
        (key,lambda1)::list2,
        (match total1,total2 with
        | Total,Total -> Total
        | Partial,_
        | _,Partial -> Partial
        | _ -> Dubious)
  in
  function
  | [], _ ->
      Lstaticraise, Partial
  | ([],act)::rest, _ ->
      act, Total
  | (ul::_,_)::_, (path::_) as mat ->
      begin match ul.p_desc with
      | Ppat_any
      | Ppat_var _ ->
          let vars, rest = split_matching mat in
          let lambda1, total1 = conquer_matching vars
          and lambda2, total2 = conquer_matching rest in
          if total1 = Total then
            lambda1, Total
          else
            Lstaticcatch(lambda1, lambda2),
            (if total2 = Total then Total else Dubious)
      | _ ->
          let ul = upper_left_pattern ul in
          match divide_matching mat with
          | [], [], others ->
              conquer_matching others
          | constants, [], others ->
              let divs1, _ = conquer_divided_matching constants
              and lambda2, total2 = conquer_matching others in
              Lstaticcatch(Lcond(path,divs1), lambda2), total2
          | [], constrs, others ->
              let divs1, total1 = conquer_divided_matching constrs
              and lambda2, total2 = conquer_matching others in
              let span = List.length divs1
              and expect_span =
                match ul.p_desc with
                | Ppat_constr(id,_) ->
                    let cd = find_constr_desc (string_of_long_ident id) in
                    get_span_of_constr cd
                | _ -> assert false
              in
              if span = expect_span && total1 = Total then
                Lswitch(span, path, divs1), Total
              else
                Lstaticcatch(Lswitch(span, path, divs1), lambda2),
                (if total1 = Total then total2
                else if total2 = Total then Total
                else Dubious)
            | _ -> assert false
      end
  | _ -> assert false

let rec transl_expr env expr =
  let rec go expr =
    match expr.e_desc with
    | Pexpr_apply(f,args) ->
        Lapply(go f, List.map go args)
    | Pexpr_constr(id,arg) ->
        let cd = find_constr_desc (string_of_long_ident id) in
        begin match arg with
        | None ->
            begin match cd.info.cs_kind with
            | Constr_constant ->
                Lprim(Pmakeblock cd.info.cs_tag, [])
            | Constr_regular ->
                Labstract(Lprim(Pmakeblock cd.info.cs_tag, [Lvar 0]))
            end
        | Some arg ->
            Lprim(Pmakeblock cd.info.cs_tag, [go arg])
        end
    | Pexpr_constant c ->
        Lconst c
    | Pexpr_ident id ->
        begin match id with
        | Ldot _ -> failwith "TODO"
        | Lident name ->
            transl_access env name
        end
    | Pexpr_let(isrec,binds,body) ->
        if isrec then (
          let ps, acts = List.split binds in
          let env' = make_env env ps in
          Lletrec(List.map (transl_expr env') acts,
          transl_match expr.e_loc env [ps,body])
        ) else
          Llet(transl_bind env binds,
          transl_match expr.e_loc env [List.map fst binds,body])
    | Pexpr_tuple es ->
        Lprim(Pmakeblock(1,0), List.map go es)
  in
  go expr

and transl_bind env = function
  | [] -> []
  | (p,e)::pes ->
      transl_expr env e :: transl_bind ([]::env) pes

and transl_match loc env psas =
  let rows = List.map (fun (ps,act) ->
    ps, transl_expr (make_env env ps) act
  ) psas in
  let rec go = function
    | 0 -> []
    | n -> Lvar(n-1) :: go (n-1)
  in
  let row_len = List.hd rows |> fst |> List.length in
  let lambda, total = conquer_matching (rows, go row_len) in
  match total with
  | Total -> lambda
  | _ -> Lstaticcatch(lambda, Lprim(Praise, [])) (* FIXME report error *)
