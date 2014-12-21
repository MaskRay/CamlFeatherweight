open Instruction
open Lambda
open Syntax

let label_ctr = ref 0
let reset_label () = label_ctr := 0
let new_label () = incr label_ctr; !label_ctr

let rec is_tail = function
  | Kreturn::_ -> true
  | Klabel _::c -> is_tail c
  | _ -> false

let make_branch code =
  match code with
  | Kreturn::_ ->
      Kreturn, code
  | (Kbranch _ as b)::_ ->
      b, code
  | _ ->
      let lbl = new_label() in
      Kbranch lbl, Klabel lbl::code

let test_for_const = function
  | Const_char x ->
      Ptest_int(Pneqimm (int_of_char x))
  | Const_int x ->
      Ptest_int(Pneqimm x)
  | Const_float x ->
      Ptest_float(Pneqimm x)
  | Const_string x ->
      Ptest_string(Pneqimm x)

let compile_lambda expr =
  let to_compile = ref [] in
  let rec compile_expr staticraise expr cont =
    let rec c_expr expr cont =
      match expr with
      | Lvar i ->
          Kaccess i :: cont
      | Lconst c ->
          Kquote c :: cont
      | Lapply(e,es) ->
          begin match cont with
          | Kreturn::_ ->
              c_expr_list es (Kpush::c_expr e (Ktermapply::cont))
          | _ ->
              Kpushmark::c_expr_list es (Kpush::c_expr e (Kapply::cont))
          end
      | Labstract e ->
          if is_tail cont then
            Kgrab::c_expr e cont
          else (
            let lbl = new_label() in
            to_compile := (e,lbl) :: !to_compile;
            Kcur lbl::cont
          )
      | Lfor(start,stop,up,body) ->
          let l_end = new_label() and l_loop = new_label() in
          c_expr start (
            Kmakeblock((1,0),1)::Klet::
            c_expr stop (
              Klet::Klabel l_loop::
              Kaccess 1::Kprim(Pfield 0)::Klet::
              Kpush::Kaccess 1::
              Ktest(Ptest_int(if up then Plt else Pgt), l_end)::
              c_expr body (
                Kendlet 1::
                Kaccess 1::Kprim(if up then Pincr else Pdecr)::
                Kbranch l_loop::
                Klabel l_end::Kendlet 3::
                Kquote(Const_block 0)::cont
              )
            )
          )
      | Llet(binds,body) ->
          let c = if is_tail cont then cont else Kendlet(List.length binds)::cont in
          let c = c_expr body c in
          let rec go = function
            | [] -> c
            | e::es -> c_expr e (Klet::go es)
          in
          go binds
      | Lletrec(binds,body) ->
          let n = List.length binds in
          let c = if is_tail cont then cont else Kendlet n::cont in
          let c = c_expr body c in
          let rec go i = function
            | [] -> c
            | e::es -> c_expr e (Kupdate (i-1)::go (i-1) es)
          in
          Kdummy n::go n binds
      | Lprim(Pdummy n, []) ->
          Kdummy n::cont
      | Lprim(Pgetglobal id, []) ->
          Kgetglobal id::cont
      | Lprim(Psequand, [e1;e2]) ->
          let lbl = new_label() in
          c_expr e1 (Kbranchifnot lbl::
            c_expr e2 (Klabel lbl::cont))
      | Lprim(Psequor, [e1;e2]) ->
          let lbl = new_label() in
          c_expr e1 (Kbranchif lbl::
            c_expr e2 (Klabel lbl::cont))
      | Lprim(Psetglobal id, [e]) ->
          c_expr e (Ksetglobal id::cont)
      | Lprim(Pmakeblock tag, ls) ->
          c_expr_list ls (Kmakeblock(tag, List.length ls)::cont)
      | Lprim(Pnot, [e]) ->
          begin match cont with
          | Kbranchif lbl::c ->
              c_expr e (Kbranchifnot lbl::c)
          | Kbranchifnot lbl::c ->
              c_expr e (Kbranchif lbl::c)
          | c ->
              c_expr e (Kprim Pnot::c)
          end
      | Lprim(p, ls) ->
          c_expr_list ls (Kprim p::cont)
      | Lstaticcatch(e, Lstaticraise) ->
          c_expr e cont
      | Lstaticcatch(e, handler) ->
          let b, c = make_branch cont
          and lbl = new_label() in
          compile_expr lbl e (b::Klabel lbl::c_expr handler c)
      | Lstaticraise ->
          Kbranch staticraise::cont
      | Lif(cond,ifso,ifnot) ->
          c_bin_test cond ifso ifnot cont
      | Lsequence(l1,l2) ->
          c_expr l1 (c_expr l2 cont)
      | Lcond(sel,alts) ->
          let b, c = make_branch cont in
          let rec go = function
            | [] -> assert false
            | [a,e] ->
                Ktest(test_for_const a, staticraise)::c_expr e c
            | (a,e)::rest ->
                let lbl = new_label() in
                Ktest(test_for_const a, lbl)::c_expr e (b::Klabel lbl::go rest)
          in
          c_expr sel (go alts)
      | Lswitch(1, sel, [_, e]) ->
          c_expr e cont
      | Lswitch(2, sel, [(_,0), l0; (_,1), l1]) ->
          c_bin_test sel l1 l0 cont
      | Lswitch(span,sel,alts) ->
          let b, c = make_branch cont in
          let tbl = Array.make span staticraise in
          (* TODO add sequential tests *)
          let rec go = function
            | [] -> assert false
            | [(_,i),e] ->
                let lbl = new_label() in
                tbl.(i) <- lbl;
                Klabel lbl::c_expr e c
            | ((_,i),e)::rest ->
                let lbl = new_label() in
                tbl.(i) <- lbl;
                Klabel lbl::c_expr e (b::go rest)
          in
          c_expr sel (Kswitch tbl::go alts)

    and c_expr_list expr cont =
      match expr with
      | [] -> cont
      | [e] -> c_expr e cont
      | e::es -> c_expr_list es (Kpush::c_expr e cont)

    and c_bin_test cond ifso ifnot cont =
      let b, c = make_branch cont
      and lbl = new_label() in
      c_expr cond (Kbranchifnot lbl::c_expr ifso (b::Klabel lbl::c_expr ifnot c))

    in
    c_expr expr cont

  and compile_rest cont =
    match !to_compile with
    | [] -> cont
    | (e,lbl)::rest ->
        to_compile := rest;
        compile_rest (Klabel lbl::compile_expr (-1) e (Kreturn::cont))

  in
  reset_label();
  let init = compile_expr (-1) expr [] in
  let func = compile_rest [] in
  init, func
