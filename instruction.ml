open Syntax

type zinc_instruction =
  | Kaccess of int
  | Kapply
  | Kbranch of int
  | Kbranchif of int
  | Kbranchifnot of int
  | Kcur of int
  | Kdummy of int
  | Kendlet of int
  | Kgetglobal of long_ident
  | Kgrab
  | Klabel of int
  | Klet
  | Kmakeblock of constr_tag * int
  | Kprim of prim
  | Kpush
  | Kpushmark
  | Kquote of constant
  | Kreturn
  | Ksetglobal of long_ident
  | Kswitch of int array
  | Ktermapply
  | Ktest of bool_test * int          (* branch if *)
  | Kupdate of int

let show_zinc =
  let output_long_ident () id = string_of_long_ident id in
  let output_label () = function
    | -1 -> ""
    | i -> Printf.sprintf "(%d)" i
  in
  function
  | Kaccess i -> Printf.sprintf "Kaccess(%d)" i
  | Kapply -> "Kapply"
  | Kbranch l -> Printf.sprintf "Kbranch%a" output_label l
  | Kbranchif l -> Printf.sprintf "Kbranchif%a" output_label l
  | Kbranchifnot l -> Printf.sprintf "Kbranchifnot%a" output_label l
  | Kcur l -> Printf.sprintf "Kcur%a" output_label l
  | Kdummy i -> Printf.sprintf "Kdummy(%d)" i
  | Kendlet i -> Printf.sprintf "Kendlet(%d)" i
  | Kgetglobal l -> Printf.sprintf "Kgetglobal(%a)" output_long_ident l
  | Kgrab -> "Kgrab"
  | Klabel l -> Printf.sprintf "Klabel%a" output_label l
  | Klet -> "Klet"
  | Kmakeblock((n,t),cnt) -> Printf.sprintf "Kmakeblock((%d,%d),%d)" n t cnt
  | Kprim prim -> "Kprim(" ^ show_prim prim ^ ")"
  | Kpush -> "Kpush"
  | Kpushmark -> "Kpushmark"
  | Kquote c -> Printf.sprintf "Kquote(%a)" (fun () -> show_constant) c
  | Kreturn -> "Kreturn"
  | Ksetglobal l -> Printf.sprintf "Ksetglobal(%a)" output_long_ident l
  | Kswitch ls ->
      let buf = Buffer.create 0 in
      Array.iteri (fun i l ->
        if i > 0 then
          Buffer.add_string buf "; ";
        Buffer.add_string buf (string_of_int l)
      ) ls;
      "Kswitch[" ^ Buffer.contents buf ^ "]"
  | Ktermapply -> "Ktermapply"
  | Ktest(tst,l) -> Printf.sprintf "Ktest(%s,%d)" (show_bool_test tst) l
  | Kupdate i -> Printf.sprintf "Kupdate(%d)" i

let dump_zinc cs =
  List.iter (fun c ->
    print_string (show_zinc c);
    print_char ' ') cs
