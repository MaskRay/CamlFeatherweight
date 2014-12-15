open Syntax

type lambda =
  | Labstract of lambda                   (* lambda abstraction *)
  | Lapply of lambda * lambda list
  | Lcond of lambda * (constant * lambda) list
  | Lconst of constant
  | Lif of lambda * lambda * lambda
  | Llet of lambda list * lambda          (* local binding *)
  | Lletrec of lambda list * lambda       (* local recursive binding *)
  | Lprim of primitive * lambda list
  | Lsequence of lambda * lambda
  | Lstaticcatch of lambda * lambda
  | Lstaticraise                           (* failure of pattern matching *)
  | Lswitch of int * lambda * (constr_tag * lambda) list
  | Lvar of int                           (* access local variable *)

let dump_lambda d l =
  let rec go d l =
    Printf.printf "%*s" (2*d) "";
    match l with
    | Labstract l ->
        print_endline "Labstract";
        go (d+1) l
    | Lapply(l,ls) ->
        print_endline "Lapply";
        go (d+1) l;
        List.iter (go (d+1)) ls;
    | Lcond(path,alts) ->
        print_endline "Lcond";
        go (d+1) path;
        List.iter (fun (c,l) ->
          Printf.printf "%*s" (2*d+2) "";
          dump_constant c;
          go (d+2) l
        ) alts
    | Lconst c ->
        print_endline "Lconst";
        Printf.printf "%*s" (2*d+2) "";
        dump_constant c
    | Lif(cond,ifso,ifnot) ->
        print_endline "Lif";
        go (d+1) ifso;
        go (d+1) ifnot
    | Llet(binds,body) ->
        print_endline "Llet";
        Printf.printf "%*sBind\n" (2*d+2) "";
        List.iter (go (d+2)) binds;
        go (d+1) body
    | Lletrec(binds,body) ->
        print_endline "Lletrec";
        Printf.printf "%*sBind\n" (2*d+2) "";
        List.iter (go (d+2)) binds;
        go (d+1) body
    | Lprim(prim,args) ->
        print_endline "Lprim";
        dump_prim (d+1) prim;
        List.iter (go (d+1)) args
    | Lsequence(l1,l2) ->
        print_endline "Lsequence";
        go (d+1) l1;
        go (d+1) l2
    | Lstaticraise ->
        print_endline "Lstaticraise"
    | Lstaticcatch(l1,l2) ->
        print_endline "Lstaticcatch";
        go (d+1) l1;
        go (d+1) l2
    | Lswitch(span,path,alts) ->
        Printf.printf "Lswitch span=%d\n" span;
        go (d+1) path;
        List.iter (fun ((n,t),l) ->
          Printf.printf "%*s%d,%d\n" (2*d+2) "" n t;
          go (d+2) l
        ) alts
    | Lvar i ->
        Printf.printf "Lvar %d\n" i
  in
  go d l
