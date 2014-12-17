open Error
open Instruction

(* code buffer *)

let out_buf = ref (Bytes.create 256 ' ')
let out_pos = ref 0

let o b =
  let len = Bytes.length out_buf in
  if !out_pos >= len then (
    let newbuf = Bytes.create (len*2) ' ' in
    Bytes.blit out_buf 0 newbuf 0 len;
    out_buf := newbuf
  );
  Bytes.set !out_buf !out_pos (char_of_int b);
  incr out_pos

let oo w =
  if w < -32768 || 32767 < w then
    displacement_overflow ()
  else (
    o w;
    o (w lsr 8)
  )

(* label *)

type label_def =
  | Label_defined of int
  | Label_undefined of (int * int) list

let label_tbl = ref [||]
let extend_label_tbl l =
  let len = Array.length !label_tbl in
  let newtbl = Array.make ((l/len+1)*len) (Label_undifiend []) in
  for i = 0 to len-1 do
    newtbl.(i) <- (!label_tbl).(i)
  done;
  label_tbl := newtbl

let define_label l =
  if l >= Array.length !label_tbl then
    extend_label_tbl l;
  match (!label_tbl).(l) with
  | Label_defined _ ->
      fatal_error "define_label: already defined"
  | Label_undefined ls ->
      let curr_pos = !out_pos in
      (!label_tbl).(l) <- Label_defined curr_pos;
      List.iter (fun (pos,orig) ->
        out_pos := pos;
        o (curr_pos-orig)
      ) ls

let out_label_with_orig orig l =
  if l >= Array.length !label_tbl then
    extend_label_tbl l;
  match (!label_tbl).(l) with
  | Label_defined pos ->
      o (pos-orig)
  | Label_undefined ls ->
      (!label_tbl).(l) <- Label_undefined ((!out_pos,orig)::ls);
      o 0

(* relocation *)

type reloc_item =
  | Reloc_const of constant
  | Reloc_getglobal of long_ident
  | Reloc_setglobal of long_ident
  | Reloc_prim of string

let relocs = ref []
let switch_relocs = ref []
let enter info =
  relocs := (Buffer.length buf, info) :: !relocs
let slot_for_prim name =
  enter (Reloc_prim name);
  o 0
let slot_for_const c =
  enter (Reloc_const c);
  oo 0
let slot_for_getglobal id =
  enter (Reloc_getglobal id);
  oo 0
let slot_for_setglobal id =
  enter (Reloc_setglobal id);
  oo 0

let rec emit =
  let symtbl = Hashtbl.create 37 in
  let output_const_int i =
    if (-128-1)/2 <= i && i <= (127-1)/2 then (
      o CONSTINT8;
      o (i+i+1)
    ) else if (-32768-1)/2 <= i && i <= (32767-1)/2 then (
      o CONSTINT16;
      oo (i+i+1)
    ) else (
      o GETGLOBAL;
      slot_for_literal (Const_int i)
    )
  in
  let out_header (n,t) =
    if Sys.word_size = 32 then (
      o t; (* [0,8) *)
      o 0; (* [8,16) *)
      o (n lsl 4); (* [16,24) *)
      o (n lsr 4) (* [24,32) *)
    ) else (
      o t; (* [0,8) *)
      o 0; (* [8,16) *)
      o 0; (* [16,24) *)
      o 0; (* [24,32) *)
      o (n lsl 4); (* [32,40) *)
      o (n lsr 4); (* [40,48) *)
      o (n lsr 12); (* [48,56) *)
      o (n lsr 20) (* [56,64) *)
    )
  in
  let inst = function
    | Kaccess n -> o ACCESS; o n
    | Kapply -> o APPLY
    | Kbranch l -> o BRANCH; o l
    | Kbranchif l -> o BRANCHIF; o l
    | Kbranchifnot l -> o BRANCHIFNOT; o l
    | Kcur l -> o CUR; o l
    | Kdummy n -> o DUMMY; o n
    | Kendlet n -> o ENDLET; o n
    | Kgetglobal id -> o GETGLOBAL; slot_for_getglobal id
    | Kgrab -> o GRAB
    | Klabel l -> o LABEL; o l
    | Klet -> o LET
    | Kmakeblock(n,t) -> o MAKEBLOCK; out_header (n,t);
    | Kprim prim ->
        begin match prim with
        | Pccall(name,arity) ->
            if arity <= 4 then
              o (CCALL1+arity-1)
            else
              not_implemented()
        | Pdummy n ->
            o DUMMY; o n
        | Pfield n ->
            o GETFIELD; o n
        | Psetfield n ->
            o SETFIELD; o n
        | Ptest t ->
            begin match t with
            | Ptest_eq -> o EQ
            | Ptest_neq -> o NEQ
            | Ptest_int t ->
                begin match t with
                | Peq -> EQ
                | Pneq -> NEQ
                | Plt -> LT
                | Ple -> LE
                | Pgt -> GT
                | Pge -> GE
                end
            | Ptest_float t ->
                begin match t with
                | Peq -> EQFLOAT
                | Pneq -> NEQFLOAT
                | Plt -> LTFLOAT
                | Ple -> LEFLOAT
                | Pgt -> GTFLOAT
                | Pge -> GEFLOAT
                end
            | Ptest_string t ->
                begin match t with
                | Peq -> EQSTRING
                | Pneq -> NEQSTRING
                | Plt -> LTSTRING
                | Ple -> LESTRING
                | Pgt -> GTSTRING
                | Pge -> GESTRING
                end
            end
        end
    | Kpush -> o PUSH
    | Kpushmark -> o PUSHMARK
    | Kquote c ->
        begin match c with
        | Const_char x -> out_const_int (int_of_char x)
        | Const_int x -> out_const_int x
        | Const_float x -> o GETGLOBAL; slot_for_const c
        end
    | Kreturn -> o RETURN
    | Ksetglobal id -> o SETGLOBAL; slot_for_setglobal id
    | Kswitch ls ->
        o SWITCH;
        o (Array.length ls);
        let orig = Buffer.length buf in
        Array.iter (out_lebel_with_orig orig) ls
    | Ktermapply -> o TERMAPPLY
    | Ktest(tst,l) ->
        begin match tst with
        | Ptest_int(Pneqimm x) ->
            o PUSH; o PUSH; out_const_int i;
            out EQ; out POPBRANCHIFNOT; out_label l
        | Ptest_float(Pneqimm x) ->
            o PUSH; o PUSH; out GETGLOBAL; slot_for_constant x;
            out EQFLOAT; out POPBRANCHIFNOT; out_label l
        | Ptest_string(Pneqimm x) ->
            o PUSH; o PUSH; out GETGLOBAL; slot_for_constant x;
            out EQSTRING; out POPBRANCHIFNOT; out_label l
        | _ -> assert false
        end
    | Kupdate n -> o UPDATE; o n
