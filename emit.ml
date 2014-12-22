open Error
open Exe
open Lambda
open Instruction
open Opcode
open Syntax

(* code buffer *)

let out_buf = ref (Bytes.create 256)
let out_pos = ref 0

let o b =
  let len = Bytes.length !out_buf in
  if !out_pos >= len then (
    let newbuf = Bytes.create (len*2) in
    Bytes.blit !out_buf 0 newbuf 0 len;
    out_buf := newbuf
  );
  Bytes.set !out_buf !out_pos (b land 255 |> char_of_int);
  incr out_pos

let oo w =
  if w < -32768 || 32767 < w then
    displacement_overflow ()
  else (
    o w;
    o (w lsr 8)
  )

let oooo w =
  oo (w land 65535);
  oo (w lsr 16 land 65535)

let out_test_int = function
  | Peq -> opEQ
  | Pneq -> opNEQ
  | Plt -> opLTINT
  | Ple -> opLEINT
  | Pgt -> opGTINT
  | Pge -> opGEINT
  | _ -> assert false

let out_test_int_b = function
  | Peq -> opBRANCHIFEQ
  | Pneq -> opBRANCHIFNEQ
  | Plt -> opBRANCHIFLT
  | Ple -> opBRANCHIFLE
  | Pgt -> opBRANCHIFGT
  | Pge -> opBRANCHIFGE
  | _ -> assert false

let out_test_float = function
  | Peq -> opEQFLOAT
  | Pneq -> opNEQFLOAT
  | Plt -> opLTFLOAT
  | Ple -> opLEFLOAT
  | Pgt -> opGTFLOAT
  | Pge -> opGEFLOAT
  | _ -> assert false

let out_test_string = function
  | Peq -> opEQSTRING
  | Pneq -> opNEQSTRING
  | Plt -> opLTSTRING
  | Ple -> opLESTRING
  | Pgt -> opGTSTRING
  | Pge -> opGESTRING
  | _ -> assert false

(* label *)

type label_def =
  | Label_defined of int
  | Label_undefined of (int * int) list

let label_tbl = ref [||]
let extend_label_tbl l =
  let len = Array.length !label_tbl in
  let newtbl = Array.make ((l/len+1)*len) (Label_undefined []) in
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
        oo (curr_pos-orig)
      ) ls;
      out_pos := curr_pos

let out_label_with_orig orig l =
  if l >= Array.length !label_tbl then
    extend_label_tbl l;
  match (!label_tbl).(l) with
  | Label_defined pos ->
      oo (pos-orig)
  | Label_undefined ls ->
      (!label_tbl).(l) <- Label_undefined ((!out_pos,orig)::ls);
      oo 0

let out_label l = out_label_with_orig !out_pos l

(* relocation *)

type reloc_entry =
  | Reloc_const of constant
  | Reloc_getglobal of long_ident
  | Reloc_setglobal of long_ident
  | Reloc_prim of string
  | Reloc_tag of long_ident * int

let relocs = ref []

let enter_reloc info =
  relocs := (!out_pos, info) :: !relocs

let slot_for_prim name =
  enter_reloc (Reloc_prim name);
  o 0

let slot_for_tag (id,stamp) =
  enter_reloc (Reloc_tag(id,stamp));
  o 0

let out_tag = function
  | Constr_tag_regular(n,t) ->
      o t
  | Constr_tag_extensible(id,stamp) ->
      slot_for_tag (id,stamp)

let slot_for_const c =
  enter_reloc (Reloc_const c);
  oo 0

let slot_for_getglobal id =
  enter_reloc (Reloc_getglobal id);
  oo 0

let slot_for_setglobal id =
  enter_reloc (Reloc_setglobal id);
  oo 0

let rec emit code =
  let out_const_int i =
    if -128 <= i && i < 128 then (
      o opCONSTINT8;
      o i
    ) else if -32768 <= i && i < 32768 then (
      o opCONSTINT16;
      oo i
    ) else (
      o opGETGLOBAL;
      slot_for_const (Const_int i)
    )
  in
  let out_header tag n =
    if Config.word_size = 32 then (
      out_tag tag; (* [0,8) *)
      o 0; (* [8,16) *)
      o (n lsl 4); (* [16,24) *)
      o (n lsr 4) (* [24,32) *)
    ) else (
      out_tag tag; (* [0,8) *)
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
    | Kaccess n -> o opACCESS; o n
    | Kapply -> o opAPPLY
    | Kbranch l -> o opBRANCH; out_label l
    | Kbranchif l -> o opBRANCHIF; out_label l
    | Kbranchifnot l -> o opBRANCHIFNOT; out_label l
    | Kcur l -> o opCUR; out_label l
    | Kdummy n -> o opDUMMY; o n
    | Kendlet n -> o opENDLET; o n
    | Kgetglobal id -> o opGETGLOBAL; slot_for_getglobal id
    | Kgrab -> o opGRAB
    | Klabel l -> define_label l
    | Klet -> o opLET
    | Kmakeblock(tag,n) -> o opMAKEBLOCK; out_header tag n
    | Kpoptrap -> o opPOPTRAP
    | Kprim prim ->
        begin match prim with
        | Paddint -> o opADDINT
        | Pandint -> o opANDINT
        | Pasrint -> o opASRINT
        | Pccall(arity,name) ->
            if arity <= 4 then (
              o (opCCALL1+arity-1);
              slot_for_prim name
            ) else
              not_implemented()
        | Pdecr -> o opDECR
        | Pdivint -> o opDIVINT
        | Pdummy n ->
            o opDUMMY; o n
        | Pfield n ->
            o opGETFIELD; o n
        | Pfloat(Paddfloat) -> o opADDFLOAT
        | Pfloat(Psubfloat) -> o opSUBFLOAT
        | Pfloat(Pmulfloat) -> o opMULFLOAT
        | Pfloat(Pdivfloat) -> o opDIVFLOAT
        | Pgetarrayitem ->
            o opGETARRAYITEM
        | Pgetstringitem ->
            o opGETSTRINGITEM
        | Pidentity -> ()
        | Pincr -> o opINCR
        | Plslint -> o opLSLINT
        | Plsrint -> o opLSRINT
        | Pmakearray init ->
            o opMAKEARRAY;
            o (if init then 1 else 0)
        | Pmakestring ->
            o opMAKESTRING
        | Pmodint -> o opMODINT
        | Pmulint -> o opMULINT
        | Pnegint -> o opNEGINT
        | Pnot -> o opNOT
        | Porint -> o opORINT
        | Praise ->
            o opRAISE
        | Psetfield n ->
            o opSETFIELD; o n
        | Psetarrayitem ->
            o opSETARRAYITEM
        | Psetstringitem ->
            o opSETSTRINGITEM
        | Pstringlength ->
            o opSTRINGLENGTH
        | Psubint -> o opSUBINT
        | Ptest t ->
            o begin match t with
            | Ptest_eq -> opEQ
            | Ptest_neq -> opNEQ
            | Ptest_int t -> out_test_int t
            | Ptest_float t -> out_test_float t
            | Ptest_string t -> out_test_string t
            | _ -> assert false
            end
        | Pxorint -> o opXORINT
        | _ ->
            dump_prim 3 prim;
            fatal_error "TODO"
        end
    | Kpush -> o opPUSH
    | Kpushmark -> o opPUSHMARK
    | Kpushtrap l ->
        o opPUSHTRAP;
        out_label l
    | Kquote c ->
        begin match c with
        | Const_block t ->
            o opATOM;
            o t
        | Const_base c ->
            match c with
            | Const_char x -> out_const_int (int_of_char x)
            | Const_int x -> out_const_int x
            | Const_float x -> o opGETGLOBAL; slot_for_const c
            | Const_string x -> o opGETGLOBAL; slot_for_const c
        end
    | Kreturn -> o opRETURN
    | Ksetglobal id -> o opSETGLOBAL; slot_for_setglobal id
    | Kswitch ls ->
        o opSWITCH;
        o (Array.length ls);
        let orig = !out_pos in
        Array.iter (out_label_with_orig orig) ls
    | Ktermapply -> o opTERMAPPLY
    | Ktest(tst,l) ->
        begin match tst with
        | Ptest_int(Pneqimm x) ->
            o opPUSH; o opPUSH; out_const_int x;
            o opEQ; o opPOPBRANCHIFNOT; out_label l
        | Ptest_int t ->
            o (out_test_int_b t);
            out_label l
        | Ptest_float(Pneqimm x) ->
            o opPUSH; o opPUSH; o opGETGLOBAL; slot_for_const (Const_float x);
            o opEQFLOAT; o opPOPBRANCHIFNOT; out_label l
        | Ptest_float t ->
            o (out_test_float t);
            o opBRANCHIF; out_label l
        | Ptest_string(Pneqimm x) ->
            o opPUSH; o opPUSH; o opGETGLOBAL; slot_for_const (Const_string x);
            o opEQSTRING; o opPOPBRANCHIFNOT; out_label l
        | Ptest_string t ->
            o (out_test_string t);
            o opBRANCHIF; out_label l
        | Ptest_noteqtag tag ->
            o opBRANCHIFNEQTAG;
            out_tag tag;
            out_label l
        | _ -> assert false
        end
    | Kupdate n -> o opUPDATE; o n
  in
  List.iter inst code

(* phrase *)

type compiled_phrase = {
  cph_pos: int;
  cph_len: int;
  cph_reloc: (int * reloc_entry) list
}

let phr_idx = ref []
let abs_out_pos = ref 0

let start_emit_phrase oc =
  phr_idx := [];
  output_string oc "meow";
  output_bin_int oc 0; (* placeholder of size *)
  abs_out_pos := 8

let emit_phrase oc (init,fcts) =
  out_pos := 0;
  relocs := [];
  label_tbl := [|Label_undefined []|];
  if fcts = [] then
    emit init
  else (
    emit init;
    emit [Kbranch 0];
    emit fcts;
    emit [Klabel 0]
  );
  output oc !out_buf 0 !out_pos;
  phr_idx := {
    cph_pos = !abs_out_pos;
    cph_len = !out_pos;
    cph_reloc = !relocs
  } :: !phr_idx;
  abs_out_pos := !abs_out_pos + !out_pos

let end_emit_phrase oc =
  output_value oc (List.rev !phr_idx);
  seek_out oc 4;
  output_bin_int oc !abs_out_pos
