open Emit
open Error
open Exe
open Opcode
open Syntax

exception Invalid

let global_tbl = Hashtbl.create 257
let global_tbl_used = ref 0
let const_tbl = Hashtbl.create 257
let prim_tbl = Hashtbl.create 257
let tag_tbl = Hashtbl.create 257

let make_slot_for_const c =
  try
    Hashtbl.find const_tbl c
  with Not_found ->
    let s = !global_tbl_used in
    if s >= 65536 then (
      prerr_endline "Used more than 65536 global table slots.";
      exit 1
    );
    incr global_tbl_used;
    Hashtbl.replace const_tbl c s;
    if !Implementation.verbose then
      Printf.printf "%s: slot %d of global table\n" (show_constant c) s;
    s

let make_slot_for_global id =
  try
    Hashtbl.find global_tbl id
  with Not_found ->
    let s = !global_tbl_used in
    if s >= 65536 then (
      prerr_endline "Used more than 65536 global table slots.";
      exit 1
    );
    incr global_tbl_used;
    Hashtbl.replace global_tbl id s;
    if !Implementation.verbose then
      Printf.printf "%s: slot %d of global table\n" (string_of_long_ident id) s;
    s

let make_slot_for_tag (id,stamp as tag) =
  try
    Hashtbl.find tag_tbl tag
  with Not_found ->
    let s = Hashtbl.length tag_tbl in
    if s >= 256 then (
      prerr_endline "Used more than 65536 tag table slots.";
      exit 1
    );
    Hashtbl.replace tag_tbl tag s;
    if !Implementation.verbose then
      Printf.printf "%s,%d: slot %d of tag table\n" (string_of_long_ident id)
      stamp s;
    s

let get_slot_for_global id =
  try
    Hashtbl.find global_tbl id
  with Not_found ->
    Printf.eprintf "The global value \"%s\" is undefined.\n" (string_of_long_ident id);
    exit 1

let get_num_of_prim name =
  try
    Hashtbl.find prim_tbl name
  with Not_found ->
    Printf.eprintf "The C primitive \"%s\" is not available.\n" name;
    exit 1

let dump_data oc =
  let pos = ref 0 in
  let buf = ref (Bytes.create 16) in
  let o u8 =
    let len = Bytes.length !buf in
    if !pos >= len then (
      let newbuf = Bytes.create (2*len) in
      Bytes.blit !buf 0 newbuf 0 !pos;
      buf := newbuf
    );
    Bytes.set !buf !pos (Int32.(logand u8 255l |> to_int) |> char_of_int);
    incr pos
  in
  let o' u8 =
    let len = Bytes.length !buf in
    if !pos >= len then (
      let newbuf = Bytes.create (2*len) in
      Bytes.blit !buf 0 newbuf 0 !pos;
      buf := newbuf
    );
    Bytes.set !buf !pos (Int64.(logand u8 255L |> to_int) |> char_of_int);
    incr pos
  in
  let oooo u32 =
    o u32;
    o Int32.(shift_right u32 8);
    o Int32.(shift_right u32 16);
    o Int32.(shift_right u32 24)
  in
  let oooo' u64 =
    o' u64;
    o' Int64.(shift_right u64 8);
    o' Int64.(shift_right u64 16);
    o' Int64.(shift_right u64 24);
    o' Int64.(shift_right u64 32);
    o' Int64.(shift_right u64 40);
    o' Int64.(shift_right u64 48);
    o' Int64.(shift_right u64 56)
  in

  let entries = ref [] and last = ref (-1) in
  Hashtbl.iter (fun c slot -> entries := (slot,c) :: !entries) const_tbl;
  entries := List.sort compare !entries;
  if Config.word_size = 32 then (
    List.iter (fun (slot,c) ->
      for i = !last+1 to slot-1 do
        o 1l;
        oooo 1l
      done;
      last := slot;
      match c with
      | Const_char x ->
          o 1l;
          oooo Int32.(int_of_char x*2+1 |> of_int)
      | Const_int x ->
          o 1l;
          oooo Int32.(add (mul (of_int x) 2l) 1l)
      | Const_float x ->
          let x = Int64.bits_of_float x in
          o 0l;
          oooo (make_header double_tag 2);
          oooo (Int64.to_int32 x);
          oooo Int64.(shift_right x 32 |> to_int32)
      | Const_string x ->
          let len = String.length x in
          let w = Config.word_size/8 in
          let size = len/w+1 in
          o 0l;
          oooo (make_string_header size);
          String.iter (fun ch -> int_of_char ch |> Int32.of_int |> o) x;
          let pad = w - len mod w in
          for i = 1 to pad do
            o (Int32.of_int pad)
          done
    ) !entries;
    for i = !last+1 to Hashtbl.length global_tbl - 1 do
      o 1l;
      oooo 1l
    done
  ) else (
    List.iter (fun (slot,c) ->
      for i = !last+1 to slot-1 do
        o 1l;
        oooo' 1L
      done;
      last := slot;
      match c with
      | Const_char x ->
          o 1l;
          oooo' Int64.(int_of_char x*2+1 |> of_int)
      | Const_int x ->
          o 1l;
          oooo' Int64.(add (mul (of_int x) 2L) 1L)
      | Const_float x ->
          o 0l;
          oooo' (make_header' double_tag 1);
          oooo' (Int64.bits_of_float x)
      | Const_string x ->
          let len = String.length x in
          let w = Config.word_size/8 in
          let size = len/w+1 in
          o 0l;
          oooo' (make_string_header' size);
          String.iter (fun ch -> int_of_char ch |> Int32.of_int |> o) x;
          let pad = w - len mod w in
          for i = 1 to pad do
            o (Int32.of_int pad)
          done
    ) !entries;
    for i = !last+1 to Hashtbl.length global_tbl - 1 do
      o 1l;
      oooo' 1L
    done
  );
  output oc !buf 0 !pos

let link objs exefile =
  let oc = open_out_bin exefile in
  let scan first objfile =
    if not (Filename.check_suffix objfile ".zo") then (
      Printf.eprintf "Object files should be `*.zo'\n";
      exit 1
    );
    let buf = Bytes.create 256 in
    let ic = open_in_bin objfile in
    if input ic buf 0 4 <> 4 then (
      Printf.eprintf "Object file \"%s\" is invalid\n" objfile;
      exit 2
    );
    if Bytes.sub_string buf 0 4 = Config.obj_magic32 then (
      if Config.word_size <> 32 then (
        Printf.eprintf "Cannot link 32-bit object file \"%s\".\n" objfile;
        exit 2
      )
    ) else if Bytes.sub_string buf 0 4 = Config.obj_magic64 then (
      if Config.word_size <> 64 then (
        Printf.eprintf "Cannot link 32-bit object file \"%s\".\n" objfile;
        exit 2
      )
    ) else (
      Printf.eprintf "Object file \"%s\" has invalid magic.\n" objfile;
      exit 2
    );
    let phr_idx_off = input_bin_int ic in
    seek_in ic phr_idx_off;
    let phr_idx = (input_value ic : compiled_phrase list) in
    if first then
      List.iter (fun cph ->
        List.iter (fun (pos,reloc) ->
          match reloc with
          | Reloc_const c ->
              make_slot_for_const c |> ignore
          | Reloc_setglobal id ->
              make_slot_for_global id |> ignore
          | Reloc_tag(id,stamp) ->
              make_slot_for_tag (id,stamp) |> ignore
          | _ ->
              ()
        ) cph.cph_reloc
      ) phr_idx
    else
      List.iter (fun cph ->
        let buf = Bytes.create cph.cph_len in
        seek_in ic cph.cph_pos;
        really_input ic buf 0 cph.cph_len;
        List.iter (fun (pos,reloc) ->
          match reloc with
          | Reloc_const c ->
              let s = make_slot_for_const c in
              Bytes.set buf pos (char_of_int (s land 255));
              Bytes.set buf (pos+1) (char_of_int (s lsr 8))
          | Reloc_setglobal id
          | Reloc_getglobal id ->
              let s = make_slot_for_global id in
              Bytes.set buf pos (char_of_int (s land 255));
              Bytes.set buf (pos+1) (char_of_int (s lsr 8))
          | Reloc_prim name ->
              let s = get_num_of_prim name in
              Bytes.set buf pos (char_of_int s)
          | Reloc_tag(id,stamp) ->
              let s = make_slot_for_tag (id,stamp) in
              Bytes.set buf pos (char_of_int s)
        ) cph.cph_reloc;
        output_bytes oc buf
      ) phr_idx
  in
  List.iter (scan true) objs;
  output_bytes oc Config.exe_magic;
  output_bin_int oc 0; (* global data offset *)
  output_bin_int oc 0; (* global data num *)
  List.iter (scan false) objs;
  output_byte oc opSTOP;
  let global_off = pos_out oc in
  dump_data oc;
  seek_out oc 4;
  output_bin_int oc global_off;
  output_bin_int oc !global_tbl_used

let init () =
  Array.iteri (fun i name ->
    Hashtbl.replace prim_tbl name i)
    Cprim.name_of_prims;
  List.iter (fun tag ->
    match tag with
    | Constr_tag_regular _ -> ()
    | Constr_tag_extensible(id,stamp) ->
        ignore @@ make_slot_for_tag (id,stamp))
    [ Builtin.match_failure_tag
    ; Builtin.division_by_zero_tag
    ]
