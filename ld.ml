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

let verbose = ref false

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
    if !verbose then
      Printf.printf "%s: slot %d of global table" (show_constant c) s;
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
    if !verbose then
      Printf.printf "%s: slot %d of global table" (string_of_long_ident id) s;
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
  let oooo u32 =
    o u32;
    o Int32.(shift_right u32 8);
    o Int32.(shift_right u32 16);
    o Int32.(shift_right u32 24)
  in

  let entries = ref [] and last = ref (-1) in
  Hashtbl.iter (fun c slot -> entries := (slot,c) :: !entries) const_tbl;
  entries := List.sort compare !entries;
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
        let size = len/4+1 in
        o 0l;
        oooo (make_string_header size);
        String.iter (fun ch -> int_of_char ch |> Int32.of_int |> o) x;
        let pad = 4 - len mod 4 in
        for i = 1 to pad do
          o (Int32.of_int pad)
        done
  ) !entries;
  output oc !buf 0 !pos

let link objs exe =
  let oc = open_out_bin exe in
  let scan first obj =
    if not (Filename.check_suffix obj ".zo") then (
      Printf.eprintf "Object files should be `*.zo'\n";
      exit 1
    );
    let buf = Bytes.create 256 in
    let ic = open_in_bin obj in
    if input ic buf 0 4 <> 4 || Bytes.sub_string buf 0 4 <> "meow" then
      raise Invalid;
    let phr_idx_off = input_binary_int ic in
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
        ) cph.cph_reloc;
        output_bytes oc buf
      ) phr_idx
  in
  List.iter (scan true) objs;
  output_bytes oc "woem";
  output_binary_int oc 0; (* global data offset *)
  output_binary_int oc 0; (* global data num *)
  List.iter (scan false) objs;
  output_byte oc opSTOP;
  let global_off = pos_out oc in
  dump_data oc;
  seek_out oc 4;
  output_binary_int oc global_off;
  output_binary_int oc !global_tbl_used

let () =
  Array.iteri (fun i name -> Hashtbl.replace prim_tbl name i) Cprim.name_of_prims;
  let files = ref [] in
  let exe = ref "a.out" in
  Arg.parse
    [ "-o", Arg.String(fun filename -> exe := filename), "outfile"
    ; "-v", Arg.Unit(fun () -> verbose := true), "verbose"
    ]
    (fun filename -> files := filename :: !files)
    ("ML ld");
  link (List.rev !files) !exe
