open Emit
open Opcode

exception Invalid

let jumptbl = Array.make (opXORINT+1) (fun _ _ -> ())

let init_jumptbl () =
  let i8 ic _ =
    let b = input_byte ic in
    Printf.printf "%d" (if b >= 128 then b-256 else b) in
  let u8 ic _ =
    Printf.printf "%d" (input_byte ic) in
  let i16 ic _ =
    let b0 = input_byte ic in
    let b1 = input_byte ic in
    let n = b0 + b1 lsl 8 in
    let n =
      if n >= 32768 then n-65536
      else n
    in
    Printf.printf "%d" n
  in
  let u16 ic _ =
    let b0 = input_byte ic in
    let b1 = input_byte ic in
    let n = b0 + b1 lsl 8 in
    Printf.printf "%d" n
  in
  let rel16 ic pos =
    let b0 = input_byte ic in
    let b1 = input_byte ic in
    let n = b0 + b1 lsl 8 in
    let n =
      if n >= 32768 then n-65536
      else n
    in
    Printf.printf "0x%04x" (pos+n) in
  let in_header ic =
    if Sys.word_size = 32 then (
      let bs = Array.init 4 (fun _ -> input_byte ic |> Int32.of_int) in
      Array.fold_right (fun b acc -> Int32.(add (mul acc 256l) b)) bs 0l
      |> Printf.printf "%ld"
    ) else (
      let bs = Array.init 8 (fun _ -> input_byte ic |> Int64.of_int) in
      Array.fold_right (fun b acc -> Int64.(add (mul acc 256L) b)) bs 0L
      |> Printf.printf "%Ld"
    )
  in
  jumptbl.(opACCESS) <- u8;
  jumptbl.(opBRANCH) <- rel16;
  jumptbl.(opBRANCHIF) <- rel16;
  jumptbl.(opBRANCHIFEQ) <- rel16;
  jumptbl.(opBRANCHIFGE) <- rel16;
  jumptbl.(opBRANCHIFGT) <- rel16;
  jumptbl.(opBRANCHIFLE) <- rel16;
  jumptbl.(opBRANCHIFLT) <- rel16;
  jumptbl.(opBRANCHIFNEQ) <- rel16;
  jumptbl.(opBRANCHIFNOT) <- rel16;
  jumptbl.(opCCALL1) <- u16;
  jumptbl.(opCCALL2) <- u16;
  jumptbl.(opCCALL3) <- u16;
  jumptbl.(opCCALL4) <- u16;
  jumptbl.(opCONSTINT8) <- i8;
  jumptbl.(opCONSTINT16) <- i16;
  jumptbl.(opCUR) <- rel16;
  jumptbl.(opDUMMY) <- u8;
  jumptbl.(opENDLET) <- u8;
  jumptbl.(opGETFIELD) <- u8;
  jumptbl.(opGETGLOBAL) <- u16;
  jumptbl.(opSETFIELD) <- u8;
  jumptbl.(opSETGLOBAL) <- u16;
  jumptbl.(opUPDATE) <- u8

let print_code ic len =
  let start = pos_in ic in
  let stop = start+len in
  while pos_in ic < stop do
    Printf.printf "%04x: " (pos_in ic-start);
    let op = input_byte ic in
    Printf.printf "%s " name_of_opcodes.(op);
    jumptbl.(op) ic (pos_in ic-start);
    print_char '\n'
  done

let print_entry ic phr =
  Printf.printf "\nOffset %d\nLength %d\n" phr.cph_pos phr.cph_len;
  seek_in ic phr.cph_pos;
  print_code ic phr.cph_len

let dump filename =
  let go () =
    Printf.printf "File %s\n" filename;
    let buf = Bytes.create 256 in
    let ic = open_in_bin filename in
    if input ic buf 0 4 <> 4 || Bytes.sub_string buf 0 4 <> "meow" then
      raise Invalid;
    let phr_idx_off = input_binary_int ic in
    seek_in ic phr_idx_off;
    let phr_idx = List.rev (input_value ic : compiled_phrase list) in
    List.iter (print_entry ic) phr_idx
  in
  try go ()
  with Invalid | End_of_file -> prerr_endline "Invalid obj file"

let () =
  init_jumptbl();
  Arg.parse
    []
    dump
    ("ML objdump")
