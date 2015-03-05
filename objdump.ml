open Cprim
open Emit
open Exe
open Opcode

exception Invalid

let jumptbl = Array.make (opXORINT+1) (fun _ _ -> ())
let relocatable = ref false

let init_jumptbl () =
  let i8 ic _ =
    let b = input_byte ic in
    Printf.printf "%d" (if b >= 128 then b-256 else b) in
  let u8 ic _ =
    Printf.printf "%d" (input_byte ic) in
  let prim ic _ =
    if !relocatable then
      Printf.printf "%d" (input_byte ic)
    else
      Printf.printf "%s" name_of_prims.(input_byte ic) in
  let i16 ic pos =
    if pos mod 2 <> 0 then
      input_byte ic |> ignore;
    let b0 = input_byte ic in
    let b1 = input_byte ic in
    let n = b0 + b1 lsl 8 in
    let n =
      if n >= 32768 then n-65536
      else n
    in
    Printf.printf "%d" n
  in
  let u16 ic pos =
    if pos mod 2 <> 0 then
      input_byte ic |> ignore;
    let b0 = input_byte ic in
    let b1 = input_byte ic in
    let n = b0 + b1 lsl 8 in
    Printf.printf "[%d]" n
  in
  let rel16 ic pos =
    let pos =
      if pos mod 2 <> 0 then (
        input_byte ic |> ignore;
        pos+1
      ) else
        pos
    in
    let b0 = input_byte ic in
    let b1 = input_byte ic in
    let n = b0 + b1 lsl 8 in
    let n =
      if n >= 32768 then n-65536
      else n
    in
    Printf.printf "0x%04x" (pos+n)
  in
  let makearray ic pos =
    let init = input_byte ic in
    if init <> 0 then
      print_string "init"
  in
  let makeblock ic pos =
    if pos mod Config.sizeof_word <> 0 then
      for i = pos mod Config.sizeof_word to Config.sizeof_word-1 do
        input_byte ic |> ignore
      done;
    Printf.printf "0x%08lx" (input_bin_int32 ic)
  in
  let branchifneqtag ic pos =
    Printf.printf "%d " (input_byte ic);
    rel16 ic (pos+1)
  in
  let switch ic pos =
    let nalts = input_byte ic in
    let pos =
      if (pos+1) mod 2 <> 0 then (
        input_byte ic |> ignore;
        pos+2
      ) else
        pos+1
    in
    print_char '[';
    for i = 1 to nalts do
      let b0 = input_byte ic in
      let b1 = input_byte ic in
      let n = b0 + b1 lsl 8 in
      let n =
        if n >= 32768 then n-65536
        else n
      in
      if i > 1 then
        print_string "; ";
      Printf.printf "%04x" (pos+n);
    done;
    print_char ']'
  in
  jumptbl.(opACCESS) <- u8;
  jumptbl.(opATOM) <- u8;
  jumptbl.(opBRANCH) <- rel16;
  jumptbl.(opBRANCHIF) <- rel16;
  jumptbl.(opBRANCHIFEQ) <- rel16;
  jumptbl.(opBRANCHIFGE) <- rel16;
  jumptbl.(opBRANCHIFGT) <- rel16;
  jumptbl.(opBRANCHIFLE) <- rel16;
  jumptbl.(opBRANCHIFLT) <- rel16;
  jumptbl.(opBRANCHIFNEQ) <- rel16;
  jumptbl.(opBRANCHIFNOT) <- rel16;
  jumptbl.(opCCALL1) <- prim;
  jumptbl.(opCCALL2) <- prim;
  jumptbl.(opCCALL3) <- prim;
  jumptbl.(opCCALL4) <- prim;
  jumptbl.(opCONSTINT8) <- i8;
  jumptbl.(opCONSTINT16) <- i16;
  jumptbl.(opCUR) <- rel16;
  jumptbl.(opDUMMY) <- u8;
  jumptbl.(opENDLET) <- u8;
  jumptbl.(opGETFIELD) <- u8;
  jumptbl.(opGETGLOBAL) <- u16;
  jumptbl.(opMAKEARRAY) <- makearray;
  jumptbl.(opMAKEBLOCK) <- makeblock;
  jumptbl.(opBRANCHIFNEQTAG) <- branchifneqtag;
  jumptbl.(opPUSHTRAP) <- rel16;
  jumptbl.(opSETFIELD) <- u8;
  jumptbl.(opSETGLOBAL) <- u16;
  jumptbl.(opSWITCH) <- switch;
  jumptbl.(opUPDATE) <- u8

let print_code ic len =
  let start = pos_in ic in
  let stop = start+len in
  while pos_in ic < stop do
    Printf.printf "%04x: " (pos_in ic);
    let op = input_byte ic in
    Printf.printf "%s " name_of_opcodes.(op);
    jumptbl.(op) ic (pos_in ic);
    print_char '\n'
  done

let print_phr_entry ic phr =
  Printf.printf "\nOffset %d\nLength %d\n" phr.cph_pos phr.cph_len;
  seek_in ic phr.cph_pos;
  print_code ic phr.cph_len

let iiii ic =
  let bs = Array.init 4 (fun _ -> input_byte ic |> Int32.of_int) in
  Array.fold_right (fun b acc -> Int32.(add (mul acc 256l) b)) bs 0l

let iiii' ic =
  let bs = Array.init 8 (fun _ -> input_byte ic |> Int64.of_int) in
  Array.fold_right (fun b acc -> Int64.(add (mul acc 256L) b)) bs 0L

let print_value ic slot =
  let kind = input_byte ic in
  Printf.printf "  %d:\n" slot;
  if Config.word_size = 32 then
    match kind with
    | 1 ->
        let x = iiii ic in
        if Int32.logand x 1l = 0l then
          assert false
        else
          Printf.printf "    int: %ld\n" Int32.(div (sub x 1l) 2l)
    | 0 ->
        let hd = iiii ic in
        let tag = tag_hd hd in
        Printf.printf "    header: %08lx\n" hd;
        Printf.printf "    tag: %s\n" (name_tag tag);
        if tag = double_tag then (
          let x = Int64.float_of_bits (iiii' ic) in
          Printf.printf "    size: %d\n" (size_hd hd);
          Printf.printf "    float: %f\n" x
        ) else if tag = string_tag then (
          let size = string_size_hd hd in
          Printf.printf "    size: %d\n" size;
          print_string "    string: ";
          for i = 1 to 4*(size-1) do
            print_string @@ Char.escaped (input_char ic)
          done;
          let bs = Array.init 4 (fun _ -> input_byte ic) in
          for i = 0 to 3-bs.(3) do
            print_string @@ Char.escaped @@ char_of_int bs.(i)
          done;
          print_char '\n'
        ) else (
          let size = size_hd hd in
          Printf.printf "    size: %d\n" size;
          for i = 1 to size do iiii ic |> ignore done
        )
    | _ ->
        assert false
  else
    match kind with
    | 1 ->
        let x = iiii' ic in
        if Int64.logand x 1L = 0L then
          assert false
        else
          Printf.printf "    int: %Ld\n" Int64.(div (sub x 1L) 2L)
    | 0 ->
        let hd = iiii' ic in
        let tag = tag_hd' hd in
        Printf.printf "    header: %016Lx\n" hd;
        Printf.printf "    tag: %s\n" (name_tag tag);
        if tag = double_tag then (
          let x = Int64.float_of_bits (iiii' ic) in
          Printf.printf "    size: %d\n" (size_hd' hd);
          Printf.printf "    float: %f\n" x
        ) else if tag = string_tag then (
          let size = string_size_hd' hd in
          Printf.printf "    size: %d\n" size;
          print_string "    string: ";
          for i = 1 to 8*(size-1) do
            print_string @@ Char.escaped (input_char ic)
          done;
          let bs = Array.init 8 (fun _ -> input_byte ic) in
          for i = 0 to 7-bs.(7) do
            print_char @@ char_of_int bs.(i)
          done;
          print_char '\n'
        ) else (
          let size = size_hd' hd in
          Printf.printf "    size: %d\n" size;
          for i = 1 to size do iiii' ic |> ignore done
        )
    | _ ->
        assert false

let dump filename =
  let go () =
    Printf.printf "File %s\n" filename;
    let buf = Bytes.create 256 in
    let ic = open_in_bin filename in
    if input ic buf 0 4 <> 4 then
      raise Invalid;
    let magic = Bytes.sub_string buf 0 4 in
    if magic = Config.obj_magic then (
      relocatable := true;
      print_endline "Relocatable file";
      let phr_idx_off = input_bin_int ic in
      seek_in ic phr_idx_off;
      let phr_idx = (input_value ic : compiled_phrase list) in
      List.iter (print_phr_entry ic) phr_idx
    ) else if magic = Config.exe_magic then (
      relocatable := false;
      print_endline "Executable";
      let global_off = input_bin_int ic in
      let global_num = input_bin_int ic in
      let code_len = global_off-12 in
      Printf.printf "\nLength %d\n" code_len;
      print_code ic code_len;
      Printf.printf "\nGlobal value: %d\n" global_num;
      for i = 0 to global_num-1 do
        print_value ic i
      done
    ) else
      raise Invalid
  in
  try go ()
  with Invalid | End_of_file -> prerr_endline "Invalid obj file"; exit 1

let () =
  let program_desc = "camlfwod [OBJECT|EXE]\n\
    Caml Featherweight objdump\n\n\
    camlfwod displays information about object files\n\
    or bytecode executable files.\n"
  in
  let files = ref [] in
  init_jumptbl();
  Arg.parse []
    (fun file -> files := file :: !files)
    program_desc;
  if !files = [] then (
    prerr_string @@ Arg.usage_string [] program_desc;
    exit 1
  ) else
    List.iter dump (List.rev !files)
