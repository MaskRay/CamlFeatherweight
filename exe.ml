let size_offset =
  if Config.word_size = 32 then
    20
  else
    36

let gcsize_offset = 8

let tag_hd hd = Int32.(logand hd 255l |> to_int)
let size_hd hd = Int32.(shift_right hd size_offset |> to_int)
let string_size_hd hd = Int32.(shift_right hd (gcsize_offset+1) |> to_int)

let no_scan_tag = (1 lsl 8) - 5

let closure_tag = no_scan_tag-1
let abstract_tag = no_scan_tag
let string_tag = no_scan_tag+1
let array_tag = no_scan_tag+2
let double_tag = no_scan_tag+3

let make_header tag size =
  Int32.(add (shift_left (of_int size) size_offset) (of_int tag))

let make_header' tag size =
  Int64.(add (shift_left (of_int size) size_offset) (of_int tag))

let make_string_header size =
  Int32.(add (shift_left (of_int size) (gcsize_offset+1)) (of_int string_tag))

let make_string_header' size =
  Int64.(add (shift_left (of_int size) (gcsize_offset+1)) (of_int string_tag))

let name_tag tag =
  if tag = closure_tag then
    "closure"
  else if tag = double_tag then
    "double"
  else if tag = array_tag then
    "array"
  else if tag = string_tag then
    "string"
  else
    raise @@ Invalid_argument(Printf.sprintf "Unknown tag %d" tag)

let input_bin_int32 ic =
  let b0 = input_byte ic |> Int32.of_int in
  let b1 = input_byte ic |> Int32.of_int in
  let b2 = input_byte ic |> Int32.of_int in
  let b3 = input_byte ic |> Int32.of_int in
  Int32.(mul b3 256l |> add b2 |> mul 256l |> add b1 |> mul 256l |> add b0)

let input_bin_int ic =
  input_bin_int32 ic |> Int32.to_int

let output_bin_int oc i =
  output_byte oc (i land 255);
  output_byte oc (i lsr 8 land 255);
  output_byte oc (i lsr 16 land 255);
  output_byte oc (i lsr 24 land 255)
