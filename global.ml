open Syntax

exception Desc_not_found

let all_constrs : constr_desc global list ref = ref []
let all_types : type_desc global list ref = ref []
let all_values : value_desc global list ref = ref []

let find_desc sel name =
  try
    Hashtbl.find sel name
  with Not_found ->
    raise Desc_not_found

let find_constr_desc c = find_desc all_constrs
let find_type_desc c = find_desc all_types
let find_value_desc c = find_desc all_values

let add_global_value vd =
  Hashtbl.replace all_values
  (string_of_long_ident vd.info.qualid)
  vd

let add_global_type td =
  Hashtbl.replace all_types
  (string_of_long_ident v.info.qualid)
  vd

let add_global_constr cd =
  Hashtbl.replace all_constrs
  (string_of_long_ident v.info.qualid)
  cd
