open Syntax

let all_constrs : (string, constr_desc global) Hashtbl.t = Hashtbl.create 17
let all_types : (string, type_desc global) Hashtbl.t= Hashtbl.create 17
let all_values : (string, value_desc global) Hashtbl.t= Hashtbl.create 17

let find_desc sel name =
  Hashtbl.find sel name

let find_constr_desc = find_desc all_constrs
let find_type_desc = find_desc all_types
let find_value_desc = find_desc all_values

let add_global_value vd =
  Hashtbl.replace all_values
  (string_of_long_ident vd.qualid)
  vd

let add_global_type td =
  Hashtbl.replace all_types
  (string_of_long_ident td.qualid)
  td

let add_global_constr cd =
  Hashtbl.replace all_constrs
  (string_of_long_ident cd.qualid)
  cd
