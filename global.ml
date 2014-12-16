open Syntax

let cur_module = ref "Main"

let all_constrs : (long_ident, constr_desc global) Hashtbl.t = Hashtbl.create 17
let all_types : (long_ident, type_desc global) Hashtbl.t= Hashtbl.create 17
let all_values : (long_ident, value_desc global) Hashtbl.t= Hashtbl.create 17

let find_desc sel name =
  Hashtbl.find sel name

let find_constr_desc = find_desc all_constrs
let find_type_desc = find_desc all_types
let find_value_desc = find_desc all_values

let add_global_value vd =
  Hashtbl.replace all_values
  vd.qualid
  vd

let add_global_type td =
  Hashtbl.replace all_types
  td.qualid
  td

let add_global_constr cd =
  Hashtbl.replace all_constrs
  cd.qualid
  cd
