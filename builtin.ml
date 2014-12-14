open Syntax
open Global

(* type_constr global *)

let f id =
  { qualid=Lident id; info={ ty_stamp=new_type_stamp(); ty_abbr=Tnotabbrev } }
let type_constr_unit   = f "unit"
let type_constr_bool   = f "bool"
let type_constr_char   = f "char"
let type_constr_int    = f "int"
let type_constr_float  = f "float"
let type_constr_string = f "string"
let type_constr_option = f "option"
let type_constr_list   = f "list"
let type_constr_array  = f "array"

(* typ *)

let f desc = { typ_desc=Tconstr(desc, []); typ_level=notgeneric }
let type_unit   = f type_constr_unit
let type_bool   = f type_constr_bool
let type_char   = f type_constr_char
let type_int    = f type_constr_int
let type_float  = f type_constr_float
let type_string = f type_constr_string
let type_option = f type_constr_option

let f desc t = { typ_desc=Tconstr(desc, [t]); typ_level=notgeneric }
let type_list   = f type_constr_list
let type_array  = f type_constr_array

let type_arrow t1 t2 =
  { typ_desc=Tarrow(t1,t2); typ_level=notgeneric }

let type_product ts =
  { typ_desc=Tproduct ts; typ_level=notgeneric }

(* constr_desc global *)

let f id info =
  { qualid=Lident id; info=info }

let constr_void =
  f "()"
  { cs_res={ typ_desc=Tconstr(type_constr_unit, []); typ_level=notgeneric }
  ; cs_arg=type_unit
  ; cs_tag=1,0
  ; cs_kind=Constr_constant
  }

let constr_false =
  f "false"
  { cs_res={ typ_desc=Tconstr(type_constr_bool, []); typ_level=notgeneric }
  ; cs_arg=type_unit
  ; cs_tag=2,0
  ; cs_kind=Constr_constant
  }

let constr_true =
  f "true"
  { cs_res={ typ_desc=Tconstr(type_constr_bool, []); typ_level=notgeneric }
  ; cs_arg=type_unit
  ; cs_tag=2,1
  ; cs_kind=Constr_constant
  }

let constr_nil =
  let arg = { typ_desc=Tvar(ref Tnolink); typ_level=generic } in
  f "[]"
  { cs_res={ typ_desc=Tconstr(type_constr_list, [arg]); typ_level=notgeneric }
  ; cs_arg=type_unit
  ; cs_tag=2,0
  ; cs_kind=Constr_constant
  }

let constr_cons =
  let arg1 = { typ_desc=Tvar(ref Tnolink); typ_level=generic } in
  let arg2 = { typ_desc=Tconstr(type_constr_list, [arg1]); typ_level=generic } in
  f "::"
  { cs_res=arg2
  ; cs_arg={ typ_desc=Tproduct [arg1; arg2]; typ_level=generic }
  ; cs_tag=2,1
  ; cs_kind=Constr_superfluous 2
  }

let constr_none =
  let arg = { typ_desc=Tvar(ref Tnolink); typ_level=generic } in
  f "None"
  { cs_res={ typ_desc=Tconstr(type_constr_option, [arg]); typ_level=generic }
  ; cs_arg=type_unit
  ; cs_tag=2,0
  ; cs_kind=Constr_constant
  }

let constr_some =
  let arg = { typ_desc=Tvar(ref Tnolink); typ_level=generic } in
  f "Some"
  { cs_res={ typ_desc=Tconstr(type_constr_option, [arg]); typ_level=generic }
  ; cs_arg=arg
  ; cs_tag=2,1
  ; cs_kind=Constr_regular
  }

(* global type_desc  *)

let () =
  let f info =
    { qualid=info.ty_constr.qualid; info=info }
  in
  List.iter add_global_type
  [ f { ty_constr=type_constr_unit
      ; ty_arity=0
      ; ty_desc=Variant_type[constr_void]
      }
  ; f { ty_constr=type_constr_bool
      ; ty_arity=0
      ; ty_desc=Variant_type[constr_false; constr_true]
      }
  ; f { ty_constr=type_constr_char
      ; ty_arity=0
      ; ty_desc=Abstract_type
      }
  ; f { ty_constr=type_constr_int
      ; ty_arity=0
      ; ty_desc=Abstract_type
      }
  ; f { ty_constr=type_constr_float
      ; ty_arity=0
      ; ty_desc=Abstract_type
      }
  ; f { ty_constr=type_constr_string
      ; ty_arity=0
      ; ty_desc=Abstract_type
      }
  ; f { ty_constr=type_constr_option
      ; ty_arity=1
      ; ty_desc=Variant_type[constr_none; constr_some]
      }
  ; f { ty_constr=type_constr_list
      ; ty_arity=1
      ; ty_desc=Variant_type[constr_nil; constr_cons]
      }
  ; f { ty_constr=type_constr_array
      ; ty_arity=1
      ; ty_desc=Abstract_type
      }
  ]

let () =
  let intop (op,p) =
    add_global_value
    { qualid=Lident op
    ; info={ v_typ=type_arrow type_int (type_arrow type_int type_int)
          ; v_prim=Prim(2, p)
          }
    }
  in
  let floatop (op,p) =
    add_global_value
    { qualid=Lident op
    ; info={ v_typ=type_arrow type_float (type_arrow type_float type_float)
          ; v_prim=Prim(2, p)
          }
    }
  in
  List.iter intop
  [ "+", Paddint
  ; "-", Psubint
  ; "*", Pmulint
  ; "/", Pdivint
  ];
  List.iter floatop
  [ "+.", Pfloat Paddfloat
  ; "-.", Pfloat Psubfloat
  ; "*.", Pfloat Pmulfloat
  ; "/.", Pfloat Pdivfloat
  ];
