#include "error.h"
#include "value.h"
#include "string.h"

// comparison on nonrecursive structures
intptr_t cmp_value(value v1, value v2)
{
  if (v1 == v2) return 0;
  if (Is_int(v1) || Is_int(v2)) return Int_val(v1)-Int_val(v2);
  u8 t1 = Tag_val(v1), t2 = Tag_val(v2);
  if (t1 != t2) return (intptr_t)t1-t2;
  switch (t1) {
  case String_tag:
    return string_compare(v1, v2);
  case Double_tag: {
    double d1 = Double_val(v1),
           d2 = Double_val(v2);
    return d1 < d2 ? -1 : d1 > d2 ? 1 : 0;
  }
  case Abstract_tag:
    invalid_argument("equal: abstract value");
    break;
  default: {
    uint32_t sz1 = Wosize_val(v1),
             sz2 = Wosize_val(v2);
    if (sz1 != sz2) return sz1-sz2;
    REP(i, sz1) {
      intptr_t r = cmp_value(Field(v1, i), Field(v2, i));
      if (r) return r;
    }
    return 0;
  }
  }
}

ML value compare(value v1, value v2)
{ return Val_int(cmp_value(v1, v2)); }

ML value equal(value v1, value v2)
{ return Atom(cmp_value(v1, v2) == 0); }

ML value notequal(value v1, value v2)
{ return Atom(cmp_value(v1, v2) != 0); }

ML value less(value v1, value v2)
{ return Atom(cmp_value(v1, v2) < 0); }

ML value lessequal(value v1, value v2)
{ return Atom(cmp_value(v1, v2) <= 0); }

ML value greater(value v1, value v2)
{ return Atom(cmp_value(v1, v2) > 0); }

ML value greaterequal(value v1, value v2)
{ return Atom(cmp_value(v1, v2) >= 0); }
