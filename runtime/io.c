#include "error.h"
#include "value.h"
#include "str.h"

ML value input_char(value _)
{
  return Val_int(getchar());
}

ML value output_char(value ch)
{
  putchar(Int_val(ch));
  return Val_unit;
}

ML value output_int(value ch)
{
  printf("%d", Int_val(ch));
  return Val_unit;
}

ML value output_float(value x)
{
  printf("%g", Double_val(x));
  return Val_unit;
}

ML value output_string(value x)
{
  u32 len = string_length(x);
  REP(i, len)
    putchar(string_getitem(x, i));
  return Val_unit;
}
