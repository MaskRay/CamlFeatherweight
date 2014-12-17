#include "error.h"
#include "value.h"

ML value input_char(value _)
{
  return Val_int(getchar());
}

ML value output_char(value ch)
{
  putchar(Int_val(ch));
  return Val_unit;
}
