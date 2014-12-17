#include "value.h"
#include "prim.h"
#include "io.h"

value compare();
value equal();
value notequal();
value less();
value lessequal();
value greater();
value greaterequal();

value input_char();
value output_char();

c_primitive cprims[] = {
  compare,
  equal,
  notequal,
  less,
  lessequal,
  greater,
  greaterequal,

  input_char,
  output_char,
};
