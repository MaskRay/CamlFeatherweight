#pragma once

#include "common.h"

#define REP(i, n) for (__typeof(n) i = 0; i < n; i++)

typedef intptr_t value;
typedef uintptr_t uvalue;
typedef uvalue hd_t;
typedef unsigned char u8;
typedef unsigned char *code_t;

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

#define pi8(p) (*(int8_t*)(p))
#define pu8(p) (*(uint8_t*)(p))
#define pi16(p) (*(int16_t*)(p))
#define pu16(p) (*(uint16_t*)(p))

#define Br16 do {if ((value)pc & 1) pc++; pc += pi16(pc);} while (0)
#define Br16if(cond) do {if ((value)pc & 1) pc++; if (cond) pc += pi16(pc); else pc += sizeof(i16);} while (0)

// value

#define Is_int(x) ((x) & 1)
#define Is_block(x) (! ((x) & 1))

/* Structure of the header:

For 32-bit architectures:

bits  31  20 19    8 7   0
     +------+-------+-----+
     | size | color | tag |
     +------+-------+-----+
      31                 0
     +--------------------+
     | xor of prev & next |
     +--------------------+

For 64-bit architectures:

bits  63  36 35    8 7   0
     +------+-------+-----+
     | size | color | tag |
     +------+-------+-----+
      63                 0
     +--------------------+
     | xor of prev & next |
     +--------------------+

*/

#define Gcsize_offset 8
#if WORD_SIZE == 32
# define Size_offset 20
#else
# define Size_offset 36
#endif
#define Make_header(tag, size) (tag | (hd_t)(size) << Size_offset)
#define Wosize_hd(x) (x >> Size_offset)
#define Wosize_val(v) Wosize_hd(Hd_val(v))
#define Bosize_hd(x) ((x >> Size_offset) * sizeof(value))
#define Bosize_val(v) Bosize_hd(Hd_val(v))
#define Bosize_block(v) (Bosize_val(v) + 2 * sizeof(value))
#define Hd_val(x) (*(hd_t*)x)
#define Op_val(x) ((value*)(x)+2)
#define Field(x, i) (((value*)(x))[i+2])
#define Color_hd(hd) ((hd) >> Gcsize_offset & ((hd_t)1 << Size_offset-Gcsize_offset) - 1)
#define Color_val(v) Color_hd(Hd_val(v))
#define Set_color_val(v,col) (Hd_val(v) & ~(((hd_t)1<<Size_offset)-((hd_t)1<<Gcsize_offset)) | (hd_t)(col)<<Gcsize_offset)

// tag

#define Num_tags (1 << 8)
#define No_scan_tag (Num_tags-5)
#define Tag_hd(hd) ((u8)((hd) & 0xFF))
#define Tag_val(v) (*(u8*)(v))

// 0: int
#define Val_int(x) (((value)(x) << 1) + 1)
#define Int_val(x) ((x) >> 1)

// 1: tag < No_scan_tag: fields

// 1-0: atom: 0-tuples
extern hd_t first_atoms[];
#define Atom(tag) ((value)&first_atoms[tag])
#define Val_unit Atom(0)
#define Val_false Atom(0)
#define Val_true Atom(1)
#define Bool_val(x) Atom((x) != 0)

// 1-1: closure
#define Closure_tag (No_scan_tag-1)
#define Closure_wosize 2
#define Code_val(v) (*(u8**)&Field(v, 0))
#define Env_val(v) Field(v, 1)

// 2: tag >= No_scan_tag: bytes

// 2-0: Abstract
#define Abstract_tag No_scan_tag

// 2-1: string
#define String_tag (No_scan_tag+1)
#define String_wosize_hd(hd) ((hd) >> Gcsize_offset+1)
#define String_wosize_val(v) String_wosize_hd(Hd_val(v))
#define String_make_header(size) (String_tag | (value)(size) << Gcsize_offset+1)
#define String_color_hd(hd) ((hd) >> Gcsize_offset & 1)
#define String_color_val(v) String_color_hd(Hd_val(v))

// 2-2: array
#define Array_tag (No_scan_tag+2)
#define Array_make_header(size) (Array_tag | (value)(size) << Gcsize_offset)

static inline u32 array_length(value s)
{ return Hd_val(s) >> Gcsize_offset; }

static inline value array_getitem(value s, intptr_t i)
{ return Field(s, i+1); }

static inline void array_setitem(value s, intptr_t i, value x)
{ Field(s, i+1) = x; }

// 2-3: float (represented by double)
#define Double_tag (No_scan_tag+3)
#define Double_val(v) (*(double*)((value*)(v)+2))
#define Double_wosize 2

/*
 * stack
 */

#define MARK ((value)0)

struct return_frame {
  code_t pc;
  value env;
};

struct trap_frame {
  code_t pc;
  value env, *asp;
  struct trap_frame *tp;
};

/* empty */
#define ML
