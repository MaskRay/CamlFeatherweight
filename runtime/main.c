#include "common.h"
#include "instruct.h"
#include "value.h"
#include "prim.h"
#include "error.h"
#include "str.h"

#define DEBUG

#ifdef DEBUG
# undef DIRECT_JUMP
#endif

//#define DIRECT_JUMP
#define Arg_stack_size 16384
#define Ret_stack_size 16384
#define Trap_stack_size 16384

#define DIVISION_BY_ZERO_EXN 1 // ld.ml division_by_zero_tag

bool trace = false;
bool verbose = false;
value global_value;
hd_t first_atoms[256];
value *arg_stack_low, *arg_stack_high;
value *ret_stack_low, *ret_stack_high;
value tail = 0;
jmp_buf external_raise_buf;

static inline void modify(value *x, value y)
{
  *x = y;
}

value alloc_with_hd(u32 size, hd_t hd)
{
  value block = (value)malloc((size+2)*sizeof(value));
  *(value *)block = hd;
  Field(block, -1) = tail;
  if (tail)
    Field(tail, -1) ^= block;
  tail = block;
  return block;
}

value alloc(u8 tag, u32 size)
{
  return alloc_with_hd(size, Make_header(tag, size));
}

value alloc_block(value env, u32 nmore)
{
  value newenv = alloc(Tag_val(env), Wosize_val(env) + nmore);
  memcpy((char*)newenv + (2+nmore)*sizeof(value),
         (char*)env + 2*sizeof(value), Bosize_val(env));
  return (value)newenv;
}

void disasm(code_t pc)
{
  u8 op = *pc++;
  printf("%s ", name_of_instructions[op]);
  switch (op) {
  case ACCESS:
  case DUMMY:
  case ENDLET:
  case GETFIELD:
  case SETFIELD:
  case UPDATE:
    printf("%d", *pc++);
    break;
  case CCALL1:
  case CCALL2:
  case CCALL3:
  case CCALL4:
    printf("%s", name_of_cprims[*pc++]);
    break;
  case CONSTINT8:
    printf("%d", *(i8*)pc++);
    break;
  case GETGLOBAL:
  case SETGLOBAL:
    if ((value)pc & 1) pc++;
    printf("[%d]", pu16(pc));
    pc += 2;
    break;
  case CONSTINT16:
    if ((value)pc & 1) pc++;
    printf("[%d]", pi16(pc));
    pc += 2;
    break;
  case BRANCH:
  case BRANCHIF:
  case BRANCHIFEQ:
  case BRANCHIFGE:
  case BRANCHIFGT:
  case BRANCHIFLE:
  case BRANCHIFLT:
  case CUR:
    if ((value)pc & 1) pc++;
    printf("0x%08x", pc+pi16(pc));
    pc += 2;
    break;
  case BRANCHIFNEQTAG:
    printf("%d ", *pc++);
    if ((value)pc & 1) pc++;
    printf("0x%08x", pc+pi16(pc));
    pc += 2;
    break;
  }
  putchar('\n');
}

bool touch(value x)
{
  u32 size, color;
  switch (Tag_val(x)) {
  case Closure_tag:
    if (Color_val(x))
      return true;
    Hd_val(x) = Set_color_val(x, 1);
    return false;
  case String_tag:
    Hd_val(x) |= 1 << Gcsize_offset;
    return true;
  case Array_tag:
    size = array_length(x);
    color = Field(x, 0);
    if (color < size) {
      Field(x, 0)++;
      return false;
    }
    if (! size)
      Field(x, 0) = 1;
    return true;
  default:
    size = Wosize_val(x);
    color = Color_val(x);
    if (color < size) {
      Hd_val(x) = Set_color_val(x, color+1);
      return false;
    }
    if (! size)
      Hd_val(x) = Set_color_val(x, 1);
    return true;
  }
}

u32 get_cur_field(value x)
{
  switch (Tag_val(x)) {
  case Closure_tag:
    return Env_val(x);
  case Array_tag:
    return Field(x, Field(x, 0));
  default:
    return Field(x, Color_val(x)-1);
  }
}

void set_cur_field(value x, value y)
{
  switch (Tag_val(x)) {
  case Closure_tag:
    Env_val(x) = y;
    break;
  case Array_tag:
    Field(x, Field(x, 0)) = y;
    break;
  default:
    Field(x, Color_val(x)-1) = y;
    break;
  }
}

bool is_fresh(value x)
{
  switch (Tag_val(x)) {
  case String_tag:
    return String_color_val(x) == 0;
  case Array_tag:
    return Field(x, 0) == 0;
  default:
    return Color_val(x) == 0;
  }
}

void reset_color(value x)
{
  switch (Tag_val(x)) {
  case String_tag:
    Hd_val(x) &= ~ (1 << Gcsize_offset);
    break;
  case Array_tag:
    Field(x, 0) = 0;
    break;
  default:
    Hd_val(x) = Set_color_val(x, 0);
    break;
  }
}

void schorr_waite(value x)
{
  if (! Is_block(x) || ! x || ! is_fresh(x)) return;
  value p = 0, y;
  for(;;) {
    if (touch(x)) {
      if (! p) return;
      y = x;
      x = p;
      p = get_cur_field(x);
      set_cur_field(x, y);
    } else {
      y = get_cur_field(x);
      if (Is_block(y) && y && is_fresh(y)) {
        set_cur_field(x, p);
        p = x;
        x = y;
      }
    }
  }
}

void gc(value acc, value env, value *asp, value *rsp, struct trap_frame *tp)
{
  return;
  for (value *p = asp; p < arg_stack_high; p++)
    schorr_waite(*p);
  for (value *p = rsp; p < ret_stack_high; ) {
    if (p == (value*)tp) {
      schorr_waite(tp->env);
      tp = tp->tp;
      p = (value*)((char*)p-sizeof(struct trap_frame));
    } else {
      schorr_waite(((struct return_frame*)p)->env);
      p++;
    }
  }
  schorr_waite(global_value);
  schorr_waite(acc);
  schorr_waite(env);
  value x = 0, y = tail, z;
  while (y) {
    z = Field(y, -1) ^ x;
    if (is_fresh(y)) {
      if (x)
        Field(x, -1) ^= y ^ z;
      else
        tail = z;
      if (z)
        Field(z, -1) ^= y ^ x;
      // printf("+ free %08x\n", y);
      free((void*)y);
      y = z;
    } else {
      reset_color(y);
      x = y;
      y = z;
    }
  }
}

value interpret(code_t code)
{
  value acc = Val_int(0), env = Atom(0),
        *asp = arg_stack_high, *rsp = ret_stack_high;
  struct trap_frame *tp = NULL;
  code_t pc = code;
  value tmp;

#define retsp ((struct return_frame *)rsp)
#define Push_ret_frame ( rsp = (value*)((char*)rsp-sizeof(struct return_frame)) )
#define Pop_ret_frame ( rsp = (value*)((char*)rsp+sizeof(struct return_frame)) )
#define trapsp ((struct trap_frame *)rsp)
#define Push_trap_frame ( rsp = (value*)((char*)rsp-sizeof(struct trap_frame)) )
#define Pop_trap_frame ( rsp = (value*)((char*)rsp+sizeof(struct trap_frame)) )
//#define Push_ret_frame rsp--
//#define Pop_ret_frame rsp++
//#define Push_trap_frame tsp--
//#define Pop_trap_frame tsp++

#ifdef DEBUG
  uvalue tick = 0;
#endif

#ifdef DIRECT_JUMP
# define Inst(name) lbl_##name
# define Next goto *jumptable[*pc++];
  static void *jumptable[] = {
    #include "jumptable.h"
  };
  Next;
#else
# define Inst(name) case name
# define Next break
  for(;;) {
# ifdef DEBUG
    if (trace) {
      tick++;
      disasm(pc);
      if (tick % (1 << 22) == 0)
        gc(acc, env, asp, rsp, tp);
    }
# endif
    switch (*pc++) {
#endif
    Inst(ACCESS):
      acc = Field(env, *pc++);
      Next;
    Inst(ADDFLOAT):
      tmp = alloc(Double_tag, Double_wosize);
      *(double*)Op_val(tmp) = Double_val(acc) + Double_val(*asp++);
      acc = tmp;
      Next;
    Inst(ADDINT):
      acc += *asp++ - 1;
      Next;
    Inst(ANDINT):
      acc &= *asp++;
      Next;
    Inst(APPLY):
      Push_ret_frame;
      retsp->pc = pc;
      retsp->env = env;
      pc = Code_val(acc);
      env = alloc_block(Env_val(acc), 1);
      Field(env, 0) = *asp++;
      // TODO check stack
      Next;
    Inst(ARRAYLENGTH):
      acc = Val_int(array_length(acc));
      Next;
    Inst(ASRINT):
      acc = 1 | acc-1 >> Int_val(*asp++);
      Next;
    Inst(ATOM):
      acc = Atom(*pc++);
      Next;
    Inst(BRANCH):
      Br16;
      Next;
    Inst(BRANCHIF):
      Br16if(Tag_val(acc));
      Next;
    Inst(BRANCHIFEQ):
      Br16if(acc == *asp++);
      Next;
    Inst(BRANCHIFGE):
      Br16if(acc >= *asp++);
      Next;
    Inst(BRANCHIFGT):
      Br16if(acc > *asp++);
      Next;
    Inst(BRANCHIFLE):
      Br16if(acc <= *asp++);
      Next;
    Inst(BRANCHIFLT):
      Br16if(acc < *asp++);
      Next;
    Inst(BRANCHIFNEQ):
      Br16if(acc != *asp++);
      Next;
    Inst(BRANCHIFNEQTAG): {
      u8 n = *pc++;
      Br16if(Tag_val(acc) != n);
      Next;
    }
    Inst(BRANCHIFNOT):
      Br16if(! Tag_val(acc));
      Next;
    Inst(CCALL1):
      acc = cprims[pu8(pc)](acc);
      pc += sizeof(u8);
      Next;
    Inst(CCALL2):
      acc = cprims[pu8(pc)](acc, asp[0]);
      pc += sizeof(u8);
      asp += 1;
      Next;
    Inst(CCALL3):
      acc = cprims[pu8(pc)](acc, asp[0], asp[1]);
      pc += sizeof(u8);
      asp += 2;
      Next;
    Inst(CCALL4):
      acc = cprims[pu8(pc)](acc, asp[0], asp[1], asp[2]);
      pc += sizeof(u8);
      asp += 3;
      Next;
    Inst(CONSTINT8):
      acc = pi8(pc)*2+1;
      pc += sizeof(i8);
      Next;
    Inst(CONSTINT16):
      if ((value)pc & 1) pc++;
      acc = pi16(pc)*2+1;
      pc += sizeof(i16);
      Next;
    Inst(CUR):
      acc = alloc(Closure_tag, Closure_wosize);
      if ((value)pc & 1) pc++;
      Code_val(acc) = pc+pi16(pc);
      Env_val(acc) = env;
      pc += sizeof(i16);
      Next;
    Inst(DECR):
      Field(acc, 0) -= 2;
      acc = Atom(0);
      Next;
    Inst(DIVFLOAT):
      tmp = alloc(Double_tag, Double_wosize);
      *(double*)Op_val(tmp) = Double_val(acc) / Double_val(*asp++);
      acc = tmp;
      Next;
    Inst(DIVINT):
      tmp = *asp++ - 1;
      if (! tmp) {
        acc = Atom(DIVISION_BY_ZERO_EXN);
        goto raise;
      }
      acc = Val_int((acc-1)/tmp);
      Next;
    Inst(DUMMY): {
      u8 n = *pc++;
      env = alloc_block(env, n);
      while (n--)
        Field(env, n) = Val_int(0);
      Next;
    }
    Inst(ENDLET): {
      u8 n = *pc++;
      uint32_t size = Wosize_val(env)-n;
      value newenv = alloc(0, size);
      REP(i, size)
        Field(newenv, i) = Field(env, i+n);
      env = newenv;
      Next;
    }
    Inst(EQ):
      acc = Atom(acc == *asp++);
      Next;
    Inst(EQSTRING):
      acc = Atom(string_compare(acc, *asp++) == 0);
      Next;
    Inst(EQFLOAT):
      acc = Atom(Double_val(acc) == Double_val(*asp++));
      Next;
    Inst(GEFLOAT):
      acc = Atom(Double_val(acc) >= Double_val(*asp++));
      Next;
    Inst(GEINT):
      acc = Atom(acc >= *asp++);
      Next;
    Inst(GESTRING):
      acc = Atom(string_compare(acc, *asp++) >= 0);
      Next;
    Inst(GETARRAYITEM):
      acc = array_getitem(acc, Int_val(*asp++));
      Next;
    Inst(GETFIELD):
      acc = Field(acc, *pc++);
      Next;
    Inst(GETGLOBAL):
      if ((value)pc & 1) pc++;
      acc = Field(global_value, pu16(pc));
      pc += sizeof(u16);
      Next;
    Inst(GETSTRINGITEM):
      acc = Val_int(string_getitem(acc, Int_val(*asp++)));
      Next;
    Inst(GRAB):
      if (*asp == MARK) {
        asp++;
        pc = retsp->pc;
        env = retsp->env;
        Pop_ret_frame;
      } else {
        env = alloc_block(env, 1);
        Field(env, 0) = *asp++;
      }
      Next;
    Inst(GTFLOAT):
      acc = Atom(Double_val(acc) > Double_val(*asp++));
      Next;
    Inst(GTINT):
      acc = Atom(acc > *asp++);
      Next;
    Inst(GTSTRING):
      acc = Atom(string_compare(acc, *asp++) > 0);
      Next;
    Inst(INCR):
      Field(acc, 0) += 2;
      acc = Atom(0);
      Next;
    Inst(INTOFFLOAT):
      acc = Val_int((value)Double_val(acc));
      Next;
    Inst(LEFLOAT):
      acc = Atom(Double_val(acc) <= Double_val(*asp++));
      Next;
    Inst(LEINT):
      acc = Atom(acc <= *asp++);
      Next;
    Inst(LESTRING):
      acc = Atom(string_compare(acc, *asp++) <= 0);
      Next;
    Inst(LET):
      env = alloc_block(env, 1);
      Field(env, 0) = acc;
      Next;
    Inst(LSLINT):
      acc = 1 | acc-1 << Int_val(*asp++);
      Next;
    Inst(LSRINT):
      acc = 1 | (uvalue)(acc-1) >> Int_val(*asp++);
      Next;
    Inst(LTFLOAT):
      acc = Atom(Double_val(acc) < Double_val(*asp++));
      Next;
    Inst(LTINT):
      acc = Atom(acc < *asp++);
      Next;
    Inst(LTSTRING):
      acc = Atom(string_compare(acc, *asp++) < 0);
      Next;
    Inst(MAKEARRAY): {
      u8 init = *pc++;
      u32 size = Int_val(acc);
      value block = alloc_with_hd(size+1, Array_make_header(size+1));
      Field(block, 0) = 0; // actual color used by GC
      if (init)
        for (uint32_t i = 1; i <= size; i++)
          Field(block, i) = *asp++;
      else {
        for (uint32_t i = 1; i <= size; i++)
          Field(block, i) = *asp;
        asp++;
      }
      acc = block;
      Next;
    }
    Inst(MAKEBLOCK): {
      pc = (code_t)(((value)pc+sizeof(value)-1) & -sizeof(value));
      value hdr = *(value*)pc;
      pc += sizeof(value);
      uint32_t size = Wosize_hd(hdr);
      value block = alloc(Tag_hd(hdr), size);
      Field(block, 0) = acc;
      for (uint32_t i = 1; i < size; i++)
        Field(block, i) = *asp++;
      acc = block;
      Next;
    }
    Inst(MAKESTRING): {
      u32 len = Int_val(acc);
      u32 w = WORD_SIZE/8;
      u32 size = len/w+1;
      value block = alloc_with_hd(size, String_make_header(size));
      memset(&Field(block, 0), Int_val(*asp++), len);
      u8 *last = (u8*)&Field(block, size-1);
      u8 pad = w-len%w;
      for (u32 i = len%w; i < w; i++)
        last[i] = pad;
      acc = block;
      Next;
    }
    Inst(MODINT):
      tmp = *asp++ - 1;
      if (! tmp) {
        acc = Atom(DIVISION_BY_ZERO_EXN);
        goto raise;
      }
      acc = (acc-1) % tmp + 1;
      Next;
    Inst(MULFLOAT):
      tmp = alloc(Double_tag, Double_wosize);
      *(double*)Op_val(tmp) = Double_val(acc) * Double_val(*asp++);
      acc = tmp;
      Next;
    Inst(MULINT):
      acc = (acc>>1) * (*asp++-1) + 1;
      Next;
    Inst(NEGFLOAT):
      tmp = alloc(Double_tag, Double_wosize);
      *(double*)Op_val(tmp) = - Double_val(acc);
      acc = tmp;
      Next;
    Inst(NEGINT):
      acc = 2-acc;
      Next;
    Inst(NEQ):
      acc = Atom(acc != *asp++);
      Next;
    Inst(NEQFLOAT):
      acc = Atom(Double_val(acc) != Double_val(*asp++));
      Next;
    Inst(NEQSTRING):
      acc = Atom(string_compare(acc, *asp++) == 0);
      Next;
    Inst(NOP):
      Next;
    Inst(NOT):
      acc = Atom(Tag_val(acc) == 0);
      Next;
    Inst(ORINT):
      acc |= *asp++;
      Next;
    Inst(POP):
      acc = *asp++;
      Next;
    Inst(POPBRANCHIFNOT):
      tmp = acc;
      acc = *asp++;
      Br16if(Tag_val(tmp) == 0);
      Next;
    Inst(POPTRAP):
      rsp = (value *)tp;
      env = trapsp->env;
      asp = trapsp->asp;
      tp = trapsp->tp;
      Pop_trap_frame;
      Next;
    Inst(PUSH):
      *--asp = acc;
      Next;
    Inst(PUSHMARK):
      *--asp = MARK;
      Next;
    Inst(PUSHTRAP):
      Push_trap_frame;
      if ((value)pc & 1) pc++;
      trapsp->pc = pc+pi16(pc);
      pc += sizeof(i16);
      trapsp->env = env;
      trapsp->asp = asp;
      trapsp->tp = tp;
      tp = trapsp;
      Next;
    Inst(RAISE):
raise:
      if (! tp)
        longjmp(external_raise_buf, 1);
      rsp = (value *)tp;
      pc = trapsp->pc;
      env = alloc_block(trapsp->env, 1);
      Field(env, 0) = acc;
      asp = trapsp->asp;
      tp = trapsp->tp;
      Pop_trap_frame;
      Next;
    Inst(RETURN):
      if (*asp == MARK) {
        asp++;
        pc = retsp->pc;
        env = retsp->env;
        Pop_ret_frame;
        Next;
      }
      // more arguments are given
      goto termapply;
    Inst(SARINT): {
      acc = 1 | (acc-1) >> Int_val(*asp++);
      Next;
    }
    Inst(SETARRAYITEM):
      array_setitem(acc, Int_val(asp[0]), asp[1]);
      asp += 2;
      acc = Atom(0);
      Next;
    Inst(SETFIELD): {
      value *ptr = &Field(acc, *pc++);
      modify(ptr, *pc++);
      Next;
    }
    Inst(SETGLOBAL):
      if ((value)pc & 1) pc++;
      modify(&Field(global_value, pu16(pc)), acc);
      pc += sizeof(u16);
      Next;
    Inst(SETSTRINGITEM):
      string_setitem(acc, Int_val(asp[0]), Int_val(asp[1]));
      acc = Atom(0);
      asp += 2;
      Next;
    Inst(SHLINT):
      acc = 1 | (acc-1) << Int_val(*asp++);
      Next;
    Inst(SHRINT):
      acc = 1 | (uvalue)(acc-1u) >> Int_val(*asp++);
      Next;
    Inst(STOP):
      return acc;
    Inst(STRINGLENGTH):
      acc = Val_int(string_length(acc));
      Next;
    Inst(SUBFLOAT):
      tmp = alloc(Double_tag, Double_wosize);
      *(double*)Op_val(tmp) = Double_val(acc) - Double_val(*asp++);
      acc = tmp;
      Next;
    Inst(SUBINT): {
      acc -= *asp++ - 1;
      Next;
    }
    Inst(SWITCH):
      pc++; // size of jumptable
      if ((value)pc & 1) pc++;
      pc += pi16(pc+Tag_val(acc)*2);
      Next;
    Inst(TAGOF):
      acc = Val_int(Tag_val(acc));
      Next;
    Inst(TERMAPPLY):
termapply:
      pc = Code_val(acc);
      env = alloc_block(Env_val(acc), 1);
      Field(env, 0) = *asp++;
      Next;
    Inst(UPDATE): {
      u8 n = *pc++;
      modify(&Field(env, n), acc);
      Next;
    }
    Inst(XORINT):
      acc = 1 + (acc ^ *asp++);
      Next;
#ifdef DIRECT_JUMP
#else
    }
  }
#endif
}

static void init_atoms(void)
{
  REP(i, 256)
    first_atoms[i] = Make_header(i, 0);
}

static void init_stacks(void)
{
  arg_stack_low = malloc(Arg_stack_size);
  arg_stack_high = arg_stack_low + Arg_stack_size/sizeof(value);
  ret_stack_low = malloc(Arg_stack_size);
  ret_stack_high = ret_stack_low + Ret_stack_size/sizeof(struct return_frame);
}

#define FAILED_TO_OPEN -1
#define BAD_MAGIC -2
#define TRUNCATED_FILE -3
#define INVALID_EXE -4
#define SYSERROR -5

int run(const char *filename)
{
  char buf[4];
  value val;
  int fd = open(filename, O_RDONLY);
  int t;
  if (fd < 0)
    return FAILED_TO_OPEN;
  if (read(fd, buf, 4) != 4 || ! (buf[0]==MAGIC[0] && buf[1]==MAGIC[1] && buf[2]==MAGIC[2] && buf[3]==MAGIC[3]))
    return BAD_MAGIC;
  u32 global_value_off, global_value_num, size;
  if (read(fd, &global_value_off, 4) != 4 || read(fd, &global_value_num, 4) != 4)
    return TRUNCATED_FILE;
  if (lseek(fd, global_value_off, SEEK_SET) < 0)
    return TRUNCATED_FILE;
  global_value = alloc(0, global_value_num);
  REP(i, global_value_num) {
    if (read(fd, buf, 1) != 1)
      return TRUNCATED_FILE;
    if (buf[0] == 1) {
      read(fd, &val, sizeof(value));
      if (val % 2 == 0)
        return INVALID_EXE;
      Field(global_value, i) = val;
    } else if (buf[0] == 0) {
      if (read(fd, &val, sizeof(value)) != sizeof(value))
        return TRUNCATED_FILE;
      value block;
      switch (Tag_hd(val)) {
      case String_tag:
        size = String_wosize_hd(val);
        block = alloc_with_hd(size, String_make_header(size));
        break;
      default:
        size = Wosize_hd(val);
        block = alloc(Tag_hd(val), size);
        break;
      }
      REP(j, size) {
        if (read(fd, &val, sizeof(value)) != sizeof(value))
          return TRUNCATED_FILE;
        Field(block, j) = val;
      }
      Field(global_value, i) = block;
    } else
      return INVALID_EXE;
  }

  if (lseek(fd, 4*3, SEEK_SET) < 0)
    return SYSERROR;
  u32 code_len = global_value_off-4*3;
  code_t code = malloc(code_len);
  if (! code)
    return SYSERROR;
  if (read(fd, code, code_len) != code_len)
    return TRUNCATED_FILE;
  value r = interpret(code);
  if (trace)
    fprintf(stderr, "+ acc=%d\n", r);
  return 0;
}

static void print_help(const char *argv0)
{
  fprintf(stderr, "Usage: %s\n", argv0);
}

int main(int argc, char *argv[])
{
#ifdef JS
  int optind = 1;
#else
  int opt;
  while ((opt = getopt(argc, argv, "htv")) != -1) {
    switch (opt) {
    case 'h':
      print_help(argv[0]);
      return 0;
    case 't':
      trace = true;
      break;
    case 'v':
      verbose = true;
      break;
    case '?':
      print_help(argv[0]);
      return 1;
    }
  }
  if (optind == argc)
    fatal_error("No bytecode file specified.");
#endif

  init_atoms();
  init_stacks();
  if (! setjmp(external_raise_buf)) {
    int r = run(argv[optind]);
    if (r < 0)
      switch (r) {
      case FAILED_TO_OPEN:
        fatal_error_fmt("Failed to open \"%s\"\n", strerror(errno));
        break;
      case TRUNCATED_FILE:
        fatal_error_fmt("\"%s\" seems to be truncated\n", argv[optind]);
        break;
      case INVALID_EXE:
        fatal_error_fmt("\"%s\" is not a bytecode executable file\n", argv[optind]);
        break;
      case BAD_MAGIC:
        fatal_error_fmt("\"%s\" is not a bytecode executable file: missing magic \"%s\"\n", argv[optind], MAGIC);
        break;
      case SYSERROR:
        fatal_error(strerror(errno));
        break;
      }
  } else
    fatal_error("Fatal error: uncaught exception");
  return 0;
}
