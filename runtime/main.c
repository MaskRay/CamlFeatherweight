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

bool trace = false;
bool verbose = false;
value global_value;
hd_t first_atoms[256];
value *arg_stack_low, *arg_stack_high;
value *ret_stack_low, *ret_stack_high;
value *tail;

static inline void modify(value *x, value y)
{
  *x = y;
}

value alloc_with_hd(u32 size, hd_t hd)
{
  value block = (value)malloc((size+2)*sizeof(value));
  *(value *)block = hd;
  ((value *)block)[1] = (value)tail;
  if (tail)
    tail[1] ^= block;
  tail = (value *)block;
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
    printf("[%d]", pu16(pc));
    pc += 2;
    break;
  case CONSTINT16:
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
    printf("0x%08x", pc+pi16(pc));
    pc += 2;
    break;
  }
  putchar('\n');
}

value interpret(code_t code)
{
  value acc = Val_int(0), env = Atom(0),
        *asp = arg_stack_high,
        *rsp = ret_stack_high;
  code_t pc = code;
  value tmp;

#define retsp ((struct return_frame *)rsp)
#define Push_ret_frame ( rsp = (value*)((char*)rsp-sizeof(struct return_frame)) )
#define Pop_ret_frame ( rsp = (value*)((char*)rsp+sizeof(struct return_frame)) )

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
    if (trace)
      disasm(pc);
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
    Inst(BRANCH):
      pc += pi16(pc);
      Next;
    Inst(BRANCHIF):
      if (Tag_val(acc))
        pc += pi16(pc);
      else
        pc += sizeof(i16);
      Next;
    Inst(BRANCHIFEQ):
      if (acc == *asp++)
        pc += pi16(pc);
      else
        pc += sizeof(i16);
      Next;
    Inst(BRANCHIFGE):
      if (acc >= *asp++)
        pc += pi16(pc);
      else
        pc += sizeof(i16);
      Next;
    Inst(BRANCHIFGT):
      if (acc > *asp++)
        pc += pi16(pc);
      else
        pc += sizeof(i16);
      Next;
    Inst(BRANCHIFLE):
      if (acc <= *asp++)
        pc += pi16(pc);
      else
        pc += sizeof(i16);
      Next;
    Inst(BRANCHIFLT):
      if (acc < *asp++)
        pc += pi16(pc);
      else
        pc += sizeof(i16);
      Next;
    Inst(BRANCHIFNEQ):
      if (acc != *asp++)
        pc += pi16(pc);
      else
        pc += sizeof(i16);
      Next;
    Inst(BRANCHIFNOT):
      if (! Tag_val(acc))
        pc += pi16(pc);
      else
        pc += sizeof(i16);
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
      acc = pi16(pc)*2+1;
      pc += sizeof(i16);
      Next;
    Inst(CUR):
      acc = alloc(Closure_tag, Closure_wosize);
      Code_val(acc) = pc+pi16(pc);
      Env_val(acc) = env;
      pc += sizeof(i16);
      Next;
    Inst(DIVFLOAT):
      tmp = alloc(Double_tag, Double_wosize);
      *(double*)Op_val(tmp) = Double_val(acc) / Double_val(*asp++);
      acc = tmp;
      Next;
    Inst(DIVINT):
      // TODO exn
      acc = Val_int((acc-1)/(*asp++-1));
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
        Field(newenv, i+n) = Field(env, i);
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
    Inst(MAKESTRING):
      // TODO
    Inst(MODINT):
      acc = (acc-1) % (*asp++-1) + 1;
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
    Inst(ORINT):
      acc |= *asp++;
      Next;
    Inst(POP):
      acc = *asp++;
      Next;
    Inst(POPBRANCHIFNOT):
      tmp = acc;
      acc = *asp++;
      if (Tag_val(tmp) == 0)
        pc += pi16(pc);
      else
        pc += sizeof(i16);
      Next;
    Inst(PUSH):
      *--asp = acc;
      Next;
    Inst(PUSHMARK):
      *--asp = MARK;
      Next;
    Inst(RAISE):
      not_implemented("RAISE");
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
      pc++;
      pc += pi16(pc+Tag_val(acc)*2);
      Next;
    Inst(TAGOF):
      acc = Val_int(Tag_val(acc));
      Next;
    Inst(TERMAPPLY):
termapply: {
      pc = Code_val(acc);
      env = alloc_block(Env_val(acc), 1);
      Field(env, 0) = *asp++;
      Next;
    }
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
  ret_stack_high = ret_stack_low + Ret_stack_size/sizeof(value);
}

static void init_global_value(void)
{
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
  if (read(fd, buf, 4) != 4 || memcmp(buf, MAGIC, 4))
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
      read(fd, &val, 4);
      if (val % 2 == 0)
        return INVALID_EXE;
      Field(global_value, i) = val;
    } else if (buf[0] == 0) {
      if (read(fd, &val, 4) != 4)
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
        if (read(fd, &val, 4) != 4)
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
  fprintf(stderr, "Usage: %s\n", basename(argv0));
}

int main(int argc, char *argv[])
{
  for(;;) {
    static struct option long_options[] = {
      {"trace",   no_argument, 0, 't'},
      {"verbose", no_argument, 0, 'v'},
      {0,         0,           0,  0 },
    };
    int opt, c = getopt_long(argc, argv, "htv", long_options, &opt);
    if (c == -1) break;
    switch (c) {
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

  init_atoms();
  init_stacks();
  init_global_value();
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
  return 0;
}
