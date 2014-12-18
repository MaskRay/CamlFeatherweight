#include "instruct.h"
#include "value.h"
#include "prim.h"

//#define DIRECT_JUMP

extern value global_data;
uvalue first_atoms[256];
value *tail;

static inline void modify(value *x, value y)
{
  *x = y;
}

#ifdef DIRECT_JUMP
# define Inst(name) lbl_##name
# define Next goto *jumptable[*pc++];
#else
# define Inst(name) case name
# define Next break
#endif

value alloc(u8 tag, uint32_t size)
{
  value block = (value)malloc((size+2)*sizeof(value));
  *(value *)block = Make_header(tag, size);
  ((value *)block)[1] = (value)tail;
  if (tail)
    tail[1] ^= block;
  return block;
}

value alloc_block(value env, u32 nmore)
{
  value newenv = alloc(Tag_val(env), Wosize_val(env) + nmore);
  memcpy((char*)newenv + (2+nmore)*sizeof(value),
         (char*)env + 2*sizeof(value), Bosize_block(env));
  return (value)newenv;
}

value interpret(code_t pc)
{
  value acc, env, *asp, *rsp;
  value tmp;

#define retsp ((struct return_frame *)rsp)
#define Push_ret_frame ( rsp = (value*)((char*)rsp-sizeof(struct return_frame)) )
#define Pop_ret_frame ( rsp = (value*)((char*)rsp+sizeof(struct return_frame)) )

#ifdef DIRECT_JUMP
  static void *jumptable[] = {
    #include "jumptable.h"
  };
  Next;
#else
  for(;;) {
    switch (*pc++) {
#endif
    Inst(ACCESS): {
      u8 i = *pc++;
      acc = Field(env, i);
    }
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
      Field(env, 0) = *--asp;
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
      acc = cprims[pu16(pc)](acc);
      pc += sizeof(u16);
      Next;
    Inst(CCALL2):
      acc = cprims[pu16(pc)](acc, asp[0]);
      pc += sizeof(u16);
      asp += 1;
      Next;
    Inst(CCALL3):
      acc = cprims[pu16(pc)](acc, asp[0], asp[1]);
      pc += sizeof(u16);
      asp += 2;
      Next;
    Inst(CCALL4):
      acc = cprims[pu16(pc)](acc, asp[0], asp[1], asp[2]);
      pc += sizeof(u16);
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
    Inst(DIVINT):
      // TODO exn
      acc = Val_int((acc-1)/(*asp++-1));
      Next;
    Inst(DUMMY): {
      u8 n = *pc++;
      value newenv = alloc_block(env, n);
      while (n--)
        Field(newenv, n) = Val_int(0);
      env = newenv;
    }
    Inst(ENDLET): {
      u8 n = *pc++;
      uint32_t size = Wosize_val(env)-n;
      alloc(Tag_hd
      Next;
    }
    Inst(EQ):
      acc = Atom(acc == *asp++);
      Next;
    Inst(EQSTRING):
      acc = Atom(string_compare(acc, *asp++) == 0);
      Next;
    Inst(EQFLOAT): {
      acc = Atom(Double_val(acc) == Double_val(*asp++));
      Next;
    }
    Inst(GEFLOAT): {
      acc = Atom(Double_val(acc) >= Double_val(*asp++));
      Next;
    }
    Inst(GEINT): {
      acc = Atom(acc >= *asp++);
      Next;
    }
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
      acc = Field(global_data, pu16(pc));
      pc += sizeof(u16);
      Next;
    Inst(GETSTRINGITEM):
      acc = string_getitem(acc, Int_val(*asp++));
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
      *--rsp = acc;
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
      uint32_t size = Int_val(acc);
      value block = alloc(Array_tag, 1+size);
      Field(block, 0) = 0; // actual color used by GC
      for (uint32_t i = 1; i <= size; i++)
        Field(block, i) = *asp;
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
    Inst(MODINT):
      acc = (acc-1) % (*asp++-1) + 1;
      Next;
    Inst(MULINT):
      acc = (acc>>1) * (*asp++-1) + 1;
      Next;
    Inst(NEGINT):
      acc = 2-acc;
      Next;
    Inst(NEQ):
      acc = Atom(acc != *asp++);
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
      Next;
    Inst(SETFIELD): {
      value *ptr = &Field(acc, *pc++);
      modify(ptr, *pc++);
      Next;
    }
    Inst(SETGLOBAL):
      modify(&Field(global_data, pu16(pc)), acc);
      pc += sizeof(u16);
      Next;
    Inst(SETSTRINGITEM):
      string_setitem(acc, Int_val(asp[0]), Int_val(asp[1]));
      asp += 2;
      Next;
    Inst(SHLINT):
      acc = 1 | (acc-1) << Int_val(*asp++);
      Next;
    Inst(SHRINT):
      acc = 1 | (uvalue)(acc-1) >> Int_val(*asp++);
      Next;
    Inst(STOP):
      return acc;
    Inst(STRINGLENGTH):
      acc = Val_int(string_length(acc));
      Next;
    Inst(SUBINT): {
      acc -= *asp++ - 1;
      Next;
    }
    Inst(SWITCH):
      pc++;
      pc += pi16(pc+acc-1);
      Next;
    Inst(TAGOF):
      acc = Val_int(Tag_val(acc));
      Next;
    Inst(TERMAPPLY):
termapply: {
      pc = Code_val(acc);
      env = alloc_block(Env_val(acc), 1);
      Field(env, 0) = *--asp;
      Next;
    }
    Inst(UPDATE): {
      tmp = *asp++;
      Tag_val(acc) = Tag_val(tmp);
      REP(i, Wosize_val(tmp))
        modify(&Field(acc, i), Field(tmp, i));
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

static void init_atoms()
{
  REP(i, 256)
    first_atoms[i] = Make_header(i, 0);
}

int main(int argc, char *argv[])
{
  init_atoms();
  return 0;
}
