open Syntax

type instruction =
  | Kaccess of int
  | Kapply
  | Kbranch of label
  | Kbranchif of label
  | Kbranchifnot of label
  | Kbranchifnot of label
  | Kclosure
  | Kdummies
  | Kendlet
  | Kgetglobal of longident
  | Kgrab
  | Klabel of label
  | Klet
  | Kletrec1
  | Kmakeblock of constr_tag * int
  | Kprim of primitive
  | Kpush
  | Kpushmark
  | Kquote of constant
  | Kreturn
  | Ksetglobal of longident
  | Kswitch of label array
  | Ktermapply
  | Kupdate

and label = Nolabel | Label of num
