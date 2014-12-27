exception A of int;;

try
  try 1/0; ()
  with A i -> output_int i
with Division_by_zero ->
  output_char 't'
;;

try
  let Some x = None in
  ()
with Match_failure(s,i,j) ->
  output_int i;
  output_char '\n';
  output_int j
