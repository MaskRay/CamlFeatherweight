type t = A | B of int * int | C;;

match false, B (2,4) with
| _,A ->
    output_char '0'
| true,B( i,j) ->
    ()
| _,B( i,j) ->
    output_int i;
    output_int j;
| _,C ->
    output_char '2'
