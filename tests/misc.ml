let rec ( +: ) fs gs =
  match fs, gs with
  | [], fs -> fs
  | fs, [] -> fs
  | f::fs, g::gs -> f+g::fs+:gs
and ( *: ) fs gs =
  match fs, gs with
  | [], _ | _, [] -> []
  | f::fs, g::gs -> f*g::([f]*:gs +: fs*:(g::gs))
let rec (^:) x n =
  if n <= 1 then
    x
  else
    (x ^: (n-1)) *: x
let rec pr = function [] -> () | x::xs -> output_int x;output_char ' ';pr xs

;;

for i = 1 to 6 do
  pr ([1;1] ^: i);
  output_char '\n'
done
