let (@@) f x = f x

let rec fib n =
  if n < 2 then
    n
  else
    fic (n-1) + fic (n-2)
and fic n =
  if n < 2 then
    n
  else
    fib (n-1) + fib (n-2)

let fib2 f n =
  if n < 2 then
    n
  else
    f (n-1) + f (n-2)

let z f =
  (fun g -> Obj.magic g g)
  (fun x -> f (fun v -> Obj.magic x x v))
;;

output_string "Simple:\n";
for i = 0 to 10 do
  output_int @@ fib i;
  output_char '\n'
done;

output_string "Z combinator:\n";
for i = 0 to 10 do
  output_int @@ z fib2 i;
  output_char '\n'
done
