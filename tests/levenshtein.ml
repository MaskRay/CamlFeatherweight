let min a b =
  if a < b then a else b

let levenshtein a b =
  let m = String.length a
  and n = String.length b in
  let s = Array.make 2 [||] in
  s.(0) <- Array.make (n+1) 0;
  s.(1) <- Array.make (n+1) 0;
  for j = 1 to n do
    s.(0).(j) <- j
  done;
  for i = 1 to m do
    s.(i land 1).(0) <- i;
    for j = 1 to n do
      s.(i land 1).(j) <-
        if a.[i-1] = b.[j-1] then
          s.((i-1) land 1).(j-1)
        else
          1 + min (min s.((i-1) land 1).(j-1)
              s.((i-1) land 1).(j)
          ) s.(i land 1).(j-1)
    done
  done;
  s.(m land 1).(n)

let () =
  output_int (levenshtein "abcdheloworldabcd" "abcdhellowoorldabcd")
