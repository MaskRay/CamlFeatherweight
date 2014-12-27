output_string "hello, ";
let s = "world" in
for i = 0 to String.length s - 1 do
  output_char s.[i]
done
