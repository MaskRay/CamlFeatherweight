let check b =
  output_char (if b then '.' else 'x')

let (@@) f x = f x;;
let (+:) x y = 2*x+y;;

check (3 +: 4 = 10);
check (1+2*3/4 = 2);
(*check (3.2+.0.2 < 3.5);
check (0.5+.0.5 = 1.);
check (3.2e4 = 3.2e4);
check (4.5 >= 4.4);
check @@ not (4.5 >= 4.6);*)
check (5 land 3 = 1);
check (5 lor 3 = 7);
check (5 lsl 3 = 40);
check ("a" < "b");
check ("aa" < "ab");
check ("aa" > "a")
