module List = struct
  include List
  let zip xs ys =
    let rec go acc = function
      | [], _ | _, [] -> List.rev acc
      | x::xs, y::ys -> go ((x,y)::acc) (xs,ys)
    in
    go [] (xs,ys)
  let rec take n xs =
    match n, xs with
    | 0, _ | _, [] -> []
    | n, x::xs -> x :: take (n-1) xs
  let rec drop n xs =
    match n, xs with
    | 0, _ | _, [] -> xs
    | n, _::xs -> drop (n-1) xs
  let rec last = function
    | [] -> failwith "last: empty list"
    | [x] -> x
    | x::xs -> last xs
  let split_at k xs =
    let rec go l k = function
      | xs when k = 0 ->
          List.rev l, xs
      | [] ->
          List.rev l, []
      | x::xs ->
          go (x::l) (k-1) xs
    in
    go [] k xs
  let iota n =
    let rec go acc n =
      if n = 0 then
        acc
      else
        go ((n-1)::acc) (n-1)
    in
    go [] n
  let map_accuml_rev f acc xs =
    let acc, ys = List.fold_left (fun (acc,ys) x ->
      let acc, y = f acc x in
      acc, y::ys
    ) (acc,[]) xs
    in
    acc, ys
  let map_accuml f acc xs =
    let acc, ys = map_accuml_rev f acc xs in
    acc, List.rev ys
end

(* deque *)

type 'a deque = 'a list * 'a list

let emptyq = [], []
let pushl x (xs,ys) = x::xs, ys
let pushr x (xs,ys) = xs, x::ys
let viewl = function
  | [], [] ->
      None
  | [], ys ->
      let n = List.length ys in
      let ys, xs = List.split_at (n/2) ys in
      (match List.rev xs with
      | x::xs ->
          Some (x, (xs,ys))
      | _ -> assert false
      )
  | x::xs, ys ->
      Some (x, (xs,ys))
let viewr = function
  | [], [] ->
      None
  | xs, [] ->
      let n = List.length xs in
      let xs, ys = List.split_at (n/2) xs in
      (match List.rev ys with
      | y::ys ->
          Some (y, (xs,ys))
      | _ -> assert false
      )
  | xs, y::ys ->
      Some (y, (xs,ys))

type doc =
  | Nil
  | Line of int
  | Char of char
  | Text of string
  | Cat of doc * doc
  | Group of doc
  | Nest of int * doc
  | Align of int * doc

let empty = Nil
let line = Line 1
let linebreak = Line 0
let char c = Char c
let text t = Text t
let (<.>) x y = Cat (x, y)
let group x = Group x
let nest i x = Nest (i, x)
let align x = Align (0, x)

let space = Char ' '
let softline = group line
let softbreak = group linebreak
let lbrace = Char '{'
let rbrace = Char '}'
let lbracket = Char '['
let rbracket = Char ']'
let langle = Char '<'
let rangle = Char '>'
let lparen = Char '('
let rparen = Char ')'
let semicolon = Char ';'
let comma = Char ','
let int i = Text (string_of_int i)

let (<+>) x y = x <.> text " " <.> y
let (<$>) x y = x <.> line <.> y
let (<$$>) x y = x <.> linebreak <.> y
let (</>) x y = x <.> softline <.> y
let (<//>) x y = x <.> softbreak <.> y
let enclose l r x = l <.> x <.> r
let fill_sep = List.fold_left (</>) empty

let rec sep_by sep = function
  | [] -> empty
  | x::xs ->
    let rec go acc = function
      | [] -> acc
      | x::xs -> go (acc <.> sep <.> x) xs
    in
    align (go x xs)

let rec enclose_sep l r sep = function
  | [] -> l <.> r
  | x::xs ->
    let rec go acc = function
      | [] -> acc
      | x::xs -> go (acc <.> sep <.> x) xs
    in
    align (go (l <.> x) xs <.> r) <.> Align (-10, empty)

let rec enclose_sep_a l r sep a =
  let n = Array.length a in
  if n = 0 then
    l <.> r
  else
    let rec go acc i =
      if i = n then
        acc
      else
        go (acc <.> sep <.> a.(i)) (i+1)
    in
    align (go (l <.> a.(0)) 1 <.> r)

let rec len = function
  | Nil -> 0
  | Char _ -> 1
  | Text t -> String.length t
  | Cat (x, y) -> len x + len y
  | _ -> assert false

let normalize x =
  (* ensure y and (fst result) only built from text,nil and Cat *)
  let rec go x y =
    match x with
    | Nil -> y, Nil
    | Line _ -> Nil, x <.> y
    | Char _ | Text _ -> x <.> y, Nil
    | Cat (u,v) ->
        let l2,r2 = go v y in
        let l1,r1 = go u l2 in
        l1, r1 <.> r2
    | Group u ->
        let l,r = go u y in
        l, Group r
    | Nest (i,u) ->
        let l,r = go u y in
        l, Nest (i, r)
    | Align (i,u) ->
        let l,r = go u y in
        l, Align (i-len l, r)
  in
  let l, r = go x Nil in
  l <.> r

type pos = int
type remaining = int
type indentation = int list
type horizontal = bool
type out = remaining -> indentation -> unit
type outg = horizontal -> out -> out
type dq = (pos*outg) deque
type tree_cont = pos -> dq -> out

let render w x =
  let buf = Buffer.create 0 in
  let rec interpret doc (w:int) (tc:tree_cont) (p:pos) (dq:dq) : out =
    let rec prune (tc:tree_cont) (p:pos) (dq:dq) (r:remaining) =
      match viewl dq with
      | None -> tc p dq r
      | Some ((s,outg),dq') ->
          if s+r < p then
            outg false (prune tc p dq') r
          else
            tc p dq r
    in
    let extend cont1 cont2 outg (tc:tree_cont) (p:pos) (dq:dq) =
      (match viewr dq with
      | None ->
          outg false (cont1 tc p dq)
      | Some ((s,outg2),dq') ->
          let outg' h c r = outg2 h (outg h c) r in
          cont2 tc p (pushr (s, outg') dq')
      )
    in
    let leave (tc:tree_cont) (p:pos) (dq:dq) =
      match viewr dq with
      | None ->
          tc p dq
      | Some ((s1,outg1),dq1) ->
          (match viewr dq1 with
          | None ->
              outg1 true (tc p emptyq)
          | Some ((s2,outg2),dq2) ->
              let outg' h c = outg2 h (fun r1 -> outg1 (p <= s2+r1) c r1) in
              tc p (pushr (s2,outg') dq2)
          )
    in
    let id x = x in
    match doc with
    | Nil ->
        tc p dq
    | Line l ->
        let outg h c r i =
          if h then (
            Buffer.add_string buf @@ String.make l ' ';
            c (r-l) i
          ) else (
            Buffer.add_char buf '\n';
            Buffer.add_string buf @@ String.make (List.hd i) ' ';
            c (w-List.hd i) i
          )
        in
        extend id prune outg tc (p+l) dq
    | Char ch ->
        let outg h c r i =
          Buffer.add_char buf ch;
          c (r-1) i
        in
        extend id prune outg tc (p+1) dq
    | Text t ->
        let l = String.length t in
        let outg h c r i =
          Buffer.add_string buf t;
          c (r-l) i
        in
        extend id prune outg tc (p+l) dq
    | Cat (x,y) ->
        interpret x w (interpret y w tc) p dq
    | Group x ->
        interpret x w (leave tc) p (pushr (p, fun h c -> c) dq)
    | Nest (j,x) ->
        let outg h c r i = c r ((List.hd i+j)::i) in
        let undo h c r i = c r (List.tl i) in
        let f tc = interpret x w (extend id id undo tc) in
        extend f f outg tc p dq
    | Align (j,x) ->
        let outg h c r i = c r ((w-r+j)::i) in
        let undo h c r i = c r (List.tl i) in
        let f tc = interpret x w (extend id id undo tc) in
        extend f f outg tc p dq
  in
  interpret (normalize x) w (fun p dq r i -> ()) 0 emptyq w [0];
  Buffer.contents buf

let pretty = render
let take n s = String.sub s 0 n

let prop0 = pretty 6 (group (text "Hi" <.> line <.> text "you") <.> text "!") =
        "Hi\nyou!"
let prop1 = pretty 4 (group (text "hi" <.> line <.> text "world")) =
        "hi\nworld"
let prop2 = 
  pretty 8 (group (text "hi" <.> line <.> text "world") <.> text "liness") =
  "hi\nworldliness"
let prop3 = 
  take 6 (pretty 4 (group (text "hi" <.> line <.> text "you" <.> empty))) =
  "hi\nyou"
let prop4 = 
  take 6 (pretty 4 (group (text "hi" <.> line) <.>
           group (text "you" <.> line) <.> empty)) =
  "hi\nyou"
let prop5 = 
  take 6 (pretty 4 (group (text "hi" <.> 
           group (line <.> text "you" <.> empty)))) =
  "hi\nyou"
let prop6 = 
  take 7 (pretty 3 (group (text "hi" <.> line <.> 
           group (line <.> text "you" <.> empty)))) =
  "hi\n\nyou"
let prop7 = 
  pretty 10 (group (text "what" <.>
    align (group (text "do" <.> line <.> text "you" <.> line <.> 
      text "do" <.> align (line <.> text "now?"))))) =
  "whatdo\n    you\n    do\n      now?"

let prop8 = 
  pretty 10 (group (text "one " <.> (align (line <.> text "two" <.> 
    align (line <.> text "three"))))) =
  "one \n    two\n       three"

let prop9 =
  pretty 10 (group (text "one " <.> (nest 2 (line <.> text "two" <.>
    nest 3 (line <.> text "three"))))) =
  "one \n  two\n     three"
