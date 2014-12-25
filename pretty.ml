open Prettiest
open Syntax

let pri_in = 5
let pri_semi = 10
let pri_function = 30
let pri_with = 30
let pri_and = 35
let pri_then = 37
let pri_else = 38
let pri_as = 42
let pri_bar = 43
let pri_comma = 45
let pri_minusgreater = 47
let pri_barbar = 50
let pri_amperamper = 52
let pri_equal = 55
let pri_infix0 = 55
let pri_infix1 = 60
let pri_coloncolon = 65
let pri_infix2 = 70
let pri_infix3 = 80
let pri_infix4 = 90
let pri_unary_minus = 95
let pri_app = 110
let pri_dot = 120
let pri_prefix = 130

let paren flag doc =
  if flag then
    lparen <.> align doc <.> rparen
  else
    doc

let pretty_constant pri = function
  | Const_char x ->
      char '\'' <.> text (Char.escaped x) <.> char '\''
  | Const_int x ->
      let d = text @@ string_of_int x in
      if x < 0 then
        paren (pri>=pri_unary_minus) d
      else
        d
  | Const_float x ->
      let d = text @@ string_of_float x in
      if x < 0. then
        paren (pri>=pri_unary_minus) d
      else
        d
  | Const_string x ->
      char '"' <.> text (String.escaped x) <.> char '"'

let pretty_longid id = text @@ string_of_long_ident id

let pretty_tuple pretty pri xs =
  match xs with
  | [] -> assert false
  | x::xs ->
      paren (pri>=pri_comma) @@
      List.fold_left (fun acc x ->
        acc <//> char ',' </> pretty pri_comma x
      ) (pretty pri_comma x) xs

let pretty_array pretty pri xs =
  match xs with
  | [] -> assert false
  | x::xs ->
      let con =
        List.fold_left (fun acc x ->
          acc <//> char ';' </> pretty pri_semi x
        ) (pretty pri_semi x) xs
      in
      text "[|" </> con </> text "|]"

let rec pretty_expr pri expr =
  let rec go pri expr =
    match expr.e_desc with
    | Pexpr_apply(e,es) ->
        let fall () = fill_sep @@ List.map (go pri_app) (e::es) in
        begin match es with
        | [x; y] ->
          begin match e.e_desc with
          | Pexpr_ident (Lident op) ->
              if op = "&&" then
                paren (pri>=pri_amperamper) (
                  go pri_amperamper x </> text op </> go (pri_amperamper-1) y)
              else if op = "||" then
                paren (pri>=pri_barbar) (
                  go pri_barbar x </> text op </> go (pri_barbar-1) y)
              else if op.[0] = '=' || op.[0] = '<' || op.[0] = '>' then
                paren (pri>=pri_infix0) (
                  go (pri_infix0-1) x </> text op </> go pri_infix0 y)
              else if op.[0] = '@' || op.[0] = '^' then
                paren (pri>=pri_infix1) (
                  go pri_infix1 x </> text op </> go (pri_infix1-1) y)
              else if op.[0] = '+' || op.[0] = '-' then
                paren (pri>=pri_infix2) (
                  go (pri_infix2-1) x </> text op </> go pri_infix2 y)
              else if op.[0] = '*' || op.[0] = '/' || op = "land" || op = "lor" || op = "lxor" then
                paren (pri>=pri_infix3) (
                  go (pri_infix3-1) x </> text op </> go pri_infix3 y)
              else if String.length op >= 2 && op.[0] = '*' && op.[1] = '*' || op = "lsl" || op = "lsr" then
                paren (pri>=pri_infix4) (
                  go pri_infix4 x </> text op </> go (pri_infix4-1) y)
              else
                fall()
          | _ -> fall()
          end
        | _ -> fall()
        end
    | Pexpr_array es ->
        pretty_array go pri es
    | Pexpr_constant c ->
        pretty_constant pri c
    | Pexpr_constr(id,e) ->
        begin match e with
        | None ->
            pretty_longid id
        | Some e ->
            let fall () = group (pretty_longid id </> go pri_app e) in
            begin match e.e_desc with
            | Pexpr_tuple [x; y] ->
                if id = Lident "::" then
                  paren (pri>=pri_coloncolon) (
                    go pri_coloncolon x </> text "::" </> go (pri_coloncolon-1) y
                  )
                else
                  paren (pri>=pri_app) (fall())
            | _ -> fall()
            end
        end
    | Pexpr_constraint(e,t) ->
        lparen <.> align (
          go pri_as e <+> char ':' <+> pretty_type_expression pri_as t
        ) <.> rparen
    | Pexpr_for(name,start,stop,up,body) ->
        text "for" <+> text name <+> char '=' <+>
        go pri_equal start <+> text (if up then "to" else "downto") <+>
        go pri_equal stop <+> text "do" <.>
        nest 2 (line <.> go 0 body) <$>
        text "done"
    | Pexpr_function alts ->
        let keyword = text "function" in
        paren (pri>0)
        begin match alts with
        | [] -> assert false
        | [pe] ->
            keyword <.> nest 2 (pretty_action pe)
        | _ ->
            align (keyword <$> char '|' <.>
              sep_by (line <.> char '|')
              (List.map (fun pe' -> nest 2 @@ pretty_action pe') alts))
        end
    | Pexpr_ident id ->
        pretty_longid id
    | Pexpr_if(cond,ifso,ifnot) ->
        let ifso pri' =
          text "if" <+> align (go pri cond) </> text "then" <.>
          nest 2 (line <.> go pri ifso)
        in
        paren (pri>=pri_else)
        begin match ifnot with
        | None ->
            ifso pri_then
        | Some ifnot ->
            align (
              ifso pri_else <$>
              text "else" <.>
              nest 2 (line <.> go pri_else ifnot)
            )
        end
    | Pexpr_let(isrec,binds,body) ->
        let d = pretty_let_and_binds isrec binds in
        align (d </> text "in" <$> go 0 body)
    | Pexpr_sequence(e1,e2) ->
        paren (pri>=pri_semi) (
          go pri_semi e1 <.> char ';' <$>
          go (pri_semi-1) e2
        )
    | Pexpr_try(body,pes) ->
        let keyword = text "with" in
        align (
          (text "try" <.> nest 2 (softline <.>
            go 0 body)) <$>
          begin match pes with
          | [] -> assert false
          | [pe] ->
              keyword <.> nest 4 (pretty_action pe)
          | _ ->
              align (keyword <$> char '|' <.>
                sep_by (line <.> char '|')
                (List.map (fun pe' -> nest 4 @@ pretty_action pe') pes))
          end)
    | Pexpr_tuple es ->
        pretty_tuple go pri es
  in
  go pri expr

and pretty_let_and_binds isrec binds =
  let keyword = if isrec then text "let" <+> text "rec" else text "let" in
  match binds with
  | [] -> assert false
  | [p,e as pe] ->
      let binds =
        let rec collect ps e =
          match e.e_desc with
          | Pexpr_function [p',e'] ->
              collect (p'::ps) e'
          | _ ->
              pretty_function (List.rev ps) e
        in
        collect [p] e
      in
      keyword <.> nest 2 binds
  | pe::pes ->
      keyword <.> nest 2 (pretty_bind pe) </> text "and" <.>
        sep_by (space <.> text "and" <.> softline)
        (List.map (fun pe' -> nest 2 @@ pretty_bind pe') pes)

and pretty_bind (p,e) =
  softline <.>
    pretty_pat pri_equal p </> char '=' <.> (softline <.>
      pretty_expr pri_equal e)

and pretty_function ps e =
  softline <.> sep_by softline (List.map (pretty_pat pri_app) ps) </>
    char '=' <.> (softline <.> pretty_expr pri_equal e)

and pretty_action (p,e) =
  softline <.>
    pretty_pat 0 p </> text "->" <.> (softline <.>
      pretty_expr 0 e)

and pretty_type_expression pri te =
  let rec go pri te =
    match te.te_desc with
    | Ptype_arrow(te1,te2) ->
        paren (pri>=pri_minusgreater) (
          go pri_minusgreater te1 </> text "->" </>
          go (pri_minusgreater-1) te2
        )
    | Ptype_constr(id,tes) ->
        begin match tes with
        | [] ->
            pretty_longid id
        | [te] ->
            go pri_app te <+> pretty_longid id
        | te::tes ->
            lparen <.> align (
              List.fold_left (fun acc te ->
                acc </> char ',' </> go pri_comma te
              ) (go pri_comma te) tes
            ) <.> rparen <+> pretty_longid id
        end
    | Ptype_tuple tes ->
        begin match tes with
        | [] -> assert false
        | te::tes ->
            paren (pri>=pri_comma) @@
            List.fold_left (fun acc te ->
              acc <.> char ',' <$> go pri_comma te
            ) (go pri_comma te) tes
        end
    | Ptype_var v ->
        char '\'' <.> text v
  in
  go pri te
and pretty_pat pri pat =
  let rec go pri p =
    match p.p_desc with
    | Ppat_alias(p,a) ->
        go pri_as p </> text "as" </> text a
    | Ppat_any ->
        char '_'
    | Ppat_array ps ->
        pretty_array go pri ps
    | Ppat_constant c ->
        pretty_constant pri c
    | Ppat_constr(id,arg) ->
        begin match id with
        | Lident "()" ->
            text "()"
        | _ ->
            begin match arg with
            | None ->
                text @@ string_of_long_ident id
            | Some arg ->
                text (string_of_long_ident id) </> go pri_app arg
            end
        end
    | Ppat_constraint(p,te) ->
        lparen <//> go pri_as p </> char ':' </>
        pretty_type_expression pri_as te <//> rparen
    | Ppat_or(p1,p2) ->
        go pri_bar p1 </> char '|' </>
        go (pri_bar-1) p2
    | Ppat_tuple ps ->
        pretty_tuple go pri ps
    | Ppat_var v ->
        text v
  in
  go pri pat

let pretty_letdef isrec binds =
  align (pretty_let_and_binds isrec binds)

let pprint_impl width impl =
  let doc =
    match impl.im_desc with
    | Pimpl_expr e ->
        pretty_expr 0 e
    | Pimpl_typedef decl ->
        empty
    | Pimpl_letdef(isrec,pes) ->
        pretty_letdef isrec pes
  in
  print_endline @@ render 1.0 width doc

let pprint_implementation width impls =
  List.iter (pprint_impl width) impls
