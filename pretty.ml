open Fpretty
open Syntax

let pri_in = 5
let pri_semi = 10
let pri_else = 20
let pri_function = 30
let pri_with = 30
let pri_and = 40
let pri_comma = 45
let pri_barbar = 50
let pri_amperamper = 52
let pri_equal = 55
let pri_infix0 = 55
let pri_infix1 = 60
let pri_coloncolon = 65
let pri_infix2 = 70
let pri_infix3 = 80
let pri_infix4 = 90
let pri_as = 100
let pri_app = 110
let pri_dot = 120

let pri_te_comma = 0
let pri_te_arrow = 10
let pri_te_constr = 20

let pri_p_as = 0
let pri_p_semi = 10
let pri_p_comma = 20

let paren flag doc =
  if flag then
    lparen <.> align doc <.> rparen
  else
    doc

let pretty_constant = function
  | Const_char x ->
      char '\'' <.> text (Char.escaped x) <.> char '\''
  | Const_int x ->
      text @@ string_of_int x
  | Const_float x ->
      text @@ string_of_float x
  | Const_string x ->
      char '"' <.> text (String.escaped x) <.> char '"'

let pretty_longid id = text @@ string_of_long_ident id

let rec pretty_expr pri expr =
  let rec go pri expr =
    match expr.e_desc with
    | Pexpr_apply(e,es) ->
        let fall () = fill_sep @@ List.map (go pri) (e::es) in
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
              else if String.length op >= 2 && op.[0] = '*' && op.[1] = '*' then
                paren (pri>=pri_infix4) (
                  go pri_infix4 x </> text op </> go (pri_infix4-1) y)
              else if op.[0] = '*' || op.[0] = '/' then
                paren (pri>=pri_infix3) (
                  go (pri_infix3-1) x </> text op </> go pri_infix3 y)
              else
                fall()
          | _ -> fall()
          end
        | _ -> fall()
        end
    | Pexpr_array es ->
        let con = match es with
          | [] -> empty
          | e::es ->
              List.fold_left (fun acc e ->
                acc <//> char ';' </> go pri_semi e
              ) (go pri_semi e) es
        in
        text "[|" </> con </> text "|]"
    | Pexpr_constant c ->
        pretty_constant c
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
        text "function TODO"
    | Pexpr_ident id ->
        pretty_longid id
    | Pexpr_if(cond,ifso,ifnot) ->
        let ifso =
          text "if" <+> align (go pri_else cond) </> text "then" <.>
          nest 2 (line <.> go pri_else ifso)
        in
        paren (pri>=pri_else)
        begin match ifnot with
        | None ->
            ifso
        | Some ifnot ->
            align (
              ifso <$>
              text "else" <.>
              nest 2 (line <.> go pri_else ifnot)
            )
        end
    | Pexpr_let(isrec,binds,body) ->
        let keyword =
          if isrec then text "let" else text "let" <+> text "rec"
        in
        paren (pri>=pri_in) (
          keyword </>
          sep_by (space <.> text "and" <.> softline)
          (List.map (fun (p,e) ->
            pretty_pat 0 p <+> char '=' <+> align (pretty_expr 0 e)
          ) binds) <$>
          text "in" <$>
          pretty_expr (pri_in-1) body
        )
    | Pexpr_sequence(e1,e2) ->
        paren (pri>=pri_semi) (
          go pri_semi e1 <.> char ';' </>
          go (pri_semi-1) e2
        )
    | Pexpr_tuple es ->
        begin match es with
        | [] -> assert false
        | e::es ->
            paren (pri>=pri_comma) @@
            List.fold_left (fun acc e ->
              acc <.> char ',' <$> go pri_comma e
            ) (go pri_comma e) es
        end
  in
  go pri expr

and pretty_type_expression pri te =
  let rec go pri te =
    match te.te_desc with
    | Ptype_arrow(te1,te2) ->
        paren (pri>=pri_te_arrow) (
          go pri_te_arrow te1 </> text "->" </>
          go (pri_te_arrow-1) te2
        )
    | Ptype_constr(id,tes) ->
        begin match tes with
        | [] ->
            pretty_longid id
        | [te] ->
            go pri_te_constr te <+> pretty_longid id
        | te::tes ->
            lparen <.> align (
              List.fold_left (fun acc te ->
                acc <.> char ',' <$> go pri_te_comma te
              ) (go pri_te_comma te) tes
            ) <.> rparen <+> pretty_longid id
        end
    | Ptype_tuple tes ->
        begin match tes with
        | [] -> assert false
        | te::tes ->
            paren (pri>=pri_te_comma) @@
            List.fold_left (fun acc te ->
              acc <.> char ',' <$> go pri_te_comma te
            ) (go pri_te_comma te) tes
        end
    | Ptype_var v ->
        text v
  in
  go pri te
and pretty_pat pri pat =
  let rec go pri p =
    match p.p_desc with
    | Ppat_alias(p,a) ->
        go pri_p_as p </> text "as" </> text a
    | Ppat_any ->
        char '_'
    | Ppat_array ps ->
        let con = match ps with
          | [] -> empty
          | e::es ->
              List.fold_left (fun acc p ->
                acc <//> char ';' </> go pri_p_semi p
              ) (go pri_p_semi e) ps
        in
        text "[|" </> con </> text "|]"
    | Ppat_tuple ps ->
        begin match ps with
        | [] -> assert false
        | p::ps ->
            paren (pri>=pri_p_comma) @@
            List.fold_left (fun acc p ->
              acc <.> char ',' <$> go pri_p_comma p
            ) (go pri_p_comma p) ps
        end
    | Ppat_var v ->
        text v
  in
  go pri pat

let pretty_letdef isrec pes =
  text (if isrec then "let" else "let rec")

let pprint_impl width impl =
  let doc =
    match impl.im_desc with
    | Pimpl_expr e ->
        pretty_expr 0 e
    | Pimpl_typedef decl ->
        empty
    | Pimpl_letdef(isrec,binds) ->
        empty
  in
  print_endline @@ render width doc

let pprint_implementation width impls =
  List.iter (pprint_impl width) impls
