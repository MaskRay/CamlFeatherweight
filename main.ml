open Error
open Location
open Parser
open Lexer
open Syntax
open Implementation

let dbg_lexer lexbuf =
  match Lexer.main lexbuf with
  | EOF -> "EOF"

  | CHAR c -> "CHAR " ^ String.escaped (String.make 1 c)
  | INT i -> "INT " ^ string_of_int i
  | FLOAT f -> "FLOAT " ^ string_of_float f
  | STRING s -> "STRING(" ^ String.escaped s ^ ")"
  | LIDENT id -> "LIDENT(" ^ id ^ ")"
  | UIDENT id -> "UIDENT(" ^ id ^ ")"
  | PREFIX op -> "PREFIX " ^ op
  | INFIX0 op -> "INFIX0 " ^ op
  | INFIX1 op -> "INFIX1 " ^ op
  | INFIX2 op -> "INFIX2 " ^ op
  | INFIX3 op -> "INFIX3 " ^ op
  | INFIX4 op -> "INFIX4 " ^ op

  | EQUAL -> "="
  | EQUALEQUAL -> "=="
  | LPAREN -> "("
  | RPAREN -> ")"
  | STAR -> "*"
  | COMMA -> ","
  | MINUSGREATER -> "->"
  | DOT -> "."
  | COLON -> ":"
  | COLONCOLON -> "::"
  | COLONEQUAL -> ":="
  | SEMI -> ";"
  | SEMISEMI -> ";;"
  | LBRACKET -> "["
  | LBRACKETBAR -> "[|"
  | LESSMINUS -> "<-"
  | RBRACKET -> "]"
  | QUOTE -> "'"
  | UNDERSCORE -> "_"
  | BAR -> "|"
  | BARRBRACKET -> "|]"
  | RBRACE -> "}"
  | AMPERSAND -> "&"
  | AMPERAMPER -> "&&"
  | BARBAR -> "||"
  | AND -> "and"
  | AS -> "as"
  | BEGIN -> "begin"
  | ELSE -> "else"
  | END -> "end"
  | FALSE -> "false"
  | FOR -> "for"
  | FUN -> "fun"
  | FUNCTION -> "function"
  | IF -> "if"
  | IN -> "in"
  | LET -> "let"
  | MATCH -> "match"
  | MUTABLE -> "mutable"
  | NOT -> "not"
  | OF -> "of"
  | OR -> "or"
  | REC -> "rec"
  | THEN -> "then"
  | TO -> "to"
  | TRUE -> "true"
  | TYPE -> "type"
  | WITH -> "with"

let file f =
  if not (Filename.check_suffix f ".ml") then (
    Printf.eprintf "Input files should be `*.ml'\n";
    exit 1
  );
  let basename = Filename.chop_suffix f ".ml" in
  let obj_name = basename ^ ".zo" in

  let ic = open_in_bin f in
  let oc = open_out_bin obj_name in
  Location.input_chan := ic;
  let lexbuf = Lexing.from_channel ic in
  begin match !stage with
  | 0 ->
      let rec go () =
        let s = dbg_lexer lexbuf in
        Printf.printf "%s " s;
        if s <> "EOF" then
          go ()
      in
      go ()
  | 1 ->
      let ast = Parser.implementation Lexer.main lexbuf in
      List.iter (Syntax.dump_impl_phrase 0) ast
  | _ ->
      try
        let impls = Parser.implementation Lexer.main lexbuf in
        compile_implementation oc impls
      with Lexical_error(err, l) ->
        (*Printf.eprintf "character %d-%d" l m;*)
        begin match err with
        | Bad_char_constant ->
            Printf.eprintf "%aIll-formed character literal\n"
            output_location l
        | Illegal_character ch ->
            Printf.eprintf "%aIllegal character %c\n"
            output_location l
            ch
        | Unterminated_comment ->
            Printf.eprintf "%aComment not terminated\n"
            output_location l
        | Unterminated_string ->
            Printf.eprintf "%aString literal not terminated\n"
            output_location l
        end
      | Failure _ ->
        prerr_endline "Syntax error";
        close_in ic
  end;
  close_in ic;
  close_out oc

let () =
  Arg.parse
    [ "-d", Arg.Int(fun i -> stage := i), "stage"
    ; "-v", Arg.Unit(fun () -> Implementation.verbose := true), "verbose"
    ]
    file
    ("compiler")
