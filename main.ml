open Error
open Parser
open Lexer
open Syntax
open Implementation

let stage = ref 2

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

let channel ic =
  let lexbuf = Lexing.from_channel ic in
  match !stage with
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
      (*Syntax.dump_expression 0 ast*)
  | _ ->
      try
        let impls = Parser.implementation Lexer.main lexbuf in
        compile_implementation impls
      with Lexical_error(err, (l,m)) ->
        Printf.eprintf "character %d-%d" l m;
        begin match err with
        | Bad_char_constant ->
            prerr_endline "Ill-formed character literal"
        | Illegal_character ch ->
            Printf.eprintf "Illegal character %c\n" ch
        | Unterminated_comment ->
            prerr_endline "Comment not terminated"
        | Unterminated_string ->
            prerr_endline "String literal not terminated"
        end;
        close_in ic
      (*| Parser.Error _ ->*)
        (*prerr_endline "Syntax error";*)
        (*close_in ic*)

let file f =
  channel (if f = "-" then stdin else open_in f)

let () =
  let files = ref [] in
  Arg.parse
    [ "-d", Arg.Int(fun i -> stage := i), "stage"
    ; "-v", Arg.Unit(fun () -> Implementation.verbose := true), "verbose"
    ]
    (fun s -> files := s :: !files)
    ("compiler");
  if !files = [] then
    channel stdin
  else
    List.rev !files |> List.iter file
