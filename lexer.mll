{
open Error
open Parser
open Syntax

let keyword_tbl = Hashtbl.create 31;;

List.iter (fun (str,tok) -> Hashtbl.replace keyword_tbl str tok) [
  "and", AND;
  "as", AS;
  "do", DO;
  "done", DONE;
  "downto", DOWNTO;
  "else", ELSE;
  "end", END;
  "for", FOR;
  "fun", FUN;
  "function", FUNCTION;
  "if", IF;
  "in", IN;
  "let", LET;
  "match", MATCH;
  "not", NOT;
  "of", OF;
  "or", OR;
  "rec", REC;
  "then", THEN;
  "to", TO;
  "type", TYPE;
  "while", WHILE;
  "with", WITH
]

(* string *)

let init_string_buf = Bytes.make 256 ' '
let string_buf = ref init_string_buf
let string_idx = ref 0

let reset_string_buf () =
  string_buf := init_string_buf;
  string_idx := 0

let store_string_char c =
  let len = Bytes.length !string_buf in
  if !string_idx >= len then (
    let new_buf = Bytes.make (len*2) ' ' in
    Bytes.blit !string_buf 0 new_buf 0 0;
    string_buf := new_buf
  ) else
    Bytes.unsafe_set !string_buf !string_idx c;
  incr string_idx

let get_stored_string () =
  Bytes.sub_string !string_buf 0 !string_idx

(* escape sequence *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c =
    100 * (int_of_char(Lexing.lexeme_char lexbuf i) - 48) +
     10 * (int_of_char(Lexing.lexeme_char lexbuf (i+1)) - 48) +
          (int_of_char(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  char_of_int(c land 0xFF)
}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule main = parse
  | [' ' '\n' '\r'] { main lexbuf }
  | "(*" {
      let start_pos = Lexing.lexeme_start lexbuf in
      begin try
        comment lexbuf
      with Lexical_error(Unterminated_comment, (_, end_pos)) ->
        raise (Lexical_error(Unterminated_comment, (start_pos, end_pos)))
      end;
      main lexbuf }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
      { CHAR(Lexing.lexeme_char lexbuf 1) }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
      { CHAR(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { CHAR(char_for_decimal_code lexbuf 2) }
  | "'" { QUOTE }
  | '"' {
      reset_string_buf ();
      let start_pos = Lexing.lexeme_start lexbuf in
      begin try
        string lexbuf
      with Lexical_error(Unterminated_string, (_, end_pos)) ->
        raise (Lexical_error(Unterminated_string, (start_pos, end_pos)))
      end;
      STRING (get_stored_string()) }
  | "false" { FALSE }
  | "true" { TRUE }
  | (lower|upper) (lower|upper|digit|'_')* {
      let s = Lexing.lexeme lexbuf in
      try Hashtbl.find keyword_tbl s
      with Not_found -> if 'A' <= s.[0] && s.[0] <= 'Z' then UIDENT s else LIDENT s }
  | digit+
  | '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+
  | '0' ['o' 'O'] ['0'-'7']+
  | '0' ['b' 'B'] ['0'-'1']+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)? { FLOAT(Lexing.lexeme lexbuf |> float_of_string) }
  | "&" { AMPERSAND }
  | "&&" { AMPERAMPER }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "*" { STAR }
  | "," { COMMA }
  | "." { DOT }
  | "->" { MINUSGREATER }
  | ":" { COLON }
  | "::" { COLONCOLON }
  | ";" { SEMI }
  | ";;" { SEMISEMI }
  | "<-" { LESSMINUS }
  | "=" { EQUAL }
  | "==" { EQUALEQUAL }
  | "!=" { INFIX0 "!=" }
  | "[" { LBRACKET }
  | "[|" { LBRACKETBAR }
  | "]" { RBRACKET }
  | "_" { UNDERSCORE }
  | "|" { BAR }
  | "|]" { BARRBRACKET }
  | "||" { BARBAR }

  | [ '!' '?' ] [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' ] *
      { PREFIX(Lexing.lexeme lexbuf) }
  | [ '=' '<' '>' '|' '&' '~' '$' ] [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' ] *
      { INFIX0(Lexing.lexeme lexbuf) }
  | [ '@' '^' ] [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' ] *
      { INFIX1(Lexing.lexeme lexbuf) }
  | [ '+' '-' ] [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' ] *
      { INFIX2(Lexing.lexeme lexbuf) }
  | [ '*' '/' '%' ] [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' ] *
      { INFIX3(Lexing.lexeme lexbuf) }
  | "**" [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' ] *
      { INFIX4(Lexing.lexeme lexbuf) }

  | eof { EOF }
  | _ {
      raise (Lexical_error(Illegal_character(Lexing.lexeme_char lexbuf
      (Lexing.lexeme_start lexbuf)),
      (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf))) }

and comment = parse
  | "*)" { () }
  | "(*" { comment lexbuf; comment lexbuf }
  | _ { comment lexbuf }
  | eof {
      raise (Lexical_error(Unterminated_comment, (0,
      Lexing.lexeme_end lexbuf))) }

and char = parse
  | [^ '\\' '\''] "'"
      { Lexing.lexeme_char lexbuf 0 }
  | '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { char_for_backslash (Lexing.lexeme_char lexbuf 1) }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { char_for_decimal_code lexbuf 1 }
  | [^ '\''] * ("'" | eof)
      { raise (Lexical_error(Bad_char_constant,
                            (Lexing.lexeme_start lexbuf-1,
                            Lexing.lexeme_end lexbuf))) }

and string = parse
  | '"' { () }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | eof {
      raise (Lexical_error(Unterminated_string, (0,
      Lexing.lexeme_end lexbuf))) }
  | _ {
      store_string_char(Lexing.lexeme_char lexbuf 0);
      string lexbuf }
