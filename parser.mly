%{
open Syntax
open Builtin

let get_loc () = Parsing.symbol_start (), Parsing.symbol_end ()

let make_expr desc =
  {e_desc=desc; e_loc=get_loc(); e_typ=no_type}

let make_pat desc =
  {p_desc=desc; p_loc=get_loc(); p_typ=no_type}

let make_type_expression desc =
  {te_desc=desc; te_loc=get_loc()}

let make_impl desc =
  {im_desc=desc; im_loc=get_loc()}

let make_unop op ({e_loc=l1,_} as e1) =
  let l,_ as loc = get_loc() in
  {e_desc=Pexpr_apply({e_desc=Pexpr_ident(Lident op);
                       e_loc=(l,l1);
                       e_typ=no_type}, [e1]);
   e_loc=loc;
   e_typ=no_type}

let make_binop op ({e_loc=_,m1} as e1) ({e_loc=l2,_} as e2) =
  Pexpr_apply({e_desc=Pexpr_ident(Lident op);
               e_loc=(m1,l2);
               e_typ=no_type}, [e1; e2]) |> make_expr

let make_ternop op ({e_loc=_,m1} as e1) ({e_loc=l2,_} as e2) e3 =
  Pexpr_apply({e_desc=Pexpr_ident(Lident op);
               e_loc=(m1,l2);
               e_typ=no_type}, [e1; e2; e3]) |> make_expr

let make_expr_list es =
  List.fold_right (fun e acc ->
    make_expr (Pexpr_constr(constr_cons,
      Some(make_expr(Pexpr_tuple [e; acc]))))
  ) es (make_expr(Pexpr_constr(constr_nil, None)))

let make_pat_list es =
  List.fold_right (fun p acc ->
    make_pat (Ppat_constr(constr_cons,
      Some(make_pat(Ppat_tuple [p; acc]))))
  ) es (make_pat(Ppat_constr(constr_nil, None)))
%}

%token <char> CHAR
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT
%token <string> PREFIX
%token <string> INFIX0
%token <string> INFIX1
%token <string> INFIX2
%token <string> INFIX3
%token <string> INFIX4

%token EOF

%token EQUAL          /* "=" */
%token EQUALEQUAL     /* "==" */
%token LPAREN         /* "(" */
%token RPAREN         /* ")" */
%token STAR           /* "*" */
%token COMMA          /* "," */
%token MINUSGREATER   /* "->" */
%token DOT            /* "." */
%token COLON          /* ":" */
%token COLONCOLON     /* "::" */
%token COLONEQUAL     /* ":=" */
%token SEMI           /* ";" */
%token SEMISEMI       /* ";;" */
%token LBRACKET       /* "[" */
%token LBRACKETBAR    /* "[|" */
%token LESSMINUS      /* "<-" */
%token RBRACKET       /* "]" */
%token QUOTE          /* "'" */
%token UNDERSCORE     /* "_" */
%token BAR            /* "|" */
%token BARRBRACKET    /* "|]" */
%token RBRACE         /* "}" */
%token AMPERSAND      /* "&" */
%token AMPERAMPER     /* "&&" */
%token BARBAR         /* "||" */
/* Keywords */
%token AND            /* "and" */
%token AS             /* "as" */
%token BEGIN          /* "begin" */
%token ELSE           /* "else" */
%token END            /* "end" */
%token FOR            /* "for" */
%token FUN            /* "fun" */
%token FUNCTION       /* "function" */
%token IF             /* "if" */
%token IN             /* "in" */
%token LET            /* "let" */
%token MATCH          /* "match" */
%token MUTABLE        /* "mutable" */
%token NOT            /* "not" */
%token OF             /* "of" */
%token OR             /* "or" */
%token REC            /* "rec" */
%token THEN           /* "then" */
%token TO             /* "to" */
%token TYPE           /* "type" */
%token WITH           /* "with" */

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI
%nonassoc LET
%nonassoc FUNCTION WITH
%nonassoc AND
%nonassoc THEN
%nonassoc ELSE
%nonassoc LESSMINUS
%right COLONEQUAL
%nonassoc AS
%right BAR
%nonassoc below_COMMA
%left COMMA
%nonassoc MINUSGREATER
%right OR BARBAR
%right AMPERSAND AMPERAMPER

%left NOT
%left INFIX0 EQUAL
%right INFIX1
%right COLONCOLON
%left INFIX2
%left INFIX3 STAR
%right INFIX4
%left DOT
%right PREFIX

%start implementation
%type <Syntax.impl_phrase list> implementation
%start test_expr
%type <Syntax.expression> test_expr

%%

test_expr:
  | expr EOF { $1 }

implementation:
  | structure EOF { $1 }

structure:
  | structure_tail { $1 }
  | seq_expr SEMISEMI structure_tail { make_impl(Pimpl_expr $1)::$3 }

structure_tail:
  | /* emtpy */ { [] }
  | SEMISEMI { [] }
  | SEMISEMI seq_expr structure_tail { make_impl(Pimpl_expr $2)::$3 }
  | SEMISEMI structure_item structure_tail { $2::$3 }
  | structure_item structure_tail { $1::$2 }

structure_item:
  | TYPE type_decl_list { make_impl(Pimpl_typedef $2) }
  | LET rec_flag let_binding_list { make_impl(Pimpl_letdef($2, $3)) }

/* type */

type_:
  | simple_type { $1 }
  | type_star_list { make_type_expression(Ptype_tuple(List.rev $1)) }
  | simple_type MINUSGREATER type_ { make_type_expression(Ptype_arrow($1, $3)) }

simple_type:
  | type_var { make_type_expression(Ptype_var $1) }
  | IDENT { make_type_expression(Ptype_constr(Lident $1, [])) }
  | LPAREN type_ RPAREN { $2 }
  | LPAREN type_ COMMA type_comma_list RPAREN IDENT { make_type_expression(Ptype_constr(Lident $6, $2::$4)) }

type_comma_list:
  | type_ COMMA type_comma_list { $1::$3 }
  | type_ { [$1] }

type_star_list:
  | type_star_list STAR simple_type { $3::$1 }
  | simple_type STAR simple_type { [$3; $1] }

/* type declaration */

type_decl_list:
  | type_decl { [$1] }
  | type_decl AND type_decl_list { $1::$3 }

type_decl:
  | type_vars IDENT type_def { $2, $1, $3 }

type_vars:
  | LPAREN type_var_list RPAREN { $2 }
  | type_var { [$1] }
  | /* empty */ { [] }

type_var_list:
  | type_var COMMA type_var_list { $1::$3}
  | type_var { [$1] }

type_var:
  | QUOTE IDENT { $2 }

type_def:
  | /* empty */ { Ptd_abstract }
  | EQUAL opt_bar constr_decl_list { Ptd_variant $3 }

constr_decl_list:
  | constr_decl BAR constr_decl_list { $1::$3 }
  | constr_decl { [$1] }

constr_decl:
  | IDENT OF type_ { $1, Some $3 }
  | IDENT { $1, None }

/* expression */

opt_bar:
  | /* empty */ { () }
  | BAR { () }

rec_flag:
  | /* empty */ { false }
  | REC { true }

mutable_flag:
  | /* empty */ { false }
  | MUTABLE { true }

seq_expr:
  | expr SEMI seq_expr { make_expr(Pexpr_sequence($1, $3)) }
  | expr SEMI { $1 }
  | expr %prec below_SEMI { $1 }

expr:
  | simple_expr { $1 }
  | simple_expr simple_expr_list { make_expr(Pexpr_apply($1, $2)) }
  | expr_comma_list %prec below_COMMA { make_expr(Pexpr_tuple(List.rev $1)) }
  | NOT expr { make_unop "not" $2 }
  /*| simple_expr expr LESSMINUS expr { make_expr (Pexpr_assign($1, $3)) }*/
  | expr INFIX4 expr { make_binop $2 $1 $3 }
  | expr INFIX3 expr { make_binop $2 $1 $3 }
  | expr STAR expr { make_binop "*" $1 $3 }
  | expr COLONCOLON expr {
      make_expr(Pexpr_constr(constr_cons,
        Some(make_expr(Pexpr_tuple [$1; $3])))) }
  | expr INFIX2 expr { make_binop $2 $1 $3 }
  | expr INFIX1 expr { make_binop $2 $1 $3 }
  | expr INFIX0 expr { make_binop $2 $1 $3 }
  | expr EQUAL expr { make_binop "=" $1 $3 }
  | expr AMPERSAND expr { make_binop "&" $1 $3 }
  | expr AMPERAMPER expr { make_binop "&&" $1 $3 }
  | expr OR expr { make_binop "or" $1 $3 }
  | expr BARBAR expr { make_binop "||" $1 $3 }
  | IF expr THEN expr ELSE expr { make_expr(Pexpr_if($2, $4, Some $6)) }
  | IF expr THEN expr { make_expr(Pexpr_if($2, $4, None)) }
  | LET rec_flag let_binding_list IN seq_expr { make_expr(Pexpr_let($2, $3, $5)) }
  | FUN fun_case { make_expr(Pexpr_function $2) }
  | FUNCTION opt_bar match1_case_list {
      make_expr (Pexpr_function (
        List.map (fun (p,e) -> [p],e) $3)) }
  | MATCH expr WITH opt_bar match1_case_list {
      make_expr (Pexpr_apply(
        make_expr (Pexpr_function (
          List.map (fun (p,e) -> [p],e) $5)), [$2])) }

simple_expr:
  | INT { make_expr(Pexpr_constant(Const_int $1)) }
  | FLOAT { make_expr(Pexpr_constant(Const_float $1)) }
  | STRING { make_expr(Pexpr_constant(Const_string $1)) }
  | IDENT { make_expr(Pexpr_ident(Lident $1)) }
  | LPAREN expr RPAREN { $2 }
  | LPAREN expr COLON type_ RPAREN { make_expr(Pexpr_constraint($2, $4)) }
  | LPAREN RPAREN { make_expr(Pexpr_constr(constr_void, None)) }
  | LBRACKET expr_semi_list RBRACKET { make_expr_list($2) }
  | LBRACKETBAR expr_semi_list BARRBRACKET { make_expr(Pexpr_array($2)) }
  | BEGIN expr END { $2 }

  | error
      { failwith
        (Printf.sprintf "characters %d %d"
          (Parsing.symbol_start ())
          (Parsing.symbol_end ())) }

simple_expr_list:
  | simple_expr simple_expr_list { $1::$2 }
  | simple_expr { [$1] }

expr_comma_list: /* reversed */
  | expr_comma_list COMMA expr { $3 :: $1 }
  | expr COMMA expr { [$3; $1] }

expr_semi_list:
  | expr SEMI expr_semi_list { $1 :: $3 }
  | expr { [$1] }
  | /* empty */ { [] }

/* let */

let_binding_list:
  | let_binding { [$1] }
  | let_binding AND let_binding_list { $1::$3 }

let_binding:
  | pattern EQUAL seq_expr { $1, $3 }
  | IDENT simple_pattern_list EQUAL seq_expr { make_pat(Ppat_var $1), make_expr(Pexpr_function [$2, $4]) }

/* pattern */

pattern:
  | simple_pattern { $1 }
  | pattern AS IDENT { make_pat(Ppat_alias($1, $3)) }
  | pattern COLONCOLON pattern {
      make_pat(Ppat_constr(constr_cons,
        Some(make_pat(Ppat_tuple [$1; $3])))) }
  | pattern_comma_list %prec below_COMMA { make_pat(Ppat_tuple(List.rev $1)) }
  | pattern BAR pattern { make_pat(Ppat_or($1, $3)) }

simple_pattern:
  | INT { make_pat(Ppat_constant(Const_int $1)) }
  | FLOAT { make_pat(Ppat_constant(Const_float $1)) }
  | UNDERSCORE { make_pat(Ppat_any) }
  | IDENT { make_pat(Ppat_var($1)) }
  | LPAREN pattern RPAREN { $2 }
  | LPAREN pattern COLON type_ RPAREN { make_pat(Ppat_constraint($2, $4)) }
  | LPAREN RPAREN { make_pat(Ppat_constr(constr_void, None)) }
  | LBRACKET pattern_semi_list RBRACKET { make_pat_list($2) }
  | LBRACKETBAR pattern_semi_list BARRBRACKET { make_pat(Ppat_array($2)) }

simple_pattern_list:
  | simple_pattern simple_pattern_list { $1::$2 }
  | simple_pattern { [$1] }

pattern_comma_list:
  | pattern_comma_list COMMA pattern { $3::$1 }
  | pattern COMMA pattern { [$3; $1] }

pattern_semi_list:
  | pattern SEMI pattern_semi_list { $1 :: $3 }
  | pattern { [$1] }
  | /* empty */ { [] }

action:
  | MINUSGREATER seq_expr { $2 }

fun_case:
  | simple_pattern_list action { [$1, $2] }

match1_case_list:
  | pattern action %prec BAR { [$1, $2] }
  | pattern action BAR match1_case_list { ($1, $2)::$4 }

match_case_list:
  | simple_pattern_list action %prec BAR { [$1, $2] }
  | simple_pattern_list action BAR match_case_list { ($1, $2)::$4 }
