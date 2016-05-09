%{

open Syntax

%}

%token <bool> BOOL
%token <int> INT
%token <string> IDENT
%token NOT
%token LT
%token LE
%token GT
%token GE
%token EQ
%token FUN
%token RARROW
%token IF
%token THEN
%token ELSE
%token LET
%token IN
%token REC

%token LPAREN
%token RPAREN
%token EOF

%right prec_let
%right prec_if
%right prec_fun
%left LT GT LE GE
%left prec_app

%type <BType.t Syntax.t> exp

%start exp

%%

atomic_exp:
| LPAREN exp RPAREN
    { $2 }
| BOOL
    { Bool $1 }
| INT
    { Int $1 }
| IDENT
    { Var $1 }

exp:
| atomic_exp
    { $1 }
| FUN IDENT RARROW exp
    %prec prec_fun
    { Lambda((BType.newtype(), $2), $4) }
| exp atomic_exp
    %prec prec_app
    { App($1, $2) }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { If($2, $4, $6) }
| LET IDENT EQ exp IN exp
    %prec prec_let
    { Let((BType.newtype(), $2), $4, $6) }
| LET REC IDENT IDENT EQ exp IN exp
    %prec prec_let
    { LetRec((BType.newtype(), $3), ((BType.newtype(), $4), $6), $8) }
| NOT exp
    { Not $2 }
| exp LT exp
    { Lt($1, $3) }
| exp GT exp
    { Gt($1, $3) }
| exp LE exp
    { Le($1, $3) }
| exp GE exp
    { Ge($1, $3) }
| error
    { failwith
        (let {
                Lexing.pos_lnum = s_lnum;
                Lexing.pos_cnum = s_cnum;
                Lexing.pos_bol = s_bol
             } = Parsing.symbol_start_pos () in
        let {
                Lexing.pos_lnum = e_lnum;
                Lexing.pos_cnum = e_cnum;
                Lexing.pos_bol = e_bol
             } = Parsing.symbol_end_pos () in
        (Printf.sprintf "parse error near (%d, %d)-(%d, %d)"
           s_lnum (s_cnum - s_bol)
           e_lnum (e_cnum - e_bol)
           )) }
