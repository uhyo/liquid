%{

open Syntax

%}

%token <bool> BOOL

%token LPAREN
%token RPAREN
%token EOF

%type <Syntax.t> exp

%start exp

%%

exp:
| BOOL
    { Bool $1 }

