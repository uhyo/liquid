type token =
  | BOOL of (bool)
  | INT of (int)
  | IDENT of (string)
  | NOT
  | LT
  | LE
  | GT
  | GE
  | EQ
  | FUN
  | RARROW
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | REC
  | LPAREN
  | RPAREN
  | EOF

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> BType.t Syntax.t
