{
open Parser
}

let space = [' ' '\t']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']


rule token = parse
| space+
    { token lexbuf }
| ("\r\n" | "\r" | "\n")
    { Lexing.new_line lexbuf;
      token lexbuf }
| "(*"
    { comment lexbuf;
      token lexbuf }

| '('
    { LPAREN }
| ')'
    { RPAREN }
| "true"
    { BOOL true }
| "false"
    { BOOL false }
| digit+
    { INT (int_of_string (Lexing.lexeme lexbuf)) }
| "fun"
    { FUN }
| "->"
    { RARROW }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "let"
    { LET }
| '='
    { EQ }
| "in"
    { IN }
| "REC"
    { REC }
| "not"
    { NOT }
| '<'
    { LT }
| "<="
    { LE }
| '>'
    { GT }
| ">="
    { GE }
| eof
    { EOF }
| lower (digit|lower|upper|'_')*
    { IDENT(Lexing.lexeme lexbuf) }
| _
    { failwith "unknown token!!!!!!!!!" }

and comment = parse
| ("\r\n" | "\r" | "\n")
    { Lexing.new_line lexbuf;
      comment lexbuf }
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Printf.eprintf "warning: unterminated comment/" }
| _
    { comment lexbuf }
