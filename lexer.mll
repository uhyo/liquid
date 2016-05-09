{
open Parser
}

let space = [' ' '\t']


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
| eof
    { EOF }
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
