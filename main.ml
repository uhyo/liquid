let () =
  (* ファイル名の入力 *)
  let file = ref "" in
  Arg.parse [] (fun s -> file := s) "foo";
  let inchan = open_in (!file) in
  let exp = (try
               let t = (Parser.exp Lexer.token (Lexing.from_channel inchan)) in
                 close_in inchan;
                 t
             with e -> close_in inchan; raise e) in
    Printf.printf "%s\n" (Syntax.tree exp)


