let () =
  (* ファイル名の入力 *)
  let file = ref "" in
  Arg.parse [] (fun s -> file := s) "foo";
  let inchan = open_in (!file) in
  (* 構文解析 *)
  let exp = (try
               let t = (Parser.exp Lexer.token (Lexing.from_channel inchan)) in
                 close_in inchan;
                 t
             with e -> close_in inchan; raise e) in
  (* Base Type 推論 *)
  let exp2 = BTyping.f exp in
  Printf.printf "%s\n" (Syntax.tree BType.type_str exp2);
  (* template化*)
  let exp3 = LTyping.template_term exp2 in
    Printf.printf "%s\n" (Syntax.tree LType.type_str exp3)

