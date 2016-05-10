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
  let (exp2, topBTyp) = BTyping.f exp in
  Printf.printf "BTyping\n%s\n" (Syntax.tree BType.type_str exp2);
  (* K-Normalize *)
  let exp3 = KNormal.f exp2 in
  Printf.printf "KNormal\n%s\n" ((KNormal.tree BType.type_str) exp3);
  (* Constrains 生成 *)
  let (t, cs) = Cons.f exp3 in
    Printf.printf "Cons\n%s\n" (LType.type_str t);
    List.iter
      (fun c -> Printf.printf "%s\n" (Cons.cons_str c))
      cs;
  (* Constants を Split *)
    let cs' = Cons.split cs in
    Printf.printf "Cons (Split)\n";
    List.iter
      (fun c -> Printf.printf "%s\n" (Cons.cons_str c))
      cs';

