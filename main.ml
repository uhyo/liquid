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
  Printf.printf "Cons\n%s\n\n" (LType.type_str t);
  List.iter
    (fun c -> Printf.printf "%s\n" (Cons.cons_str c))
    cs;
  (* Constants を Split *)
  let cs' = Cons.split cs in
    Printf.printf "Cons (Split)\n\n";
    List.iter
      (fun c -> Printf.printf "%s\n" (Cons.cons_str c))
      cs';
  (* とりあえずQを表示 *)
  Printf.printf "Q:\n";
  List.iter
    (fun e -> Printf.printf "%s\n" (KNormal.short_str e))
    Solve.default_q;
  (* SoLoveる *)
  let a = Solve.f cs' Solve.default_q exp3 in
  (* assignmentを表示 *)
  Printf.printf "----------\nAssigmment:\n";
  MI.iter
    (fun i qs ->
       Printf.printf "κ%d: " i;
       if List.length qs = 0 then Printf.printf "true";
       List.iter
         (fun q -> Printf.printf "%s " (KNormal.short_str q))
         qs;
       Printf.printf "\n")
    a;
  Printf.printf "----------\n";
  (* bindingも表示 *)
  Printf.printf "Bindings:\n";
  M.iter
    (fun x t ->
       let t' = Solve.apply_asgn_lt Builtin.dtypes a t in
       (*Printf.printf "\027[96m%s\027[39m: \027[93m%s\027[39m\n" x (LType.type_str t);*)
       Printf.printf "\027[96m%s\027[39m: \027[93m%s\027[39m\n" x (LType.type_str t'))
    !Cons.allenv;
  Printf.printf "----------\n";
  let t' = Solve.apply_asgn_lt M.empty a t in
  Printf.printf "%s\n" (LType.type_str t');
    ()


