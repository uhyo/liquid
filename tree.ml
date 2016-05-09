(* e: ツリー表示したい項
 * f: ツリーのラベルと子を返す関数
 * di: デフォルトのインデント *)
let make (f: 'a -> string * 'a list) (di: string) (e: 'a) =
  let result = Buffer.create 100 in
  let rec indent a e =
    (* ラベルを書く *)
    let (lbl, nodes) = f e in
      Buffer.add_string result a; (* indent *)
      Buffer.add_string result lbl; (* ラベル *)
      Buffer.add_string result " # "; (* 位置情報 *)
      Buffer.add_char result '\n';
    (* 'a listの内容を順に処理 *)
      let a' = a ^ "  " in
        List.iter (fun n -> indent a' n) nodes
  in
    indent di e;
    Buffer.contents result

