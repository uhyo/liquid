(* e: ツリー表示したい項
 * f: ツリーのラベルと兄弟、子を返す関数
 * di: デフォルトのインデント *)
let make (f: 'a -> string * 'a list * 'a list) (di: string) (e: 'a) =
  let result = Buffer.create 100 in
  let rec indent a e =
    (* ラベルを書く *)
    let (lbl, nexts, children) = f e in
      Buffer.add_string result a; (* indent *)
      Buffer.add_string result lbl; (* ラベル *)
      Buffer.add_char result '\n';
      (* 子を処理 *)
      let a' = a ^ "  " in
        List.iter (fun n -> indent a' n) children;
      (* 兄弟を処理 *)
      List.iter (fun n -> indent a n) nexts;
  in
    indent di e;
    Buffer.contents result

