(* Built-in something. *)
(* special variable. *)
let nu = "ν"
let star = "*"
(* Built-in functions. *)
let iff= "⇔"
let lt = "<"
let gt = ">"
let le = "<="
let ge = ">="
let not = "not"
let eq = "="
let add = "+"
let sub = "-"
let len = "len"
let arrayget = "arrayget"
let mkarray = "mkarray"
(* Equality of int array. ????? XXX *)
let arrayeq = "=[]="
(*
let max = "max"
let foldn = "foldn"
 *)

let rs x = ref (Some x)
(* BType for built-in functions. *)
let btypes = M.of_list
                  [
                    (lt, BType.Fun((BType.Int, rs "x"),
                         BType.Fun((BType.Int, rs "y"), BType.Bool)));
                    (gt, BType.Fun((BType.Int, rs "x"),
                         BType.Fun((BType.Int, rs "y"), BType.Bool)));
                    (le, BType.Fun((BType.Int, rs "x"),
                         BType.Fun((BType.Int, rs "y"), BType.Bool)));
                    (ge, BType.Fun((BType.Int, rs "x"),
                         BType.Fun((BType.Int, rs "y"), BType.Bool)));
                    (eq, BType.Fun((BType.Int, rs "x"),
                         BType.Fun((BType.Int, rs "y"), BType.Bool)));
                    (not, BType.Fun((BType.Bool, rs "x"), BType.Bool));
                    (add, BType.Fun((BType.Int, rs "x"),
                          BType.Fun((BType.Int, rs "y"), BType.Int)));
                    (sub, BType.Fun((BType.Int, rs "x"),
                          BType.Fun((BType.Int, rs "y"), BType.Int)));
                    (len, BType.Fun((BType.IntArray, rs "a"), BType.Int));
                    (arrayget, BType.Fun((BType.IntArray, rs "a"),
                               BType.Fun((BType.Int, rs "x"), BType.Int)));
                    (mkarray, BType.Fun((BType.Int, rs "n"),
                              BType.Fun((BType.Fun((BType.Int, rs "i"),
                                                   BType.Int),
                                         rs "f"), BType.IntArray)));
                    (arrayeq, BType.Fun((BType.IntArray, rs "x"),
                              BType.Fun((BType.IntArray, rs "y"), BType.Bool)));

                    (*
                    (max, BType.Fun((BType.Int, rs "x"),
                          BType.Fun((BType.Int, rs "y"), BType.Int)));
                    (foldn, BType.Fun((BType.Int, rs "n"),
                            BType.Fun((BType.Int, rs "b"),
                            BType.Fun((BType.Fun((BType.Int, rs "a"),
                                                 BType.Fun((BType.Int, rs "b"), BType.Int)), rs "f"),
                            BType.Int))))
                     *)
                  ]
