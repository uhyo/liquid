open KNormal
open Constant


(* Dependent Type for built-in functions. *)
let lbint = LType.Base(BType.Int, LType.RExp([]))
let lbbool= LType.Base(BType.Bool, LType.RExp([]))
let lbintarray = LType.Base(BType.IntArray, LType.RExp([]))
let dtypes = M.of_list
                  [
                    (iff, LType.Fun((lbint, "x"),
                          LType.Fun((lbint, "y"),
                          LType.Base(BType.Bool, LType.RExp([
                            App(App(Var(iff), Var(nu)),
                                App(App(Var(iff),Var("x")),
                                    Var("y")))])))));
                    (lt, LType.Fun((lbint, "x"),
                         LType.Fun((lbint, "y"),
                         LType.Base(BType.Bool, LType.RExp([
                           App(App(Var(iff), Var(nu)),
                               App(App(Var(lt),Var("x")),
                                   Var("y")))])))));
                    (gt, LType.Fun((lbint, "x"),
                         LType.Fun((lbint, "y"),
                         LType.Base(BType.Bool, LType.RExp([
                           App(App(Var(iff), Var(nu)),
                               App(App(Var(gt),Var("x")),
                                   Var("y")))])))));
                    (le, LType.Fun((lbint, "x"),
                         LType.Fun((lbint, "y"),
                         LType.Base(BType.Bool, LType.RExp([
                           App(App(Var(iff), Var(nu)),
                               App(App(Var(le),Var("x")),
                                   Var("y")))])))));
                    (ge, LType.Fun((lbint, "x"),
                         LType.Fun((lbint, "y"),
                         LType.Base(BType.Bool, LType.RExp([
                           App(App(Var(iff), Var(nu)),
                               App(App(Var(ge),Var("x")),
                                   Var("y")))])))));
                    (eq, LType.Fun((lbint, "x"),
                         LType.Fun((lbint, "y"),
                         LType.Base(BType.Bool, LType.RExp([
                           App(App(Var(iff), Var(nu)),
                               App(App(Var(eq),Var("x")),
                                   Var("y")))])))));
                    (not, LType.Fun((lbbool, "x"),
                          LType.Base(BType.Bool, LType.RExp([
                            App(App(Var(iff), Var(nu)),
                                App(Var(not), Var("x")))]))));
                    (add, LType.Fun((lbint, "x"),
                          LType.Fun((lbint, "y"),
                          LType.Base(BType.Int, LType.RExp([
                            App(App(Var(eq), Var(nu)),
                                App(App(Var(add),Var("x")),
                                    Var("y")))])))));
                    (sub, LType.Fun((lbint, "x"),
                          LType.Fun((lbint, "y"),
                          LType.Base(BType.Int, LType.RExp([
                            App(App(Var(eq), Var(nu)),
                                App(App(Var(add),Var("x")),
                                    Var("y")))])))));
                    (len, LType.Fun((lbintarray, "a"),
                          LType.Base(BType.Int, LType.RExp([
                            App(App(Var(eq), Var(nu)),
                                App(Var(len), Var("a")))]))));
                    (arrayget, LType.Fun((lbintarray, "a"),
                               LType.Fun((LType.Base(BType.Int,
                                                     LType.RExp([
                                                       App(App(Var(le), Int(0)), Var(nu));
                                                       App(App(Var(lt), Var(nu)), App(Var(len), Var("a")))])), "x"),
                               lbint)));
                    (mkarray, LType.Fun((lbint, "n"),
                              LType.Fun((LType.Fun((LType.Base(BType.Int, LType.RExp([
                                App(App(Var(le), Int(0)), Var(nu));
                                App(App(Var(lt), Var(nu)), Var("n"))])), "i"), lbint), "f"),
                              LType.Base(BType.IntArray,
                                         LType.RExp([
                                           App(App(Var(eq), Var("n")), App(Var(len), Var(nu)))])))));
                    (arrayeq, LType.Fun((lbintarray, "x"),
                              LType.Fun((lbintarray, "y"),
                              LType.Base(BType.Bool, LType.RExp([
                                App(App(Var(iff), Var(nu)),
                                    App(App(Var(arrayeq),Var("x")),
                                        Var("y")))])))));
                    (*
                    (max, LType.Fun((lbint, "x"),
                          LType.Fun((lbint, "y"),
                          LType.Base(BType.Int, LType.RExp([
                            App(App(Var(le), Var("x")), Var(nu));
                            App(App(Var(le), Var("y")), Var(nu));
                          ])))));
                    (foldn, LType.Fun((lbint, "n"),
                            LType.Fun((lbint, "b"),
                            LType.Fun((LType.Fun((LType.Base(BType.Int, LType.RExp([
                                              App(App(Var(le), Int(0)), Var(nu));
                                              App(App(Var(lt), Var(nu)), Var("n"));
                                            ])), "x"),
                                       LType.Fun((lbint, "y"),
                                       lbint)), "f"),
                            lbint))));
                     *)
                  ]
