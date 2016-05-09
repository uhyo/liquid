open Z3
open Z3.Arithmetic
open Z3.Boolean
open Z3.Tactic
open Z3.Tactic.ApplyResult

let _ = 
  let cfg = [("model", "true"); ("proof", "false")] in
  let ctx = Z3.mk_context cfg in
  let x = Real.mk_const_s ctx "x" in
  let g = Goal.mk_goal ctx true false false in
    Goal.add g [ (mk_gt ctx x (Real.mk_numeral_i ctx 10)) ];
    Goal.add g [ (mk_eq ctx x (Real.mk_numeral_i ctx 5)) ];
  let ar = Tactic.apply (and_then ctx (mk_tactic ctx "simplify") (mk_tactic ctx "solve-eqs") []) g None in
    Printf.printf "%d %s\n" (get_num_subgoals ar) (Tactic.ApplyResult.to_string ar);
  let g2 = get_subgoal ar 0 in
    Printf.printf "bar! %s %s %s\n" (Goal.to_string g2) (string_of_bool (Goal.is_decided_sat g2)) (string_of_bool (Goal.is_decided_unsat g2));
