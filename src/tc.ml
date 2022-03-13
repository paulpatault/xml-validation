open Tdef

let rec validate_td autom tree rac state =
  let open AutomT in
  if Tree.is_empty tree then true (* is_final state*)
  else
    let f x =
      Format.printf "trans:@.%a@." Tree_automata.Pprinter.pp_trans x;
      Format.printf "rac::%a, str::%s@." Tree_automata.Pprinter.pp_alphabet rac
        state;
      match x with
      | Transition.F (x, Some str, _, _) when x = rac && state = str -> true
      | _ -> false
    in
    let trans = Delta.filter f autom.delta in
    Format.printf "%d@." (Delta.cardinal trans);
    Delta.fold
      (fun q' acc ->
        let _a =
          match q' with F (_, Some x, _, _) -> x | _ -> failwith "todo"
        in
        let rl = Obj.magic () in
        let rc = Obj.magic () in
        acc
        || validate_td autom (Tree.first_child tree) rl _a
           && validate_td autom (Tree.next_sibling tree) rc _a)
      trans false

let check ?(debug = false) (autom : Tdef.AutomT.t) tree rac =
  if debug then (
    Utils.print_tree tree;
    Utils.print_autom autom);

  validate_td autom tree (Alphabet.singleton rac) (Utils.mk_state_str Format.pp_print_string rac)
