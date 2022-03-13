open Tdef

let rec validate_td ?(debug = true) autom tree rac state =
  let open AutomT in
  if Tree.is_empty tree then true (* is_final state*)
  else
    let f x =
      if debug then (Format.printf "trans:@.%a@." Tree_automata.Pprinter.pp_trans x;
      Format.printf "rac::%a, str::%s@." Tree_automata.Pprinter.pp_alphabet rac
        state);
      match x with
      | Transition.F (x, Some str, _, _) when x = rac && state = str -> true
      | _ -> false
    in
    let trans = Delta.filter f autom.delta in
    if debug then Format.printf "%d@." (Delta.cardinal trans);
    Delta.fold
      (fun q' acc ->
        let _curr, state_child, state_sibling =
          match q' with F (_, Some curr, Some c, Some s) -> curr, c,s | _ -> failwith "todo"
        in
        let rl = Obj.magic () in
        let rc = Obj.magic () in
        acc
        || validate_td autom (Tree.first_child tree) rl state_child
           && validate_td autom (Tree.next_sibling tree) rc state_sibling)
      trans false

let check ?(debug = false) (autom : Tdef.AutomT.t) tree rac =
  if debug then (
    Utils.print_tree tree;
    Utils.print_autom autom);

  validate_td ~debug autom tree (Alphabet.singleton rac) (Utils.mk_state_str Format.pp_print_string rac)
