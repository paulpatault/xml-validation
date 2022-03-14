open Tdef

(* variable de debuging *)
let i = ref 0
let sp () = Format.eprintf "\n----------------\n----------------\n"

let rec validate_td ?(debug = false) autom tree rac state =
  if debug then (
    incr i;
    sp ();
    Format.eprintf "[ %d ]@." !i;
    Utils.print_tree tree;
    Format.eprintf "rac::%a\nstr::%s\n@." Tree_automata.Pprinter.pp_alphabet rac
      state);
  if Tree.is_empty tree then true (* is_final state*)
  else
    let trans =
      AutomT.Delta.filter
        (fun x ->
          match x with
          | F (rac', Some str, _, _) when rac' = rac && state = str ->
              if debug then
                Format.eprintf "trans OK:     %a@."
                  Tree_automata.Pprinter.pp_trans x;
              true
          | _ ->
              if debug then
                Format.eprintf "trans NOTOK : %a@."
                  Tree_automata.Pprinter.pp_trans x;
              false)
        AutomT.(autom.delta)
    in
    if debug then
      Format.eprintf "\nTaille liste : [%d]@." (AutomT.Delta.cardinal trans);
    AutomT.Delta.fold
      (fun q' acc ->
        let b_child, b_sibling =
          match q' with
          | F (_, Some _, x, y) -> (x, y)
          | _ ->
              if debug then Format.eprintf "match dans tc.ml@.";
              assert false
        in

        let child_validation =
          if Option.is_none b_child then false
          else
            let child = Tree.first_child tree in
            let rac_child =
              Utils.mk_state_str Format.pp_print_string (Tree.label child)
            in
            let rac_child = Alphabet.singleton rac_child in
            validate_td ~debug autom child rac_child (Option.get b_child)
        in

        let sibling_validation =
          if Option.is_none b_sibling then false
          else
            let sibling = Tree.next_sibling tree in
            let rac_sibling =
              Utils.mk_state_str Format.pp_print_string (Tree.label sibling)
            in
            let rac_sibling = Alphabet.singleton rac_sibling in
            validate_td ~debug autom sibling rac_sibling (Option.get b_sibling)
        in
        acc || (child_validation && sibling_validation))
      trans false

let check ?(debug = false) (autom : Tdef.AutomT.t) (tree : Tree.t) rac lrac =
  if debug then (
    Utils.print_tree tree;
    Utils.print_autom autom);

  validate_td ~debug autom tree (Alphabet.singleton rac)
    (Utils.mk_state_str Format.pp_print_string lrac)
