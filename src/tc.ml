open Tdef

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
  let open AutomT in
  if Tree.is_empty tree then true (* is_final state*)
  else
    let trans =
      Delta.filter
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
        autom.delta
    in
    if debug then
      Format.eprintf "\nTaille liste : [%d]@." (Delta.cardinal trans);
    Delta.fold
      (fun q' acc ->
        match q' with
        | F (_, Some _, Some state_child, Some state_sibling) ->
            let child = Tree.first_child tree in
            let sibling = Tree.next_sibling tree in
            let rac_child =
              Utils.mk_state_str Format.pp_print_string (Tree.label child)
            in
            let rac_child = Alphabet.singleton rac_child in
            let rac_sibling =
              Utils.mk_state_str Format.pp_print_string (Tree.label sibling)
            in
            let rac_sibling = Alphabet.singleton rac_sibling in
            acc
            || validate_td ~debug autom child rac_child state_child
               && validate_td ~debug autom sibling rac_sibling state_sibling
        | F (_, Some _, Some state_child, None) ->
            let child = Tree.first_child tree in
            let rac_child =
              Utils.mk_state_str Format.pp_print_string (Tree.label child)
            in
            let rac_child = Alphabet.singleton rac_child in
            acc || validate_td ~debug autom child rac_child state_child
        | F (_, Some _, None, Some state_sibling) ->
            let sibling = Tree.next_sibling tree in
            let rac_sibling =
              Utils.mk_state_str Format.pp_print_string (Tree.label sibling)
            in
            let rac_sibling = Alphabet.singleton rac_sibling in
            acc || validate_td ~debug autom sibling rac_sibling state_sibling
        | _ ->
            if debug then Format.eprintf "match dans tc.ml@.";
            false)
      trans false

let check ?(debug = false) (autom : Tdef.AutomT.t) (tree : Tree.t) rac lrac =
  if debug then (
    Utils.print_tree tree;
    Utils.print_autom autom);

  validate_td ~debug autom tree (Alphabet.singleton rac)
    (Utils.mk_state_str Format.pp_print_string lrac)
