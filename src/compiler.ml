open Tdef

let mkhsh (typ : DTD.t) =
  let h = Hashtbl.create 42 in

  List.iter
    (fun (ident, guard, regex) ->
      let lst =
        match Hashtbl.find_opt h ident with None -> [] | Some e -> e
      in
      Hashtbl.add h ident ((guard, regex) :: lst))
    typ;
  h

let compile_typ (typ : DTD.t) (entry : Ident.t) : AutomT.t =
  let table = mkhsh typ in

  let automata = Tree_automata.empty () in

  Tree_automata.(extends_states automata (None, entry));
  Hashtbl.iter
    (fun _ident values ->
      List.iter
        (fun (guard, regex) ->
          match guard with
          | Tdef.DTD.Label l ->
              Tree_automata.extends_sigma automata l;

              let reg = Regautom.make_dfa regex in
              let sibling = (reg, "state todo") in
              let child = (reg, "state todo") in
              let cur_state = (None, "state todo") in

              (* let child = Tree_automata.State.Bottom in *)
              let trans =
                AutomT.Transition.F
                  (Alphabet.singleton l, cur_state, child, sibling)
              in

              Tree_automata.extends_delta automata trans
          | _ -> failwith "pas encore codé :("
          (* | Tdef.Neg ll ->
                 List.iter (fun e -> Automata.extends automata (`Sigma e)) ll;

                 let l,ll = match ll with | a::b -> a,b | _ -> assert false in

                 let trans = Automata.
                   (Transition.CoF (
                     Alphabet.of_list ll,
                     State.Q (String.concat "" ll),
                     State.Q l,
                     State.Next l
                 )) in

                 Automata.extends automata (`Trans trans)

             | Tdef.Star -> () *))
        values)
    table;

  automata
