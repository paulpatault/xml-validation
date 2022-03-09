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

let compile_typ (typ : DTD.t) : AutomT.t =
  let table = mkhsh typ in

  let automata = Tree_automata.empty () in

  Hashtbl.iter
    (fun ident values ->
      List.iter
        (fun (guard, regex) ->
          match guard with
          | Tdef.DTD.Label l ->
              Tree_automata.extends_sigma automata l;

              let dfa = Regautom.make_dfa regex in
              let sibling = (dfa, ident ^ l) in
              let child = (dfa, "c: " ^ ident ^ l) in
              let cur_state = (dfa, ident ^ l) in

              let trans =
                AutomT.Transition.F
                  (Alphabet.singleton l, cur_state, child, sibling)
              in

              Tree_automata.extends_delta automata trans
          | _ -> failwith "TODO")
        values)
    table;

  automata
