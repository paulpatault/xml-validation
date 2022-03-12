open Tdef

(* remplissage de la table de hashage des types *)
let mkhsh ?(debug = false) (typ : DTD.t) =
  let h = Hashtbl.create 42 in

  let fmt = Format.err_formatter in
  if debug then Format.fprintf fmt "regexps::=@[<v>@,";
  List.iter
    (fun (ident, guard, regex) ->
      let lab = match guard with Tdef.DTD.Label l -> l | _ -> "empty" in
      if debug then Utils.print_dtd_def fmt ident lab regex;

      let lst =
        match Hashtbl.find_opt h ident with None -> [] | Some e -> e
      in
      Hashtbl.add h ident ((guard, regex) :: lst))
    typ;
  if debug then Format.fprintf fmt "@]@.";
  h

(* compilation du fichier dtd vers un automate d'arbre *)
let compile_typ ?(debug = false) (typ : DTD.t) : AutomT.t =
  let table = mkhsh ~debug typ in

  let automata = Tree_automata.empty () in

  Hashtbl.iter
    (fun ident values ->
      List.iter
        (fun (guard, regex) ->
          match guard with
          | Tdef.DTD.Label l ->
              Tree_automata.extends_sigma automata l;

              let dfa = Regautom.make_dfa regex in
              let sibling = (dfa, "q_from_" ^ ident) in
              let child = (dfa, "q_from_" ^ ident) in
              let cur_state = (dfa, "q_" ^ ident) in

              let trans =
                AutomT.Transition.F
                  (Alphabet.singleton l, cur_state, child, sibling)
              in

              Tree_automata.extends_delta automata trans
          | _ -> failwith "TODO")
        values)
    table;

  automata
