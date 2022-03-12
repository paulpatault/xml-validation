open Tdef

(* utile pour debug *)
let rec r2str r =
  let open DTD in
  let open Format in
  match r with
  | Epsilon -> "eps"
  | Ident x -> x
  | Concat (r1, r2) -> sprintf "(%s)+(%s)" (r2str r1) (r2str r2)
  | Star r -> sprintf "(%s)*" (r2str r)
  | Alt (r1, r2) -> sprintf "(%s)|(%s)" (r2str r1) (r2str r2)

(* remplissage de la table de hashage des types *)
let mkhsh (typ : DTD.t) =
  let h = Hashtbl.create 42 in

  List.iter
    (fun (ident, guard, regex) ->
      let lab = match guard with Tdef.DTD.Label l -> l | _ -> "empty" in

      Format.eprintf "%s,%s,%s@." ident lab (r2str regex);

      let lst =
        match Hashtbl.find_opt h ident with None -> [] | Some e -> e
      in
      Hashtbl.add h ident ((guard, regex) :: lst))
    typ;
  h

(* compilation du fichier dtd vers un automate d'arbre *)
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
              let sibling = (dfa, "q_" ^ l) in
              let child = (dfa, "q_" ^ l) in
              let cur_state = (dfa, "q_" ^ l) in

              let trans =
                AutomT.Transition.F
                  (Alphabet.singleton ident, cur_state, child, sibling)
              in

              Tree_automata.extends_delta automata trans
          | _ -> failwith "TODO")
        values)
    table;

  automata
