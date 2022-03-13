open Tdef

(* remplissage de la table de hashage des types *)
let mk_tbl ?(debug = false) (typ : DTD.t) =
  let tbl = Hashtbl.create 42 in

  let fmt = Format.err_formatter in
  (* if debug then Format.fprintf fmt "regexps::=@[<v>@,"; *)
  List.iter
    (fun (ident, guard, regex) ->
      let lab = match guard with Tdef.DTD.Label [ l ] -> l | _ -> "empty" in
      if debug then Utils.print_dtd_def fmt ident lab regex;

      let guard', regex' =
        match Hashtbl.find_opt tbl ident with
        | None -> (guard, regex)
        | Some (guard', regex') -> (
            match (guard, guard') with
            | Tdef.DTD.Label l, Tdef.DTD.Label l' ->
                (Tdef.DTD.Label (l @ l'), Tdef.DTD.Alt (regex, regex'))
            | _ -> failwith "todo")
      in

      Hashtbl.add tbl ident (guard', regex'))
    typ;
  if debug then Format.fprintf fmt "@]@.";
  tbl

(* compilation du fichier dtd vers un automate d'arbre *)
let compile_typ ?(debug = false) (rac : string) (typ : DTD.t) : AutomT.t =
  let table = mk_tbl ~debug typ in

  let automata = Tree_automata.empty () in

  Hashtbl.iter
    (fun ident (guard, regex) ->
      match guard with
      | Tdef.DTD.Label [ l ] ->
          Tree_automata.extends_sigma automata l;

          let dfa = Regautom.make_dfa regex in

          let sibling = if ident = rac then None else Some ident in
          let child = if regex = Tdef.DTD.Epsilon then None else Some ident in

          let cur = Some ident in

          let trans =
            AutomT.Transition.F (Alphabet.singleton l, cur, child, sibling)
          in

          Tree_automata.extends_delta automata trans
      | _ -> failwith "TODO")
    table;

  automata
