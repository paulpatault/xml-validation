open Tdef
open Utils

(* remplissage de la table de hashage des types *)
let mk_tbl ?(debug = false) (typ : DTD.t) =
  let tbl = Hashtbl.create 42 in

  (* if debug then Format.fprintf fmt "regexps::=@[<v>@,"; *)
  List.iter
    (fun (ident, guard, regex) ->
      let lab = match guard with DTD.Label [ l ] -> l | _ -> "empty" in
      if debug then print_dtd_def Format.err_formatter ident lab regex;

      let guard', regex' =
        match Hashtbl.find_opt tbl ident with
        | None -> (guard, regex)
        | Some (guard', regex') -> (
            let open DTD in
            match (guard, guard') with
            | Label l, Label l' -> (Label (l @ l'), Alt (regex, regex'))
            | _ -> failwith "todo")
      in

      Hashtbl.add tbl ident (guard', regex'))
    typ;
  if debug then Format.eprintf "@]@.";
  tbl

(* compilation du fichier dtd vers un automate d'arbre *)
let compile_typ ?(debug = false) (rac : string) (typ : DTD.t) :
    AutomT.t * string =
  let table = mk_tbl ~debug typ in

  let automata = Tree_automata.empty () in
  let rac_res = ref "" in

  Hashtbl.iter
    (fun ident (guard, regex) ->
      if debug then Format.eprintf "GLoop::::%s@." ident;
      match guard with
      | Tdef.DTD.Label [ l ] -> (
        if debug then Format.eprintf "\t::::%s@." l;
          if ident = rac then rac_res := l;
          Tree_automata.extends_sigma automata l;

          match Regautom.make_dfa regex with
          | Some dfa ->
              if ident = rac then
                Tree_automata.extends_delta automata
                  (AutomT.Transition.F
                     ( Alphabet.singleton l,
                       Some (mk_state_str Format.pp_print_string ident),
                       Some
                         (mk_state_str Tree_automata.Pprinter.pp_alphabet
                            AutomS.(dfa.start)),
                       None ));

              AutomS.DeltaMap.iter
                (fun state nexts ->
                  if debug then Format.eprintf "\t::::--iter@.";
                  (* iteration sur les transitions *)
                  AutomS.TransMap.iter
                    (fun _ value ->
                      if debug then Format.eprintf "\t::::----subiter@.";
                      let state =
                        mk_state_str Tree_automata.Pprinter.pp_alphabet state
                      in
                      let value =
                        mk_state_str Tree_automata.Pprinter.pp_alphabet value
                      in

                      let sibling = if ident = rac then None else
                        (if debug then Format.eprintf "val::(%s)@." value; Some value) in

                      let child =
                        if regex = Tdef.DTD.Epsilon then None
                        else Some (mk_state_str Format.pp_print_string ident)
                      in

                      let cur = Some state in

                      let trans =
                        AutomT.Transition.F
                          (Alphabet.singleton l, cur, child, sibling)
                      in

                      Tree_automata.extends_delta automata trans)
                    nexts)
                AutomS.(dfa.trans)
          | None -> if debug then Format.eprintf "compiler.match : NONE...@.")
      | _ -> failwith "TODO")
    table;

  (automata, !rac_res)
