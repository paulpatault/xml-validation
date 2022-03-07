
let compile_typ (typ: Tdef.t) (entry : Tdef.ident) : Automata.t =

  let automata = Automata.mk_automata () in

  Automata.(extends automata (`States (State.Root entry)));

  List.iter (fun (_ident, guard, _regex) ->

    match guard with

    | Tdef.Label l ->
        Automata.extends automata (`Sigma l);

        let trans =
          Automata.(Transition.F (
            Alphabet.singleton l,
            State.Q l,
            State.Q l, (* TODO ICI NOT OK*)
            State.Next l
        ))
        in
        Automata.extends automata (`Trans trans)

    | Tdef.Neg ll ->
        List.iter (fun e -> Automata.extends automata (`Sigma e)) ll;

        let trans = Automata.
          (Transition.CoF (
            Alphabet.of_list ll,
            State.Q (String.concat "" ll),
            State.Q l, (* TODO ICI NOT OK*)
            State.Next l
        )) in

        Automata.extends automata (`Trans trans)

    | Tdef.Star -> ()

  ) typ;

  let fmt = Format.std_formatter in
  Format.fprintf fmt "%a" Automata.Pprinter.pp_autom automata;

  assert false
