
let compile_typ (typ: Tdef.t) (entry : Tdef.ident) : Automata.t =

  let automata = Automata.mk_automata () in

  Automata.(extends automata (`States (State.Root entry)));

  List.iter (fun (_ident, guard, _regex) ->

    match guard with
    | Tdef.Label l -> Automata.extends automata (`Sigma l)
    | Tdef.Neg ll  -> List.iter (fun e -> Automata.extends automata (`Sigma e)) ll
    | Tdef.Star    -> ()

  ) typ;

  assert false
